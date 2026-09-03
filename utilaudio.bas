
' file type specific functions
' ______________________________________________________________________________'

' code by squall4226
' see https://www.freebasic.net/forum/viewtopic.php?p=149207&hilit=user+need+TALB+for+album#p149207
' april 2026 tweaked to deal with both 64 and 32 bit
Function getmp3tag(searchtag As String, fn As String) As String
   Dim As Long skip, offset
   Dim As LongInt count, maxcheck = 3000
   Dim As Long fnum
   Dim As UInteger tag_length
   Dim As String tagdata, searchSig, sig_str
   Dim As Long i
   Dim As Byte b1, b2, b3, b4, textEnc

   ' Tag normalization
   Select Case UCase(searchtag)
        Case "HEADER", "ID3"
            searchSig = "ID3" & Chr(&h03)
        Case "TITLE", "TIT2"
            searchSig = "TIT2"
        Case "ARTIST", "TPE1"
            searchSig = "TPE1"
        Case "ALBUM", "TALB"
            searchSig = "TALB"
        Case "COMMENT", "COMM"
            searchSig = "COMM"
        Case "COPYRIGHT", "TCOP"
            searchSig = "TCOP"
        Case "COMPOSER", "TCOM"
            searchSig = "TCOM"
        Case "BEATS PER MINUTE", "BPM", "TBPM"
            searchSig = "TBPM"
        Case "PUBLISHER", "TPUB"
            searchSig = "TPUB"
        Case "URL", "WXXX"
            searchSig = "WXXX"
        Case "PLAY COUNT", "PCNT"
            searchSig = "PCNT"
        Case "GENRE", "TCON"
            searchSig = "TCON"
        Case "ENCODER", "TENC"
            searchSig = "TENC"
        Case "TRACK", "TRACK NUMBER", "TRCK"
            searchSig = "TRCK"
        Case "YEAR", "TYER"
            searchSig = "TYER"      
        Case "PICTURE", "APIC"
            searchSig = "APIC"
        Case Else
            Return "----"
   End Select

   fnum = FreeFile
   Open fn For Binary Access Read As #fnum
   If Lof(fnum) < maxcheck Then maxcheck = Lof(fnum)
   
   For count = 1 To maxcheck - 10 Step 1
        ' Read 4 bytes for tag ID
        Get #fnum, count, b1
        Get #fnum, count + 1, b2
        Get #fnum, count + 2, b3
        Get #fnum, count + 3, b4
        
        ' Reconstruct as string
        sig_str = Chr(b1) & Chr(b2) & Chr(b3) & Chr(b4)
        
        If sig_str = Left(searchSig, 4) Then
             If searchSig = "ID3" & Chr(&h03) Then
                Close #fnum
                Return "1"
             EndIf
             
             ' Read syncsafe size (4 bytes at offset 4)
             Get #fnum, count + 4, b1
             Get #fnum, count + 5, b2
             Get #fnum, count + 6, b3
             Get #fnum, count + 7, b4
             
             tag_length = (CLng(b1) Shl 24) Or (CLng(b2) Shl 16) Or (CLng(b3) Shl 8) Or CLng(b4)
             
             ' Decode syncsafe integer
             tag_length = ((tag_length And &h7F000000) Shr 24) Or _
                         ((tag_length And &h007F0000) Shr 16) Or _
                         ((tag_length And &h00007F00) Shr 8) Or _
                         (tag_length And &h0000007F)
             
             If tag_length < 2 Then
                Close #fnum
                Return "ERROR"
             EndIf
             
             ' Frame data starts at count + 10
             Dim As Byte dataget(1 To tag_length)
             Get #fnum, count + 10, dataget()
             
             ' Get encoding byte (byte 1)
             textEnc = dataget(1)
             
             ' Handle different text encodings
             Select Case textEnc
                Case 0
                    ' ISO-8859-1 (single byte)
                    For i = 2 To tag_length
                        If dataget(i) = 0 Then Exit For
                        If dataget(i) >= 32 Then 
                            tagdata = tagdata & Chr(dataget(i))
                        EndIf
                    Next
                    
                Case 1
                    ' UTF-16 with BOM (2 bytes per character)
                    i = 2
                    ' Skip BOM if present (bytes -1, -2 = 0xFF, 0xFE in two's complement)
                    If dataget(2) = 255 And dataget(3) = 254 Then
                        i = 4
                    EndIf
                    
                    ' Read UTF-16LE (little-endian)
                    While i < tag_length
                        If dataget(i) = 0 And dataget(i + 1) = 0 Then Exit While
                        If dataget(i) >= 32 And dataget(i) <> 0 Then
                            tagdata = tagdata & Chr(dataget(i))
                        EndIf
                        i = i + 2
                    Wend
                    
                Case 2
                    ' UTF-16BE (big-endian)
                    For i = 2 To tag_length Step 2
                        If dataget(i) = 0 And dataget(i + 1) = 0 Then Exit For
                        If dataget(i + 1) >= 32 Then
                            tagdata = tagdata & Chr(dataget(i + 1))
                        EndIf
                    Next
                    
                Case 3
                    ' UTF-8
                    For i = 2 To tag_length
                        If dataget(i) = 0 Then Exit For
                        If dataget(i) >= 32 Then
                            tagdata = tagdata & Chr(dataget(i))
                        EndIf
                    Next
             End Select
             
             Exit For
        End If
   Next
   
   Close #fnum
   
   If Len(tagdata) = 0 Then
        tagdata = "----"
   EndIf

   Return tagdata

End Function

' attempt to extract and write cover art of mp3 to temp thumb file
Function getmp3cover(filename As String) As boolean
    Dim buffer  As String
    dim chunk   as string
    dim length  as string
    dim bend    as integer
    dim ext     as string = ""
    dim thumb   as string
    dim f       as long
    f = freefile
    ' remove old thumb if present
    delfile(exepath + pathchar + "thumb.jpg")
    delfile(exepath + pathchar + "thumb.png")
    Open filename For Binary Access Read As #f
        If LOF(f) > 0 Then
            buffer = String(LOF(f), 0)
            Get #f, , buffer
        End If
    Close #f
    if instr(1, buffer, "APIC") > 0 then
        length = mid(buffer, instr(buffer, "APIC") + 4, 4)
        ' ghetto check funky first 4 bytes signifying length image
        ' not sure how reliable this info is
        ' see comment codecaster https://stackoverflow.com/questions/47882569/id3v2-tag-issue-with-apic-in-c-net
        if val(asc(length, 1) & asc(length, 2)) = 0 then
            bend = (asc(length, 3) shl 8) or asc(length, 4)
        else
            bend = (asc(length, 1) shl 24 + asc(length, 2) shl 16 + asc(length, 3) shl 8 or asc(length, 4))
        end if
        if instr(1, buffer, "JFIF") > 0 then
            ' override end jpg if marker FFD9 is present
            if instr(buffer, CHR(&hFF, &hD9)) > 0 then
                bend = instr(1, mid(buffer, instr(1, buffer, "JFIF")), CHR(&hFF, &hD9)) + 7
            end if
            chunk = mid(buffer, instr(buffer, "JFIF") - 6, bend)
            ' thumbnail detection
            if instr(instr(1, buffer, "JFIF") + 4, buffer, "JFIF") > 0 then
                chunk = mid(buffer, instr(10, buffer, CHR(&hFF, &hD8)), instr(instr(buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9)) - (instr(10, buffer, CHR(&hFF, &hD8)) - 2))
                ' thumbnail in thumbnail edge case ffd8 ffd8 ffd9 ffd9 pattern in jpeg
                if instr(chunk, CHR(&hFF, &hD8, &hFF)) > 0 then
                    chunk = mid(buffer,_
                    instr(1,buffer, CHR(&hFF, &hD8)),_
                    instr(instr(instr(instr(1,buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9)) + 1, buffer, CHR(&hFF, &hD9))_
                    , buffer, CHR(&hFF, &hD9)) + 2 - instr(buffer, CHR(&hFF, &hD8)))
                end if
            end if
            ext = ".jpg"
        end if
        ' use ext and exif check to catch false png
        if instr(1, buffer, "‰PNG") > 0 and instr(1, buffer, "Exif") = 0 and ext = "" then
            ' override end png if tag is present
            if instr(1, buffer, "IEND") > 0 then
                bend = instr(1, mid(buffer, instr(1, buffer, "‰PNG")), "IEND") + 7
            end if
            chunk = mid(buffer, instr(buffer, "‰PNG"), bend)
            ext = ".png"
        end if
        ' funky variant for non jfif and jpegs video encoding?
        if (instr(1, buffer, "Lavc58") > 0 or instr(1, buffer, "Exif") > 0) and ext = "" then
            ' override end jpg if marker FFD9 is present
            if instr(buffer, CHR(&hFF, &hD9)) > 0 then
                bend = instr(1, mid(buffer, instr(1, buffer, "Exif")), CHR(&hFF, &hD9)) + 7
            end if
            if instr(1, buffer, "Exif") > 0 then
                chunk = mid(buffer, instr(buffer, "Exif") - 6, bend)
            else
                chunk = mid(buffer, instr(buffer, "Lavc58") - 6, bend)
            end if
            ext = ".jpg"
        end if
        ' last resort just check on begin and end marker very tricky...
        ' see https://stackoverflow.com/questions/4585527/detect-end-of-file-for-jpg-images#4614629
        if instr(buffer, CHR(&hFF, &hD8)) > 0 and ext = ""then
            chunk = mid(buffer, instr(1, buffer, CHR(&hFF, &hD8)), instr(1, buffer, CHR(&hFF, &hD9)))
            ext = ".jpg"
        end if
        buffer = ""
        'Close #1
        ' attempt to write thumbnail to temp file
        if ext <> "" then
            f = freefile
            thumb = exepath + pathchar + "thumb" + ext
            open thumb for Binary Access Write as #f
                put #f, , chunk
            close #f
        else
            ' no cover art in mp3 optional use folder.jpg if present as thumb
        end if
        return true
    else
        ' no cover art in mp3 optional use folder.jpg if present as thumb
        logentry("notice", "no cover art found in: " + filename)
        return false
    end if
end function


' get base mp3 info
dim shared taginfo(1 to 6) as string
function getmp3baseinfo(fx1File as string) as boolean
    taginfo(1) = getmp3tag("artist",fx1File)
    taginfo(2) = getmp3tag("title", fx1File)
    taginfo(3) = getmp3tag("album", fx1File)
    taginfo(4) = getmp3tag("year",  fx1File)
    taginfo(5) = getmp3tag("genre", fx1File)
    ' use last part path as theme
    ReDim As String ordinance(0)
    explode(fx1File, "\", ordinance())
    taginfo(6) = ordinance(UBound(ordinance) -1)
    if taginfo(1) <> "----" and taginfo(2) <> "----" then
        'nop
    else    
        taginfo(1) = mid(left(fx1File, len(fx1File) - instr(fx1File, pathchar) -1), InStrRev(fx1File, pathchar) + 1, len(fx1File))
        taginfo(2) = ""
    end if                
    return true
end function

/' get http stream info
function gethttpstreaminfo(fx1Handle as HSTREAM) as boolean
    dim as const zstring ptr meta
    dim as string artist,title
    ' shoutcast
    meta = BASS_ChannelGetTags(fx1Handle, BASS_TAG_META)
    ' icecast
    if meta = 0 then
        meta = BASS_ChannelGetTags(fx1Handle, BASS_TAG_OGG)
    end if
    if meta <> 0 then
        dim as string raw       = *meta
        dim as integer start    = instr(raw, "'") + 1
        dim as integer endpos   = instr(start, raw, "';")
        title     = mid(raw, start, endpos - start)
        dim as integer dashpos  = instr(title, " - ")
        if dashpos > 0 then
            artist = left(title, dashpos - 1)
            title  = mid(title, dashpos + 3)
        else
            artist = title ' artist and title
        end if
       ' print *meta        
    end if
    taginfo(1) = artist
    taginfo(2) = title
    taginfo(3) = "----"
    taginfo(4) = "----"
    taginfo(5) = "----"
    return true
end function
'/

function getmp3playlist(filename as string, listtype as string) as integer
    dim              as long f
    dim itemnr       as integer = 1
    dim listitem     as string
    dim mp3listtype  as string = ""
    dim temptitle    as string = ""

    select case true  
        case instr(lcase(filename), ".pls") > 0
            mp3listtype = "pls"
        case instr(lcase(filename), ".m3u") > 0
            mp3listtype = "m3u"
        case else
            return 0
    end select    

    if len(filename) = 0 then
        logentry("warning", filename + " path or file not found.")
    else
        logentry("notice", "parsing and playing plylist " + filename)
    end if
    f = freefile
    Open filename For input As #f
    itemnr = 0

    do until eof(f)
        line input #f, listitem
        listitem = trim(listitem)

        if mp3listtype = "pls" then
            if instr(listitem, "=") > 0 then
                select case true
                    case instr(lcase(listitem), "file") = 1
                        itemnr += 1
                        redim preserve listrec.listname(0 to itemnr)
                        redim preserve listrec.listfile(0 to itemnr)
                        redim preserve listrec.listtype(0 to itemnr)
                        redim preserve listrec.listseqh(0 to itemnr)
                        listrec.listfile(itemnr) = trim( mid(listitem, instr(listitem, "=") + 1) )
                        listrec.listname(itemnr) = ""
                        listrec.listtype(itemnr) = listtype
                        listrec.listseqh(itemnr) = 0
                    case instr(lcase(listitem), "title") = 1
                        if itemnr >= 0 then
                            listrec.listname(itemnr) = trim( mid(listitem, instr(listitem, "=") + 1) )
                        end if
                    case else
                        ' nop
                end select
            end if
        end if

        if mp3listtype = "m3u" then
            if len(listitem) > 0 then
                select case true
                    case left(listitem, 7) = "#EXTINF"
                        dim as integer p = instr(listitem, ",")
                        if p > 0 then
                            temptitle = trim( mid(listitem, p + 1) )
                        else
                            temptitle = ""
                        end if
                    case left(listitem, 1) <> "#"
                        ' file/url line (after EXTINF or plain entry)
                        itemnr += 1
                        redim preserve listrec.listname(0 to itemnr)
                        redim preserve listrec.listfile(0 to itemnr)
                        redim preserve listrec.listtype(0 to itemnr)
                        redim preserve listrec.listseqh(0 to itemnr)
                        listrec.listfile(itemnr) = listitem
                        listrec.listname(itemnr) = temptitle
                        listrec.listtype(itemnr) = listtype
                        listrec.listseqh(itemnr) = 0
                        temptitle = ""
                    case else
                        ' nop
                end select
            end if
        end if
    loop
    close f
    'for i as integer = 0 to ubound(listrec.listname)
    '    with listrec
    '        print listrec.listname(i)
    '        print listrec.listfile(i)
    '        print listrec.listtype(i)
    '        print listrec.listseqh(i)
    '    end with
    'next i
    'maxmusicitems = itemnr
    return itemnr
end function

' export m3u
' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function exportm3u(folder as string, filterext as string, listtype as string = "m3u", htmloutput as string = "default", tag as string = "", tagquery as string = "") as integer
    ' setup filelist
    dim                as integer i = 1, j=1, n = 1, attrib, itemnr, maxfiles
    dim dummy          as string
    dim dummy2         as string
    dim tbname         as string
    dim file           as string
    dim fileext        as string
    dim fsize          as long
    dim fdate          as string
    dim fattr          as string
    dim argc(0 to 5)   as string
    dim argv(0 to 5)   as string

    redim path(1 to 1) As string
    'export to m3u
    Open exepath + pathchar + tagquery + ".m3u" For output As #20
    print #20, "#EXTM3U"
    print "exporting result to " & exepath + pathchar + tagquery + ".m3u"  & " ..."

    ' read dir recursive starting directory
    path(1) = folder 
    if( right(path(1), 1) <> pathchar) then
        file = dir(path(1), fbNormal or fbDirectory, @attrib)
        if( attrib and fbDirectory ) then
            path(1) += pathchar
        end if
    end if

    itemnr = 0
    cls
    while i <= n
    file = dir(path(i) + "*" , fbNormal or fbDirectory, @attrib)
        while file > ""
            if (attrib and fbDirectory) then
                if file <> "." and file <> ".." then
                    n += 1
                    redim preserve path(1 to n)
                    path(n) = path(i) + file + pathchar
                end if
            else
                fileext = lcase(mid(file, instrrev(file, ".")))
                if instr(1, filterext, fileext) > 0 and len(fileext) > 3 then
                    ' get specific file information
                    fsize = filelen(path(i) + file)
                    fdate = Format(FileDateTime(path(i) + file), "yyyy-mm-dd hh:mm:ss" )
                    If (attrib And fbReadOnly) <> 0 Then fattr = "read-only"
                    If (attrib And fbHidden  ) <> 0 Then fattr = "hidden"
                    If (attrib And fbSystem  ) <> 0 Then fattr = "system"
                    If (attrib And fbArchive ) <> 0 Then fattr = "archived"
                    select case listtype
                        case "m3u"
                            if instr(filterext, ".mp3") > 0 and htmloutput = "exif" then
                                Locate 1, 1   
                                print "scanning " & folder + " with filespec " + filterext + " with tag " & tag & " contains " & tagquery
                                print str(itemnr)
                                itemnr += 1
                                ' path(i) folder and drive
                                getmp3baseinfo(path(i) + file)
                                argc(0) = "artist"
                                argc(1) = "title"
                                argc(2) = "album"
                                argc(3) = "year"
                                argc(4) = "genre"
                                argc(5) = "nop"

                                argv(0) = taginfo(1)
                                argv(1) = taginfo(2)
                                argv(2) = taginfo(3)
                                argv(3) = taginfo(4)
                                argv(4) = taginfo(5)
                                argv(5) = "nop"
                            end if

                            For j As Integer = 0 To 5
                                'export to m3u
                                if argc(j) = tag and instr(lcase(argv(j)), lcase(tagquery)) > 0 then
                                    print #20, "#EXTINF:134," & argv(0) & " - " & argv(1)
                                    print #20, path(i) & file
                                    maxfiles += 1
                                end if
                            Next j
                    end select
                else
                    'logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    itemnr = itemnr - 1
    print "scanned " & itemnr & " files in " + folder + " with filespec " + filterext + " " & maxfiles & " file(s) found with " & tag & " " & tagquery
    logentry("notice", "scanned and exported m3u")
    close(20)
    return maxfiles

end function
