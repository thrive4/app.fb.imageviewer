' used for app launcher
#include once "crt/process.bi"
' dir function and provides constants to use for the attrib_mask parameter
#include once "vbcompat.bi"
#include once "dir.bi"
' for printf
#include once "crt/stdio.bi"

' disable filename globbing otherwise g:\* list files
' when using command how ever conflicts with dir()
' also odd this is used for 64bits but works with 32bits
' Extern _dowildcard Alias "_dowildcard" As Long
'Dim Shared _dowildcard As Long = 0

' setup log
dim shared logfile    as string
dim shared logtype    as string
dim shared appname    as string
dim shared appfile    as string
dim shared usecons    as string
dim shared exeversion as string

' note command(0) can arbitraly add the path so strip it
appname = mid(command(0), instrrev(command(0), "\") + 1)
' without file extension
if instr(appname, ".exe") > 0 then
    appname = left(appname, len(appname) - 4)
end if
' options logtype verbose, full
logtype = "verbose"
' options usecons true, false
usecons = "false"
' generic check for true or false
dim chk as boolean

' get version exe for log
declare function getfileversion(versinfo() as string, versdesc() as string) as integer
declare function replace(byref haystack as string, byref needle as string, byref substitute as string) as string
dim as integer c, resp
dim as string versinfo(8)
dim as string versdesc(7) =>_
    {"CompanyName",_
    "FileDescription",_
    "FileVersion",_
    "InternalName",_
    "LegalCopyright",_
    "OriginalFilename",_
    "ProductName",_
    "ProductVersion"}
versinfo(8) = appname + ".exe"
resp = getfileversion(versinfo(),versdesc())
exeversion = replace(trim(versinfo(2)), ", ", ".")

' get metric os
dim shared os as string
os = "unknown"
#ifdef __FB_WIN32__
    os = "windows"
#endif
#ifdef __FB_UNIX__
    os = "unix"
#endif

' allows for hybrid usage console feedback when app is compiled in -gui mode
' courtesy https://stackoverflow.com/questions/510805/can-a-win32-console-application-detect-if-it-has-been-run-from-the-explorer-or-n
' comment bobsobol
function consoleprint(msg as string, forcewstr as boolean = false) as boolean

    if AttachConsole(ATTACH_PARENT_PROCESS) THEN ' gui mode
        Shell("Cls")
		freopen( "CON", "r", stdin )
		freopen( "CON", "w", stdout )
		freopen( "CON", "w", stderr )
        ' sigh work around msg = wstr(msg) does not work ....
        ' also todo figure out printf with wstr
        if forcewstr then
            print wstr(msg)
        else
            printf(msg)
        end if
        ' restore the normal prompt in cmd console
        freeconsole()
		PostMessage(GetForegroundWindow, WM_KEYDOWN, VK_RETURN, 0)
    else
        if forcewstr then
            print wstr(msg)
        else
            print msg
        end if
    END IF

    return true

end function

' metric functions
' ______________________________________________________________________________'

' used for logging
' entrytypes: error, fatal, notice, warning, terminate
Function logentry(entrytype As String, logmsg As String) As Boolean

    ' validate logentry
    If InStr(logmsg, "|") > 0 Then
        logmsg = "entry contained delimeter -> | <-"
    End If

    ' output to console
    if usecons = "true" then
        consoleprint time & " " + entrytype + " | " + logmsg
    end if

    ' setup logfile
    dim f as long
    f = FreeFile
    logfile = exepath + "\" + appname + ".log"
    if FileExists(logfile) = false then
        Open logfile For output As #f
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + logfile + " created"
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + "version " + exeversion
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + "platform " + os
        close #f
    end if

    if (entrytype = "warning" or entrytype = "notice") and logtype = "verbose" then
        return true
    end if

    ' write to logfile
    Open logfile For append As #f
    print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + entrytype + "|" + appname + "|" + logmsg
    close #f

    ' normal termination or fatal error
    select case entrytype
        case "fatal"
            consoleprint logmsg
            end
        case "terminate"
            end
    end select

    return true
End function

' get fileversion executable or dll windows only
function getfileversion(versinfo() as string, versdesc() as string) as integer

    dim as integer bytesread,c,dwHandle,res,verSize
    dim as string buffer,ls,qs,tfn
    dim as ushort ptr b1,b2
    dim as ubyte ptr bptr

    tfn=versinfo(8)
    if dir(tfn)="" then return -1
    verSize=GetFileVersionInfoSize(tfn,@dwHandle)
    if verSize=0 then return -2
    dim as any ptr verdat=callocate(verSize*2)

    res=GetFileVersionInfo(strptr(tfn),dwHandle,verSize*2,verdat)
    res=_
        VerQueryValue(_
            verdat,_
            "\VarFileInfo\Translation",_
            @bptr,_
            @bytesread)

    if bytesread=0 then deallocate(verdat):return -3

    b1=cast(ushort ptr,bptr)
    b2=cast(ushort ptr,bptr+2)
    ls=hex(*b1,4)& hex(*b2,4)

    for c=0 to 7
        qs="\StringFileInfo\" & ls & "\" & versdesc(c)
        res=_
            VerQueryValue(_
                verdat,_
                strptr(qs),_
                @bptr,_
                @bytesread)
        if bytesread>0 then
            buffer=space(bytesread)
            CopyMemory(strptr(buffer),bptr,bytesread)
            versinfo(c)=buffer
        else
            versinfo(c)="N/A"
        end if
    next c
    deallocate(verdat)

    return 1

end function

' generic file functions
' ______________________________________________________________________________'

' list files in folder
function getfilesfromfolder (filespec As String) as boolean
    Dim As String filename = Dir(filespec, 1)
    if len(filename) = 0 then print "path not found..." end if
    Do While Len(filename) > 0
        filename = Dir()
    Loop
    return true
end function

' list folders
function getfolders (filespec As String) as boolean
    Dim As String filename = Dir(filespec, fbDirectory)
    if len(filename) = 0 then print "path not found..." end if
    Do While Len(filename) > 0
        filename = Dir()
    Loop
    return true
end function

' create a new file
Function newfile(filename As String) As boolean
    Dim f As long

    if FileExists(filename) then
        logentry("warning", "creating " + filename + " file excists")
        return false
    end if    

    f = FreeFile
    Open filename For output As #f
    logentry("notice", filename + " created")
    close(f)
    return true

End Function

' append to an excisiting file
Function appendfile(filename As String, msg as string) As boolean
    Dim f As long

    if FileExists(filename) = false then
        logentry("error", "appending " + filename + " file does not excist")
        return false
    end if

    f = FreeFile
    Open filename For append As #f
    print #f, msg
    close(f)
    return true

End Function

' read a file
Function readfromfile(filename As String) As long
    Dim f As long

    if FileExists(filename) = false then
        logentry("error", "reading " + filename + " file does not excist")
    end if

    f = FreeFile
    Open filename For input As #f
    return f

End Function

' delete a file
Function delfile(filename As String) As boolean

    if FileExists(filename) = true then
        If Kill(filename) <> 0 Then
            logentry("warning", "could not delete " + filename)
            return false
        end if
    end if
    return true

End Function

' check path
Function checkpath(chkpath As String) As boolean

    dim dummy as string
    dummy = curdir

    if chdir(chkpath) <> 0 then
        logentry("warning", "path " + chkpath + " not found")
        chdir(dummy)
        return false
    end if

    chdir(dummy)
    return true

End Function

' resolve path commandline argument
Function resolvepath(path As String) As String

    Dim buffer        as String * 260
    dim resolvedpath  as string
    Dim length        as Integer 
    length = GetFullPathName(path, 260, buffer, Null)

    resolvedpath = Left(buffer, length)
    if checkpath(resolvedpath) = false and instr(path, "..") = 0 then
        resolvedpath = path
    end if  

    return resolvedpath

end function

' localization file functions
' ______________________________________________________________________________'

' localization can be applied by getting a locale or other method
sub displayhelp(locale as string)
    dim item  as string
    dim dummy as string
    dim f     as long
    f = freefile

    ' get text
    if FileExists(exepath + "\conf\" + locale + "\help.ini") then
        'nop
    else
        logentry("error", "open " + exepath + "\conf\" + locale + "\help.ini" + " file does not excist")
        locale = "en"
    end if
    Open exepath + "\conf\" + locale + "\help.ini" For input As #f
    Do Until EOF(f)
        Line Input #f, item
        dummy = dummy + item + chr$(13) + chr$(10)
    Loop
    close(f)
    consoleprint (dummy, true)
end sub

' setup ui labels aka data spindel
type tbrec
    as string fieldname(any)
    as string fieldvalue(any)
end type
dim shared record as tbrec
common shared recnr as integer
recnr = 0

' get key value pair cheap localization via ini file
Function readuilabel(filename as string) as boolean
    dim itm    as string
    dim inikey as string
    dim inival as string
    dim f      as integer

    if FileExists(filename) = false then
        logentry("error", filename + " does not excist switching to default language")
        filename = exepath + "\conf\en\menu.ini"
    end if
    f = readfromfile(filename)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            if inival = "" then
                logentry("error", inikey + " has empty value in " + filename)
                inival = "null"
            end if
            'print inikey + " - " + inival
            recnr += 1
            redim preserve record.fieldname(0 to recnr)
            redim preserve record.fieldvalue(0 to recnr)
            record.fieldname(recnr)  = inikey
            record.fieldvalue(recnr) = inival
        end if
    loop
    close(f)
    return true
end function

' display ui lable with unicode and semi automatic spacing via offset
function getuilabelvalue(needle as string, suffix as string = "", offset as integer = 10) as boolean
    dim fieldname  as string = ""
    dim fieldvalue as string = ""

    for i as integer = 0 to recnr
        with record
            if record.fieldname(i) = needle then
                fieldname  = record.fieldname(i)
                fieldvalue = record.fieldvalue(i)
            end if
        end with
    next i
    if fieldname = "" or fieldvalue = "" then
        print needle + " not found or empty " + suffix
        return false
    else
        print wstr(fieldvalue + space(offset - Len(fieldvalue)) + suffix)
        return true
    end if
end function

' file type specific functions
' ______________________________________________________________________________'

' code by squall4226
' see https://www.freebasic.net/forum/viewtopic.php?p=149207&hilit=user+need+TALB+for+album#p149207
Function getmp3tag(searchtag As String, fn As String) As String
   'so we can avoid having the user need TALB for album, TIT2 for title etc, although they are accepted
   Dim As Integer skip, offset' in order to read certain things right
   Dim As UInteger sig_to_find, count, maxcheck = 3000
   dim as long fnum
   dim as UShort tag_length
   Dim As UShort unitest, mp3frametest
   Dim As String tagdata

   Select Case UCase(searchtag)
        Case "HEADER", "ID3"
            searchtag = "ID3" & Chr(&h03)
        Case "TITLE", "TIT2"
            searchtag = "TIT2"
        Case "ARTIST", "TPE1"
            searchtag = "TPE1"
        Case "ALBUM", "TALB"
            searchtag = "TALB"
        Case "COMMENT", "COMM"
            searchtag = "COMM"
        Case "COPYRIGHT", "TCOP"
            searchtag = "TCOP"
        Case "COMPOSER", "TCOM"
            searchtag = "TCOM"
        Case "BEATS PER MINUTE", "BPM", "TPBM"
            searchtag = "TBPM"
        Case "PUBLISHER", "TPUB"
            searchtag = "TPUB"
        Case "URL", "WXXX"
            searchtag = "WXXX"
        Case "PLAY COUNT" "PCNT"
            searchtag = "PCNT"
        Case "GENRE", "TCON"
            searchtag = "TCON"
        Case "ENCODER", "TENC"
            searchtag = "TENC"
        Case "TRACK", "TRACK NUMBER", "TRCK"
            searchtag = "TRCK"
        Case "YEAR", "TYER"
            searchtag = "TYER"      
        'Special, in this case we will return the datasize if present, or "-1" if no art
        Case "PICTURE", "APIC"
            searchtag = "APIC"
            'Not implemented yet!
        Case Else
            'Tag may be invalid, but search anyway, there are MANY tags, and we have error checking
   End Select

   fnum = FreeFile
   Open fn For Binary Access Read As #fnum
   If Lof(fnum) < maxcheck Then maxcheck = Lof(fnum)
   For count = 0 to maxcheck Step 1
        Get #fnum, count, sig_to_find
        If sig_to_find = Cvi(searchtag) Then
             If searchtag = "ID3" & Chr(&h03) Then
                Close #fnum
                Return "1" 'Because there is no data here, we were just checking for the ID3 header
             EndIf
             'test for unicode
             Get #fnum, count+11, unitest         
             If unitest = &hFEFF Then 'unicode string
                skip = 4
                offset = 13           
             Else 'not unicode string
                skip = 0
                offset = 10            
             EndIf
             
             Get #fnum, count +7, tag_length 'XXXXYYYZZ Where XXXX is the TAG, YYY is flags or something, ZZ is size

             If tag_length-skip < 1 Then
                Close #fnum
                Return "ERROR" 'In case of bad things
             EndIf
             
             Dim As Byte dataget(1 To tag_length-skip)
             Get #fnum, count+offset, dataget()
             
             For i As Integer = 1 To tag_length - skip
                if dataget(i) < 4 then dataget(i) = 0 ' remove odd characters
                If dataget(i) <> 0 Then tagdata + = Chr(dataget(i)) 'remove null spaces from ASCII data in UNICODE string
             Next
        End If
        If tagdata <> "" then exit For ' stop searching!
   Next
   Close #fnum
   
   If Len(tagdata) = 0 Then
        'If the tag was just not found or had no data then "----"
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
    delfile(exepath + "\thumb.jpg")
    delfile(exepath + "\thumb.png")
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
            thumb = exepath + "\thumb" + ext
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
dim shared taginfo(1 to 5) as string
function getmp3baseinfo(fx1File as string) as boolean
    taginfo(1) = getmp3tag("artist",fx1File)
    taginfo(2) = getmp3tag("title", fx1File)
    taginfo(3) = getmp3tag("album", fx1File)
    taginfo(4) = getmp3tag("year",  fx1File)
    taginfo(5) = getmp3tag("genre", fx1File)
    if taginfo(1) <> "----" and taginfo(2) <> "----" then
        'nop
    else    
        taginfo(1) = mid(left(fx1File, len(fx1File) - instr(fx1File, "\") -1), InStrRev(fx1File, "\") + 1, len(fx1File))
        taginfo(2) = ""
    end if                
    return true
end function

function getmp3playlist(filename as string, listname as string) as integer
    dim              as long f, g, h
    dim itemnr       as integer = 1
    dim listitem     as string
    dim listduration as integer
    dim mp3listtype  as string = ""
    f = freefile

    select case true  
        case instr(filename, ".pls") > 0
            mp3listtype = "pls"
        case instr(filename, ".m3u") > 0
            mp3listtype = "m3u"
        case else
            return 0
    end select    

    if len(filename) = 0 then
        logentry("warning", filename + " path or file not found.")
    else
        logentry("notice", "parsing and playing plylist " + filename)
    end if
    Open filename For input As #f
    g = freefile
    open exepath + "\" + listname + ".tmp" for output as #g
    h = freefile
    open exepath + "\" + listname + ".lst" for output as #h
    itemnr = 0

    Do Until EOF(f)
        Line Input #f, listitem
        ' ghetto parsing pls
        if mp3listtype = "pls" then
            if instr(listitem, "=") > 0 then
                select case true
                    case instr(listitem, "file") > 0
                        print #g, mid(listitem, instr(listitem, "=") + 1, len(listitem))
                        print #h, mid(listitem, instr(listitem, "=") + 1, len(listitem))
                        itemnr += 1
                    case instr(listitem, "title" + str(itemnr)) > 0
                    case instr(listitem, "length" + str(itemnr)) > 0
                        listduration = listduration + val(mid(listitem, instr(listitem, "=") + 1, len(listitem)))
                    case len(listitem) = 0
                        'nop
                    case else
                        'msg64 = msg64 + listitem
                end select
            end if
        end if
        ' ghetto parsing m3u
        if mp3listtype = "m3u" then
            ' ghetto parsing m3u
            if len(listitem) > 0 then
                select case true
                    case instr(listitem, "EXTINF:") > 0
                        listduration = listduration + val(mid(listitem, instr(listitem, ":") + 1, len(instr(listitem, ","))- 1))
                    case instr(listitem, ".") > 0
                        print #g, listitem
                        print #h, listitem
                        itemnr += 1
                    case len(listitem) = 0
                        'nop
                    case else
                        'msg64 = msg64 + listitem
                end select
            end if
        end if
    Loop
    'maxmusicitems = itemnr
    close(f)
    close(g)
    close(h)
    return itemnr

end function

' export m3u
' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function exportm3u(folder as string, filterext as string, listtype as string = "m3u", htmloutput as string = "default", tag as string = "", tagquery as string = "") as integer
    ' setup filelist
    dim                as integer i = 1, j=1, n = 1, attrib, itemnr, maxfiles
    dim                as long tmp
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
    Open exepath + "\" + tagquery + ".m3u" For output As #20
    print #20, "#EXTM3U"

    #ifdef __FB_LINUX__
      const pathchar = "/"
    #else
      const pathchar = "\"
    #endif
    ' read dir recursive starting directory
    path(1) = folder 
    if( right(path(1), 1) <> pathchar) then
        file = dir(path(1), fbNormal or fbDirectory, @attrib)
        if( attrib and fbDirectory ) then
            path(1) += pathchar
        end if
    end if

    recnr = 0
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
                                consoleprint "scanning " & folder + " with filespec " + filterext + " with tag " & tag & " contains " & tagquery
                                consoleprint str(recnr)
                                recnr += 1
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

    recnr = recnr - 1
    consoleprint "scanned " & recnr & " files in " + folder + " with filespec " + filterext + " " & maxfiles & " file(s) found with " & tag & " " & tagquery
    logentry("notice", "scanned and exported m3u")
    close(20)
    return maxfiles

end function

' text related functions
' ______________________________________________________________________________'

' split or explode by delimiter return elements in array
' based on https://www.freebasic.net/forum/viewtopic.php?t=31691 code by grindstone
Function explode(haystack As String = "", delimiter as string, ordinance() As String) As UInteger
    Dim As UInteger b = 1, e = 1   'pointer to text, begin and end
    Dim As UInteger x              'counter

    Do Until e = 0
      x += 1
      ReDim Preserve ordinance(x)             'create new array element
      e = InStr(e + 1, haystack, delimiter)   'set end pointer to next space
      ordinance(x) = Mid(haystack, b, e - b)  'cut text between the pointers and write it to the array
      b = e + 1                               'set begin pointer behind end pointer for the next word
    Loop

    Return x 'nr of elements returned

    ' sample code for calling the function explode
    'ReDim As String ordinance(0)
    'explode("The big brown fox jumped over; the lazy; dog", ";", ordinance())
    'print UBound(ordinance)
    'For x As Integer = 1 To UBound(ordinance)
    '    Print ordinance(x)
    'Next

End Function

function replace(byref haystack as string, byref needle as string, byref substitute as string) as string
'found at https://freebasic.net/forum/viewtopic.php?f=2&t=9971&p=86259&hilit=replace+character+in+string#p86259
    dim as string temphaystack = haystack
    dim as integer fndlen = len(needle), replen = len(substitute)
    dim as integer i = instr(temphaystack, needle)

    while i
        temphaystack = left(temphaystack, i - 1) & substitute & mid(temphaystack, i + fndlen)
        i = instr(i + replen, temphaystack, needle)
    wend

    return temphaystack

end function
