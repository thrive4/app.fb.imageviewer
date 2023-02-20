' disable filename globbing otherwise g:\* list files
' when using command how ever conflicts with dir()
' also odd this is used for 64bits but works with 32bits
' Extern _dowildcard Alias "_dowildcard" As Long
'Dim Shared _dowildcard As Long = 0

' setup log
dim shared logfile as string
dim shared logtype as string
dim shared appname as string
dim shared appfile as string
dim shared usecons as string
dim shared exeversion as string
dim shared locale as string

' note command(0) can arbitraly add the path so strip it
appname = mid(command(0), instrrev(command(0), "\") + 1)
' without file extension
appname = left(appname, len(appname) - 4)
' options logtype verbose, full
logtype = "verbose"
' options usecons true, false
usecons = "false"
' generic check for true or false
dim chk as boolean

' used for logging
Function logentry(entrytype As String, logmsg As String) As Boolean

    ' validate logentry
    If InStr(logmsg, "|") > 0 Then
        logmsg = "entry contained delimeter -> | <-"
    End If

    ' output to console
    if usecons = "true" then
        print time & " " + entrytype + " | " + logmsg
    end if

    ' setup logfile
    dim f as integer
    f = FreeFile
    logfile = exepath + "\" + appname + ".log"
    if FileExists(logfile) = false then
        Open logfile For output As #f
        logmsg = exeversion + " " + logfile + " created"
        print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + "notice" + "|" + appname + "|" + logmsg
        close #f
        exit function
    end if

    if entrytype <> "error" and entrytype <> "terminate" and logtype = "verbose" then
        exit function
    end if

    ' write to logfile
    Open logfile For append As #f
    print #f, format(now, "dd/mm/yyyy") + " - " + time + "|" + entrytype + "|" + appname + "|" + logmsg
    close #f

    ' normal termination or fatal error
    if entrytype = "terminate" then
        end
    end if

    return true
End function

' localiztion can be applied by getting a locale or other method
sub displayhelp
    dim dummy as string
    dim f as integer
    f = freefile
    
    ' get / set locale
    select case locale
        case "de"
            locale = "de"
        case "en"
        ' default locale
        case else
            locale = "en"
    end select    
    ' get text
    Open exepath + "\help-" + locale + ".ini" For input As #f
    Do Until EOF(f)
        Line Input #f, dummy
        print dummy    
    Loop
    close f

end sub

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
    Dim f As integer

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

' create a temp file
Function tmpfile(filename As String) As boolean
    Dim f As integer

    if FileExists(filename) = true then
      If Kill(filename) <> 0 Then
          logentry("warning", "could not delete " + filename )
      end if
    end if

    f = FreeFile
    Open filename For output As #f
    logentry("notice", filename + " created")
    close(f)
    return true

End Function

' append to an excisiting file
Function appendfile(filename As String, msg as string) As boolean
    Dim f As integer

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
    Dim f As integer

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

    return true

End Function

' get fileversion executable or dll
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

' split or explode by delimiter return elements in array
' based on https://www.freebasic.net/forum/viewtopic.php?t=31691 code by grindstone
Function explode(haystack As String = "", delimiter as string, ordinance() As String) As UInteger
    Dim As String text = haystack  'remind explode as working copy
    Dim As UInteger b = 1, e = 1   'pointer to text, begin and end
    Dim As UInteger x              'counter
    ReDim ordinance(0)             'reset array

    Do Until e = 0
      x += 1
      ReDim Preserve ordinance(x)         'create new array element
      e = InStr(e + 1, text, delimiter)   'set end pointer to next space
      ordinance(x) = Mid(text, b, e - b)  'cut text between the pointers and write it to the array
      b = e + 1                           'set begin pointer behind end pointer for the next word
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

' text substitution
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
