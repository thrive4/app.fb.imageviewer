' used for app launcher
#include once "crt/process.bi"
' dir function and provides constants to use for the attrib_mask parameter
#include once "vbcompat.bi"
#include once "dir.bi"
' for printf
#include once "crt/stdio.bi"

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
            consoleprint(logmsg)
            end
        case "terminate"
            end
    end select

    return true
End function

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

' generic file functions
' ______________________________________________________________________________'


' sample code for calling the function getfolders and getfilesfromfolder
'ReDim As String ordinance(0)
'getfilesfromfolder("i:\games\*", ordinance())
'print UBound(ordinance)
'For x As Integer = 1 To UBound(ordinance)
'    Print ordinance(x)
'Next

' list files in folder
function getfilesfromfolder(filespec As String, ordinance() As String) as uinteger
    Dim As UInteger x      = 0 'counter
    Dim As String filename = Dir(filespec, fbnormal, fbHidden and fbSystem and fbArchive and fbReadOnly)

    if len(filename) = 0 then print "path not found..." end if
    Do While Len(filename) > 0
        x += 1
        ReDim Preserve ordinance(x) 'create new array element
        ordinance(x) = filename
        filename = Dir()
    Loop

    return x

end function

' list folders
function getfolders (filespec As String, ordinance() As String) as uinteger
    Dim As UInteger x = 0 'counter
    var mask          = fbDirectory or fbHidden or fbSystem or fbArchive or fbReadOnly
    var attrib        = 0
    var filename      = dir( filespec, mask, attrib )
    
    if len(filename) = 0 then print "path not found..." end if
    ' show directory regardless if it is system, hidden, read-only, or archive
    while(filename > "")
        if(attrib and fbDirectory) and (filename <> "." and filename <> "..") then
            x += 1
            ReDim Preserve ordinance(x) 'create new array element
            ordinance(x) = filename
        end if
        filename= dir(attrib)
    wend

    return x

end function

function getdrivelabel(drive as string) as string
    Dim As ZString * 1024 deviceName
    Dim As ZString * 1024 volumeName
    QueryDosDevice(drive, deviceName, 1024)
    GetVolumeInformation(drive, volumeName, 1024, 0, 0, 0, 0, 0)
    return volumeName
end function

function getdrivestorage(drive as string, metric as string) as ULongInt
    Dim As ULARGE_INTEGER freeBytesAvailable
    Dim As ULARGE_INTEGER totalNumberOfBytes
    Dim As ULARGE_INTEGER totalNumberOfFreeBytes
    If GetDiskFreeSpaceEx(drive, @freeBytesAvailable, @totalNumberOfBytes, @totalNumberOfFreeBytes) Then
        select case metric
            case "capacity"
                return totalNumberOfBytes.QuadPart
            case "space"
                return totalNumberOfFreeBytes.QuadPart
            case else
                return 0
        end select
    Else
    '    Print "Error: "; GetLastError()
        return 0
    End If
end function

function convertbytesize(totalsize as longint) as string

    dim size as string
    if totalsize < 1024 then
        size = str(totalsize) & " bytes"
    elseif totalsize < 1048576 then
        size = format(totalsize / 1024.0, "0.00") & " KB"
    elseif totalsize < 1073741824 then
        size = format(totalsize / 1048576.0, "0.00") & " MB"
    else
        size = format(totalsize / 1073741824.0, "0.00") & " GB"
    end if

    return size

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

    if left$(lcase(path), 7) = "http://" or left$(lcase(path), 8) = "https://" then
        resolvedpath = path
    else
        length = GetFullPathName(path, 260, buffer, Null)
        If length > 0 Then
            resolvedpath = Left(buffer, length)
        Else
            resolvedpath = path ' fallback if API fails
        End If
    end if
    return resolvedpath
End Function

' localization file functions
' ______________________________________________________________________________'

' localization can be applied by getting a locale or other method
sub displayhelp(locale as string)
    dim item  as string
    dim dummy as string
    dim f     as long
    f = freefile

    ' get text
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
