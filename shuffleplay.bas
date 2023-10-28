' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function createlist(folder as string, filterext as string, listname as string) as integer
    ' setup filelist
    dim chk            as boolean
    redim path(1 to 1) As string
    dim as integer i = 1, n = 1, f, attrib
    dim file           as string
    dim fileext        as string
    dim maxfiles       as integer
    f = freefile
    dim filelist as string = exepath + "\" + listname + ".tmp"
    open filelist for output as #f

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

    while i <= n
    file = dir(path(i) + "*" , fbNormal or fbDirectory, @attrib)
        while file > ""
            if (attrib and fbDirectory) then
                if file <> "." and file <> ".." then
                    ' todo evaluate limit recursive if starting folder is root
                    if len(path(1)) > 3 then
                        n += 1
                        redim preserve path(1 to n)
                        path(n) = path(i) + file + pathchar
                    else
                        logentry("terminate", "scanning from root dir not supported! " + path(i))
                    end if
                end if
            else
                fileext = lcase(mid(file, instrrev(file, ".")))
                if instr(1, filterext, fileext) > 0 and len(fileext) > 3 then 
                    print #f, path(i) & file
                    maxfiles += 1
                else
                    logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend
    close(f)

    ' chk if filelist is created
    if FileExists(filelist) = false then
        logentry("warning", "could not create filelist: " + filelist)
    end if
    
    ' setup base shuffle and reduce probability
    dim lastitem as string = exepath + "\" + listname + ".lst"
    chk = newfile(lastitem)
    
    return maxfiles
end function

' used for listplay sets selected item as current
function setcurrentlistitem(listname as string, filename as string) as integer
    dim listitem as string
    dim tmp as integer
    dim itemnr as integer = 1

    ' scan for filename in list
    listname = exepath + "\" + listname + ".tmp"
    tmp = readfromfile(listname)
    Do Until EOF(tmp)
        Line Input #tmp, listitem
        if listitem = filename then
            exit do
        end if
        itemnr += 1
    Loop
    close(tmp)
    return itemnr
end function

dim shared currentimage  as integer
dim shared currentsong   as integer
dim shared currentshader as integer
' video shuffle is handeld by mpv
dim shared currentvideo  as integer
function listplay (playtype as string, listname as string) as string

    ' setup item file and item count
    dim tmp         as integer
    dim chk         as boolean
    Dim listitem    as string
    Dim currentitem as integer
    dim itemnr      as integer = 1
    dim maxitems    as integer = 0
    dim baseitem    as integer
    dim lastitem    as string = exepath + "\" + listname + ".lst"
    dim tempfile    as string = exepath + "\" + listname + ".tmp"

    ' work around for multiple lists todo improve
    select case listname
        case "image"
            currentitem = currentimage
        case "music"
            currentitem = currentsong
        case "slideshow"
            currentitem = currentimage
        case "video"
            currentitem = currentvideo
        case "shader"
            currentitem = currentshader
    end select
    ' count items in list
    tmp = readfromfile(tempfile)
    Do Until EOF(tmp)
        Line Input #tmp, listitem
        itemnr += 1
    Loop
    close(tmp)

    ' note linefeed at end of list caused by pipelinig on os
    maxitems = itemnr - 1 
    itemnr = 1

    ' count items in list lastitem wipe when equal to maxitems
    tmp = readfromfile(lastitem)
    Do Until EOF(tmp)
        Line Input #tmp, listitem
        itemnr += 1
    Loop
    close(tmp)
    if itemnr > maxitems then
        If Kill(lastitem) <> 0 Then
            logentry("error", "deleteing " + lastitem)
        end if
        chk = newfile(lastitem)
        logentry("notice", "item list reset!")
    end if
    itemnr = 1
    select case playtype
        case "shuffle"
            ' choose an item
            randomize
            baseitem = int(rnd * maxitems) + 1
            ' fill lastitem list and check if item is already used
            tmp = readfromfile(lastitem)
            Do Until EOF(tmp)
                Line Input #tmp, listitem
                ' check if already chosen reduce probabilty clunky but works....
                if val(listitem) = baseitem then
                    ' roll the dice and hope for the best
                    randomize
                    baseitem = int(rnd * maxitems) + 1
                    'print "old item #1" & listitem & " new choice " & baseitem
                end if
                if val(listitem) = baseitem then
                    ' roll the dice and hope for the best
                    randomize
                    baseitem = int(rnd * maxitems) + 1
                    'print "old item #2" & listitem & " new choice " & baseitem
                end if
            loop
            close(tmp)
            chk = appendfile(lastitem, str(baseitem))
        case "linear"
            baseitem = 1
            currentitem = currentitem + 1
            baseitem = currentitem
            ' wrap to first image
            if currentitem > maxitems then
                baseitem = 1
            end if
        case "linearmin"
            baseitem = 1
            if currentitem >= 1 then
                currentitem = currentitem - 1
                baseitem = currentitem
            end if
            ' wrap to last image
            if currentitem = 0 then
                currentitem = maxitems
                baseitem = currentitem
            end if
    end select

    ' get specific item from list
    tmp = readfromfile(tempfile)
    Do Until EOF(tmp)
        Line Input #tmp, listitem
        if itemnr = baseitem then
            currentitem = itemnr
            exit do
        end if
        itemnr += 1
    Loop
    close(tmp)

    ' work around for multiple lists todo improve
    select case listname
        case "image"
            currentimage = currentitem
        case "music"
            currentsong = currentitem
        case "slideshow"
            currentimage = currentitem
        case "video"
            currentvideo = currentitem
        case "shader"
            currentshader = currentitem
    end select

    return listitem

end function
