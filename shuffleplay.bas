' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function createlist(folder as string, filterext as string, listname as string) as integer
    ' setup filelist
    dim as integer i = 1, n = 1, attrib
    redim path(1 to 1)    as string
    dim chk               as boolean
    dim file              as string
    dim fileext           as string
    dim maxfiles          as integer
    dim f                 as integer
    f = freefile
    dim filelist          as string = exepath + "\" + listname + ".tmp"
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
        print "could not create filelist: " + filelist
        exit function
    end if
    
    ' setup base shuffle and reduce probability
    dim lastitem as string = exepath + "\" + listname + ".lst"
    chk = newfile(lastitem)
    
    return maxfiles

end function

dim shared currentimage  as integer
dim shared currentsong   as integer
dim shared currentshader as integer
' video shuffle is handeld by mpv
dim shared currentvideo  as integer
' todo fix currentimage for music
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
        case "screenshow"
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
                if val(listitem) = baseitem then
                    'print "old item " & listitem & " new choice " & baseitem
                    ' roll the dice and hope for the best
                    randomize
                    baseitem = int(rnd * maxitems) + 1
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
            'print "choice  " & itemnr
            currentitem = itemnr
            ' work around for multiple lists todo improve
            select case listname
                case "image"
                    currentimage = currentitem
                case "music"
                    currentsong = currentitem
                case "screenshow"
                    currentimage = currentitem
                case "video"
                    currentvideo = currentitem
                case "shader"
                    currentshader = currentitem
            end select
            close(tmp)
            return listitem
        end if
        itemnr += 1
    Loop

    close
end function
