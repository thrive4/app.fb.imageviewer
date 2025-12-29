' setup playlist
type lrec
    as string  listname(any)
    as string  listfile(any)
    as string  listtype(any)
    as integer listseqh(any)
end type
dim shared    listrec as lrec
common shared listnr  as integer
listnr = 0

' generate list of files recursive
' based on recursive dir code of coderjeff https://www.freebasic.net/forum/viewtopic.php?t=5758
function createlist(folder as string, filterext as string, listtype as string) as integer
    ' setup filelist
    dim chk            as boolean
    redim path(1 to 1) As string
    dim as integer i = 1, n = 1, attrib
    dim file           as string
    dim fileext        as string
    dim maxfiles       as integer
    dim dummy          as string = curdir

    #ifdef __FB_LINUX__
      const pathchar = "/"
    #else
      const pathchar = "\"
    #endif

    if chdir(folder) <> 0 then
        chdir(dummy)
        print folder + " " + "not found"
        logentry("fatal", "path " + folder + " not found")
    end if
 
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
                    listnr += 1
                    redim preserve listrec.listname(0 to listnr)
                    redim preserve listrec.listfile(0 to listnr)
                    redim preserve listrec.listtype(0 to listnr)
                    redim preserve listrec.listseqh(0 to listnr)
                    listrec.listname(listnr) = file
                    listrec.listfile(listnr) = path(i) & file
                    listrec.listtype(listnr) = listtype
                    listrec.listseqh(listnr) = 0
                    maxfiles += 1
                else
                    logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    return maxfiles
end function

function getcurrentlistitem(listtype as string, filename as string) as integer
    dim itemnr as integer = -1

    for i as integer = 0 to listnr
        with listrec
            itemnr += 1
            if listrec.listtype(i) = listtype and listrec.listfile(i) = filename then
                'print listrec.listname(i)
                'print listrec.listfile(i)
                'print listrec.listtype(i)
                'print listrec.listseqh(i)
                exit for
            end if
        end with
    next i

    return itemnr
end function

function getmaxitemslist(listtype as string) as integer
    dim itemnr as integer = 0

    for i as integer = 0 to listnr
        with listrec
            if listrec.listtype(i) = listtype then
                itemnr += 1
            end if
        end with
    next i

    return itemnr
end function

sub setsequence(currentitem as integer)
    dim as string lt = listrec.listtype(currentitem)
    dim as integer maxseq = 0
    ' get highest sequence number for listtype
    for i as integer = 0 to listnr
        if listrec.listtype(i) = lt then
            if listrec.listseqh(i) > maxseq then
                maxseq = listrec.listseqh(i)
            end if
        end if
    next
    listrec.listseqh(currentitem) = maxseq + 1
end sub

sub clearseq(listtype as string)
    for i as integer = 0 to listnr
        if listrec.listtype(i) = listtype then
            listrec.listseqh(i) = 0
        end if
    next
end sub

function listshuffle(listtype as string) as integer
    dim as integer candidates(0 to listnr)
    dim as integer count = 0
    dim as integer selected = -1
    dim as integer i

    ' unplayed items (listseqh = 0)
    for i = 0 to listnr
        if listrec.listtype(i) = listtype and listrec.listseqh(i) = 0 then
            candidates(count) = i
            count += 1
        end if
    next

    ' reset sequences listtype
    if count = 0 then
        for i = 0 to listnr
            if listrec.listtype(i) = listtype then
                listrec.listseqh(i) = 0
            end if
        next
        ' rebuild list
        count = 0
        for i = 0 to listnr
            if listrec.listtype(i) = listtype and listrec.listseqh(i) = 0 then
                candidates(count) = i
                count += 1
            end if
        next
    end if

    ' get unplayed item
    if count > 0 then
        dim as integer rndidx = int(rnd * count)
        selected = candidates(rndidx)
    end if

    return selected
end function

function listnext(listtype as string, playtype as string, currentitem as integer) as integer
    dim as integer nextidx = -1
    dim as integer i

    if lcase(playtype) = "shuffle" then
        return listshuffle(listtype)
    end if

    ' linear
    for i = currentitem + 1 to listnr
        if listrec.listtype(i) = listtype then
            nextidx = i
            exit for
        end if
    next
    ' wrap to first of same type
    if nextidx = -1 then
        for i = 0 to listnr
            if listrec.listtype(i) = listtype then
                nextidx = i
                exit for
            end if
        next
    end if

    return nextidx
end function

function listprevious(listtype as string, playtype as string, currentitem as integer) as integer
    dim as integer i, previdx = -1

    if lcase(playtype) = "shuffle" then
        dim as integer currseq = listrec.listseqh(currentitem)
        dim as integer maxseq = 0
        dim as integer targetseq = 0

        ' get maximum sequence (for wrap)
        for i = 0 to listnr
            if listrec.listtype(i) = listtype then
                if listrec.listseqh(i) > maxseq then maxseq = listrec.listseqh(i)
            end if
        next
        targetseq = currseq - 1
        if targetseq < 1 then targetseq = maxseq
        for i = 0 to listnr
            if listrec.listtype(i) = listtype and listrec.listseqh(i) = targetseq then
                previdx = i
                exit for
            end if
        next

        return previdx
    end if

    ' linear
    for i = currentitem - 1 to 0 step -1
        if listrec.listtype(i) = listtype then
            return i
        end if
    next
    ' wrap
    for i = listnr to 0 step -1
        if listrec.listtype(i) = listtype then
            return i
        end if
    next

    return -1
end function
