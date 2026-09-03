#include once "crt.bi"

' setup playlist
type lrec
    as string  listname(any)
    as string  listfile(any)
    as string  listtype(any)
    as integer listseqh(any)
    as string  listdate(any)
    as single  listsize(any)
end type
dim shared    listrec as lrec
common shared listnr  as integer
listnr = 0

function date2unixtime(stamp as string, current as boolean = false) as long
    dim as integer uyear, umonth, uday, uhour, uminute, usecond

    uday    = valint(mid(stamp, 1,  2))
    umonth  = valint(mid(stamp, 4,  2))
    uyear   = valint(mid(stamp, 7,  4))
    uhour   = valint(mid(stamp, 12, 2))
    uminute = valint(mid(stamp, 15, 2))
    usecond = valint(mid(stamp, 18, 2))

    if current then
        return time_(null)
    else
        return (uyear - 1970) * 31536000 + (umonth - 1) * 2592000 + (uday - 1) * 86400 + uhour * 3600 + uminute * 60 + usecond
    end if
end function

sub sortswap(i as integer, j as integer)
    dim as string temp_name, temp_date, temp_file, temp_type
    dim as single temp_size, temp_seqh

    temp_name = listrec.listname(i)
    listrec.listname(i) = listrec.listname(j)
    listrec.listname(j) = temp_name

    temp_file = listrec.listfile(i)
    listrec.listfile(i) = listrec.listfile(j)
    listrec.listfile(j) = temp_file

    temp_type = listrec.listtype(i)
    listrec.listtype(i) = listrec.listtype(j)
    listrec.listtype(j) = temp_type

    temp_seqh = listrec.listseqh(i)
    listrec.listseqh(i) = listrec.listseqh(j)
    listrec.listseqh(j) = temp_seqh

    temp_date = listrec.listdate(i)
    listrec.listdate(i) = listrec.listdate(j)
    listrec.listdate(j) = temp_date

    temp_size = listrec.listsize(i)
    listrec.listsize(i) = listrec.listsize(j)
    listrec.listsize(j) = temp_size
end sub


function sortlst(sortby as string) as boolean
    dim as integer i, j
    dim as long   temp_date1, temp_date2
    for i = 1 to ubound(listrec.listfile)
        for j = 1 to ubound(listrec.listfile)
            select case sortby
            case "n" 'name
                if listrec.listname(j) < listrec.listname(i) then
                    sortswap(i, j)
                end if
            case "f" 'file
                if listrec.listfile(j) < listrec.listfile(i) then
                    sortswap(i, j)
                end if
            case "d" 'date
                temp_date1 = date2unixtime(listrec.listdate(i))
                temp_date2 = date2unixtime(listrec.listdate(j))
                if temp_date2 < temp_date1 then
                    sortswap(i, j)
                end if
            case "s" 'size
                if listrec.listsize(j) < listrec.listsize(i) then
                    sortswap(i, j)
                end if
            case "nd" 'descending varitions
                if listrec.listname(j) > listrec.listname(i) then
                    sortswap(i, j)
                end if
            case "fd"
                if listrec.listfile(j) > listrec.listfile(i) then
                    sortswap(i, j)
                end if
            case "dd"
                temp_date1 = date2unixtime(listrec.listdate(i))
                temp_date2 = date2unixtime(listrec.listdate(j))
                if temp_date2 > temp_date1 then
                    sortswap(i, j)
                end if
            case "sd"
                if listrec.listsize(j) > listrec.listsize(i) then
                    sortswap(i, j)
                end if
            end select
        next j
    next i
    return true
end function

/'
function sortlst(sortby as string) as boolean
    dim as integer i, n
    dim as long temp_date1, temp_date2
    dim as boolean descending = (right(sortby, 1) = "d")
    dim as string sorttype = left(sortby, 1)

    n = ubound(listrec.listfile)

    'Insertion sort - single pass through array
    for i = 2 to n
        dim as integer j = i - 1
        dim as boolean needsswap = false

        select case sorttype
        case "n"
            needsswap = iif(descending, listrec.listname(j) > listrec.listname(i), _
                                       listrec.listname(j) < listrec.listname(i))
        case "f"
            needsswap = iif(descending, listrec.listfile(j) > listrec.listfile(i), _
                                       listrec.listfile(j) < listrec.listfile(i))
        case "d"
            temp_date1 = date2unixtime(listrec.listdate(j))
            temp_date2 = date2unixtime(listrec.listdate(i))
            needsswap = iif(descending, temp_date1 > temp_date2, temp_date1 < temp_date2)
        case "s"
            needsswap = iif(descending, listrec.listsize(j) > listrec.listsize(i), _
                                       listrec.listsize(j) < listrec.listsize(i))
        end select

        if needsswap then sortswap(j, i)
    next i

    return true
end function
'/

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

    if chdir(folder) <> 0 then
        chdir(dummy)
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
                    redim preserve listrec.listdate(0 to listnr)
                    redim preserve listrec.listsize(0 to listnr)
                    listrec.listname(listnr) = file
                    listrec.listfile(listnr) = path(i) & file
                    listrec.listtype(listnr) = listtype
                    listrec.listseqh(listnr) = 0
                    listrec.listdate(listnr) = format(FileDateTime(path(i) & file), "dd-mm-yyyy hh:mm")
                    listrec.listsize(listnr) = filelen(path(i) & file)
                    maxfiles += 1
                else
                    logentry("warning", "file format not supported - " + path(i) & file)
                end if    
            end if
            file = dir(@attrib)
        wend
        i += 1
    wend

    ' sort natural order descending
    sortlst("fd")
    /' debug
    print listtype
    print "items " & ubound(listrec.listfile)
    ioffset = 0
    for yy as integer = 1 to ubound(listrec.listfile)
        if len(listrec.listname(yy)) > ioffset then
            ioffset = len(listrec.listname(yy))
        end if
        print listrec.listfile(yy);space((ioffset + 5) - Len(listrec.listfile(yy)));listrec.listdate(yy);space(20 - Len(listrec.listdate(yy)));listrec.listsize(yy)
    next yy
    '/

    return maxfiles
end function

function getcurrentlistitem(listtype as string, filename as string) as integer
    dim itemnr as integer = -1

    for i as integer = 0 to listnr
        with listrec
            itemnr += 1
            if listrec.listtype(i) = listtype and listrec.listfile(i) = filename then
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
