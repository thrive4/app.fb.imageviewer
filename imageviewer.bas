' TwinklebearDev SDL 2.0 Tutorial Lesson 3: SDL Extension Libraries
' Translated to FreeBASIC by Michael "h4tt3n" Schmidt Nissen, march 2017
' http://www.willusher.io/sdl2%20tutorials/2013/08/18/lesson-3-sdl-extension-libraries
' tweaked for fb and sdl2 sept 2022 by thrive4
' supported formats .bmp, .gif, .jpg, .jpeg, .mp3, .png, .pcx, .svg, .webp

#ifdef __FB_WIN32__
    #cmdline "app.rc"
#endif

#include once "SDL2/SDL.bi"
#include once "SDL2/SDL_ttf.bi"
#include once "SDL2/SDL_image.bi"
#include once "utilfile.bas"
#include once "listplay.bas"
#include once "utilaudio.bas"

' setup imageviewer
dim event           as SDL_Event
dim running         as boolean = True
dim screenwidth     as integer = 1280
dim screenheight    as integer = 720
dim fullscreen      as boolean = false
dim fps             as ulong   = 30
dim fpscurrent      as ulong   = 0
dim desktopw        as integer
dim desktoph        as integer
dim desktopr        as integer
dim locale          as string  = "en"
dim appversion		as string  = "1.0"
dim launch 			as string  = "image"
dim showimagename   as boolean = false
dim showhelp        as boolean = false
dim showmetric      as boolean = false
dim helptext        as string  = ""
dim metrictext      as string  = ""
dim infopanel 		as sdl_rect

dim shared as string filename
filename                       = ""
Dim shared As SDL_Texture  Ptr background_surface = 0
Dim shared As SDL_Renderer Ptr renderer           = 0
Dim shared As SDL_Window   Ptr glass, glglass
dim shared as SDL_RendererFlip rotateimage        = SDL_FLIP_NONE
dim shared as double rotateangle                  = 0
Dim shared As SDL_GLContext glContext
'zoomtype options stretch, scaled, zoomsmallimage
dim shared as string zoomtype
zoomtype = "zoomsmallimage"
dim shared dummy    as string
dim shared mp3chk   as boolean
dim shared mp3file  as string
dummy  = ""
mp3chk = false

' get desktop info
ScreenInfo desktopw, desktoph,,,desktopr

' setup timer used as interval between showing next image in microseconds
dim inittime    as integer = 0
dim interval    as integer = fps * 100 '3000
dim currenttime as integer
' setup timer used by effects
dim fxinittime  as integer = 0
dim menurefresh as integer = 25000

' setup list of images for background
dim fileext      as string = ""
dim mediafolder  as string
dim filetypes	 as string = ".bmp, .gif, .gls, .jpg, .jpeg, .mp3, .png, .pcx, .svg, .webp" 
dim playtype     as string = "linear"
dim currentitem  as integer
dim maxitemslist as integer
dim listtype     as string = "image"

' surfaces needed for adding alpha
' sdl allocates memory per step SDL_SetSurfaceAlphaMod, SDL_ConvertSurfaceFormat
' using the same surface leads to a memeory leak.... 
Dim As SDL_Surface Ptr dsurf
Dim As SDL_Surface Ptr esurf
Dim As long imagew, imageh, iW, iH

' screensaver
dim screensaveinterval as integer = 30 * 1000 ' in seconds
dim screensaveactive   as boolean = false
dim screensaveinittime as integer = 0
dim screensavetype     as string  = "dimscreen"
dim fadetime           as single  = ((screensaveinterval / 1000) * 1.25) / (screensaveinterval / 1000)
dim fade               as integer = 255

' effects
dim effectpan   as string  = "left2right"
dim effectzoom  as string  = "zoomin"
dim effectfade  as string  = "fadein"
dim fxpanrnd(1 to 5) as string
fxpanrnd(1) = "left2right"
fxpanrnd(2) = "right2left"
fxpanrnd(3) = "top2bottom"
fxpanrnd(4) = "bottom2top"
fxpanrnd(5) = "none"
dim fxzoomrnd(1 to 3) as string
fxzoomrnd(1) = "zoomin"
fxzoomrnd(2) = "zoomout"
fxzoomrnd(3) = "none"
dim fxfadernd(1 to 5) as string
fxfadernd(1) = "fadein"
fxfadernd(2) = "crossfade"
fxfadernd(3) = "none"
fxfadernd(4) = "sepiain"
fxfadernd(5) = "sepiaout"

' setup clock and date display
dim ttfmessage       as string = "ttfmessage"
Dim datetime         As Double = now()
dim dateformat       as string = "dd/mm/yyyy"
dim timeformat       as string = "hh:mm:ss"
' clockposistion options bottomleft, bottomright, topleft, topright 
dim clockposistion   as string = "bottomleft"
' options default, en, en-abrivated
dim datedisplay      as string = "default"
dim shared clockposx as integer
dim shared clockposy as integer

' force date to other langauage
dim ddatetime 				as string
dim langenday(1 to 7) 		as string
dim langenmonth(1 to 12) 	as string
' date linux and windows compatible
' returns 1-7 (1=Sunday by default)
Dim dayofweek				as Integer = Weekday(datetime)

' setup display filename
dim imagename as string
' imagenametype options file, fullpath, folder
dim imagenametype as string = "folder"

' main area for rendering
dim as SDL_Rect imagepanel, dimscreen
dim center as SDL_Point

' init zoom
dim imagex  as integer
dim imagey  as integer
dim imposx  as single
dim imposy  as single
dim scaledw as single
dim scaledh as single
dim scale   as single

' restore size no zoom
Dim oposx    as integer = imposx
Dim oposy    as integer = imposy
Dim oscaledw as integer = scaledw
Dim oscaledh as integer = scaledh

' setup the text aka texture and image with sdl
Dim As SDL_Texture Ptr temp_surface
Dim As SDL_Texture Ptr texture
SDL_SetTextureBlendMode(temp_surface, SDL_BLENDMODE_BLEND)
Dim As SDL_Color ttfcolor = (255, 255, 255, 0)
Dim As SDL_Color backgrondcolor = (1, 1, 1, 0)
Dim As SDL_Color ttffontgrey    = (185, 195, 205, 0)
Dim As SDL_Color infopanelcolor	= (33, 33, 33, 0)

Dim shared As TTF_Font Ptr ttffontdef
Dim shared As TTF_Font Ptr ttffontdefmono
Dim shared As TTF_Font Ptr ttffontclock
Dim shared As TTF_Font Ptr ttffontdate
Dim ttffont         as string = exepath + pathchar + "media" + pathchar + "gisha.ttf"
Dim ttffontmono     as string = exepath + pathchar + "media" + pathchar + "monotext.ttf"
dim ttffontsize     as integer
dim offsetfonty     as integer
dim fontsizeclock   as integer
dim fontsizedate    as integer
dim fontsizemono    as integer

' navigation default values
'dim kback as integer
' init app with config file if present conf.ini
dim itm     as string
dim inikey  as string
dim inival  as string
dim inifile as string = exepath + pathchar + "conf" + pathchar + "conf.ini"
dim f       as long
if FileExists(inifile) = false then
    logentry("error", inifile + " file does not excist")
else 
    f = readfromfile(inifile)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 and Left(itm, 1) <> "'" then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            if inival <> "" then
                select case inikey
                    case "screenwidth"
                        screenwidth = val(inival)
                    case "screenheight"
                        screenheight = val(inival)
                    case "fullscreen"
                        fullscreen = cbool(inival)
                        if fullscreen then
                            screenwidth  = desktopw
                            screenheight = desktoph
                            fullscreen = true
                        end if
                    case "locale"
                        locale = inival
                    case "usecons"
                        usecons = inival
                    case "launch"
                        launch = inival
                    case "logtype"
                        logtype = inival
                    case "mediafolder"
                        mediafolder = inival
                    case "playtype"
                        playtype = inival
                    case "imagenametype"
                        imagenametype = inival
                    case "interval"
                        interval = val(inival)
                    case "ttffont"
                        ttffont = exepath + pathchar + "media" + pathchar + inival
                    case "dateformat"
                        dateformat = inival
                    case "datedisplay"
                        datedisplay = inival
                    case "timeformat"
                        timeformat = inival
                    case "clockposistion"
                        clockposistion = inival
                    case "screensaveinterval"
                        screensaveinterval = val(inival) * 1000
                        if screensaveinterval = 0 or screensaveinterval <= 10000 then
                            logentry("notice", "screensaver interval set to lower than minimum 10 sec, forced to (default) 30 sec")                            
                            screensaveinterval = 30000
                        end if
                        fadetime = ((screensaveinterval / 1000) * 1.25) / (screensaveinterval / 1000)
                    case "screensavetype"
                        screensavetype = inival
                    case "appversion"
                        appversion = inival						
                end select
            end if
            'print inikey + " - " + inival
        end if    
    loop
    close(f)    
end if

' get date info
inifile = exepath + pathchar + "conf" + pathchar + locale + pathchar + "date.ini"
if FileExists(inifile) = false then
    logentry("error", inifile + " file does not excist")
else 
    f = readfromfile(inifile)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
            inikey = trim(mid(itm, 1, instr(1, itm, "=") - 2))
            inival = trim(mid(itm, instr(1, itm, "=") + 2, len(itm)))
            if inival <> "" then
                select case inikey
                    case "m1"
                        langenmonth(1) = inival
                    case "m2"
                        langenmonth(2) = inival
                    case "m3"
                        langenmonth(3) = inival
                    case "m4"
                        langenmonth(4) = inival
                    case "m5"
                        langenmonth(5) = inival
                    case "m6"
                        langenmonth(6) = inival
                    case "m7"
                        langenmonth(7) = inival
                    case "m8"
                        langenmonth(8) = inival
                    case "m9"
                        langenmonth(9) = inival
                    case "m10"
                        langenmonth(10) = inival
                    case "m11"
                        langenmonth(11) = inival
                    case "m12"
                        langenmonth(12) = inival
                    case "d0"
                        langenday(1) = inival
                    case "d1"
                        langenday(2) = inival
                    case "d2"
                        langenday(3) = inival
                    case "d3"
                        langenday(4) = inival
                    case "d4"
                        langenday(5) = inival
                    case "d5"
                        langenday(6) = inival
                    case "d6"
                        langenday(7) = inival
                end select
            end if
        end if    
    loop
    close(f)    
end if    
    
#ifdef __FB_LINUX__
	exeversion = appversion
#endif

' verify locale otherwise set default
select case locale
    case "en", "es", "de", "fr", "nl"
        ' nop
    case else
        logentry("error", "unsupported locale " + locale + " applying default setting")
        locale = "en"
end select

' get help text
inifile = exepath + pathchar + "conf" + pathchar + locale + pathchar + "help.ini"
if FileExists(inifile) = false then
    logentry("error", inifile + " file does not excist")
else 
    f = readfromfile(inifile)
    Do Until EOF(f)
        Line Input #f, itm
		if instr(itm, "[") then inittime += 1 end if
		if inittime > 2 then
			helptext += wstr(itm) + newline
		end if	
    loop
    close(f)    
end if
inittime = 0

function closesdlfonts() as boolean
    TTF_CloseFont(ttffontdef)
    TTF_CloseFont(ttffontdefmono)
    TTF_CloseFont(ttffontclock)
    TTF_CloseFont(ttffontdate)
    return true
end function

Sub cleanup(pathchar As String, appname as string)
    delfile(exepath + pathchar + "thumb.jpg")
    delfile(exepath + pathchar + "thumb.png")
	'cleanup sdl
	SDL_DestroyTexture(background_surface) : background_surface = 0
	SDL_DestroyRenderer(renderer)
	SDL_DestroyWindow(glass) : glass = 0
	SDL_GL_DeleteContext(glContext)
	SDL_DestroyWindow(glglass) : glglass = 0
    closesdlfonts()
	IMG_Quit()
    TTF_Quit()
	SDL_Quit()
	close
	logentry("terminate", "normal termination " + appname)
End Sub

' parse commandline
select case command(1)
    case "/?", "-h", "-help", "--help", "-man"
        displayhelp(locale)
		cleanup(pathchar, appname)
    case "-v", "-ver"
        consoleprint appname + " version " & exeversion 
		cleanup(pathchar, appname)
end select

' get media
if len(command(1)) > 0 then
	dummy = resolvepath(command(1))
else
	dummy = ""
end if

dim as integer ioffset = 0
if instr(dummy, ".m3u") = 0 and instr(dummy, ".pls") = 0 and instr(dummy, "http") = 0 then
    if instr(dummy, ".") <> 0 and instr(dummy, "..") = 0 then
		if fileexists(dummy) = 0 then
				logentry("fatal", "error: file not found " & dummy)
		end if		
        fileext = lcase(mid(dummy, instrrev(dummy, ".")))
        if instr(1, filetypes, fileext) = 0 then
            logentry("fatal", dummy + " file type not supported")
        end if
        mediafolder = left(dummy, instrrev(dummy, pathchar))
        createlist(mediafolder, filetypes, listtype)
    else
        ' specific path
        if instr(dummy, pathchar) <> 0  then
            mediafolder = dummy
            if checkpath(mediafolder) = false then
                logentry("fatal",  "error: path not found " + mediafolder)
            else
                if createlist(mediafolder, filetypes, listtype) = 0 then
                    logentry("fatal", "error: no playable files found")
                end if
            end if
        else
            ' fall back to path mediafolder specified in conf.ini
            if checkpath(mediafolder) = false then
                logentry("error", "error: mediafolder path " + mediafolder + " not found in conf.ini ")
                ' try scanning exe path
                mediafolder = exepath
            end if
            if createlist(mediafolder, filetypes, listtype) = 0 then
                logentry("fatal", "error: no playable files found")
            end if
        end if
    end if
end if

' parse commandline for optional switches
dim i as integer
for i = 0 To __FB_ARGC__ - 1
	if i > 1 then
		select case lcase(command(i))
			case "slideshow"
				launch = command(i)
			case "fullscreen"
				screenwidth  = desktopw
				screenheight = desktoph
				fullscreen = true			
		end select
	end if	
next

' use .m3u as image coverart mp3s
if instr(dummy, ".m3u") <> 0 then
    if FileExists(dummy) then
        'nop
    else
        logentry("fatal", dummy + " file does not excist or possibly use full path to file")
    end if
    listnr = getmp3playlist(dummy, listtype)
    logentry("notice", "parsing and playing playlist " + filename)
end if

' search with query and export .m3u 
if instr(command(2), ".m3u") = 0 and instr(command(2), ".pls") = 0 then
	if (left(dummy, 1) = "/" or instr(dummy, ":") <> 0) and len(command(2)) <> 0 then
		select case command(2)
			case "artist"
			case "title"
			case "album"
			case "year"
			case "genre"
			case "fullscreen"
				chk = true
			case "slideshow"
				chk = true
			case else
				logentry("fatal", "unknown tag '" & command(2) & "' valid tags artist, title, album, genre and year")
		end select		
		' scan and search nr results overwritten by getmp3playlist
		if chk then
			' nop
		else	
			listnr = exportm3u(dummy, "*.mp3", "m3u", "exif", command(2), command(3))
			if listnr < 2 then
				logentry("fatal", "no matches found for " + command(3) + " in " + command(2))
			else
				listnr = getmp3playlist(exepath + pathchar + command(3) + ".m3u", listtype)
			end if
		end if
	end if
end if
dummy = ""

' toggle main loop to opengl shader if .gls file
' todo needs beter place
#include once "shadertoy.bas"
dim shared as boolean glrunning = false

' check and get mp3 cover art
sub checkmp3cover(byref filename as string)
	if instr(filename, ".mp3") > 0  then
		if getmp3cover(filename) then
			mp3file  = filename
			mp3chk = true
			if FileExists(exepath +  pathchar + "thumb.jpg") then
				filename = exepath + pathchar + "thumb.jpg"
			else
				filename = exepath + pathchar + "thumb.png"
			end if
		else
			mp3file  = ""
			mp3chk = false
		end if
	end if	
end sub

' get next or previous image
sub playmedia(byval index as integer)
    dim as string entry = listrec.listfile(index)

    filename = entry
    if glrunning = false then
        checkmp3cover(filename)
        ' validate if false get next image
        if filename = "" or FileExists(filename) = false then
            filename = listrec.listfile(index + 1)
            checkmp3cover(filename)
        end if
        SDL_DestroyTexture(background_surface) : background_surface = 0
        background_surface = IMG_LoadTexture(renderer, filename)
        ' reset rotation
        rotateangle = 0
        zoomtype = "zoomsmallimage"
    end if
end sub

' scale and posisition image scale needs to be a float
function resizebyaspectratio(screenw as integer, screenh as integer, imagew as integer, imageh as integer) as single
    dim screenar as single = screenw / screenh
    dim imagear  as single = imagew / imageh

    dim scale as single = 0
    if (screenar > imagear) then
        scale = screenh / imageh
    else
        scale = screenw / imagew
    end if
    return scale
end function

function scaledfit(screenw as integer, screenh as integer,_
    imagew as integer, imageh as integer,_
    ByRef scaledw As single, ByRef scaledh As single,_
    byref posx as integer, byref posy as integer) as boolean
    
    ' pending on size of scaled image and window size recalculate posx, posy 
    dim scale as single = 1
    if imagew > screenw or imageh > screenh then
        posx = 0
        posy = 0
        scale = resizebyaspectratio(screenw, screenh, imagew, imageh)
    end if    
    ' round scale rendertexture works with integers
    scaledw = abs(scale * imagew)
    scaledh = abs(scale * imageh)
    if scaledw < screenw then
        posx = screenw  / 2
        posx = posx - (scaledw / 2)
    end if
    if scaledh < screenh then
        posy = screenh / 2
        posy = posy - (scaledh / 2)
    end if
    
    return true
end function

Sub renderTexture(  ByVal tex As SDL_Texture Ptr, _
	                ByVal ren As SDL_Renderer Ptr, _ 
	                Byval x   As Integer, _
	                Byval y   As Integer, _
	                Byval r   As double, _ 		   ' rotate in degrees
	                Byval c   As SDL_Point ptr, _  ' the point around which dstrect will be rotated
	                Byval f   As SDL_RendererFlip) ' flip SDL_FLIP_NONE, SDL_FLIP_HORIZONTAL, SDL_FLIP_VERTICAL
	
    if tex <> null then	
        Dim As long w, h
        Dim As SDL_Rect dst
        SDL_QueryTexture(tex, NULL, NULL, @w, @h)
        dst.x = x
        dst.y = y
        dst.w = w
        dst.h = h
        SDL_RenderCopyEx(ren, tex, NULL, @dst, r, c, f)
        SDL_DestroyTexture(tex) : tex = 0 ' todo check this	
    end if
End Sub

Function renderText( ByRef message  As Const String, _
                     Byval ttffont  As TTF_Font ptr, _
                     ByVal col      As SDL_Color, _
                     ByVal wrap     As integer, _
                     ByVal renderer As SDL_Renderer Ptr ) As SDL_Texture Ptr
    if message <> "" then
        if (ttffontdef = NULL) Then
            Return NULL
        End If
        ' load surface into a texture
        Dim As SDL_Surface Ptr surf
        surf = TTF_RenderUTF8_Blended_Wrapped(ttffont, message, col, wrap)
        if (surf = NULL) Then 
            TTF_CloseFont(ttffontdef)
            Return NULL
        End If
        Dim As SDL_Texture Ptr txture = SDL_CreateTextureFromSurface(renderer, surf)
        if (txture = NULL) Then
            Return NULL
        EndIf
        ' clean up
        SDL_FreeSurface(surf)
        return txture
    else
        return null
    end if

End Function

' set active media item
if mid(command(1), 3, 1) = pathchar then
    ' relative path
    currentitem = listnext(listtype, playtype, 0)
else
	if instr(command(1), ".") > 0 and instr(command(1), ".m3u") = 0 and instr(command(1), ".pls") = 0 then
		currentitem = getcurrentlistitem(listtype, command(1))
	else
		currentitem = listnext(listtype, playtype, 0)
	end if
end if
maxitemslist = getmaxitemslist(listtype)
setsequence(currentitem)
if lcase(playtype) = "linear" then
    clearseq(listtype)
end if

' init window and render
SDL_SetHint(SDL_HINT_VIDEO_ALLOW_SCREENSAVER, "1")
' respond to power plan settings blank display on windows set hint before sdl init video
If (SDL_Init(SDL_INIT_VIDEO) = not NULL) Then
    SDL_Quit()
    logentry("fatal", "sdl2 video could not be initlized error: " + *SDL_GetError())
else
    ' disable specific subsytems sdl
    SDL_QuitSubSystem(SDL_INIT_AUDIO)
    SDL_QuitSubSystem(SDL_INIT_HAPTIC)
    ' filter non used events
    SDL_EventState(SDL_FINGERMOTION,    SDL_IGNORE)
    SDL_EventState(SDL_FINGERDOWN,      SDL_IGNORE)
    SDL_EventState(SDL_FINGERUP,        SDL_IGNORE)
    SDL_EventState(SDL_MULTIGESTURE,    SDL_IGNORE)
    SDL_EventState(SDL_DOLLARGESTURE,   SDL_IGNORE)
    SDL_EventState(SDL_JOYBALLMOTION,   SDL_IGNORE)
    SDL_EventState(SDL_DROPFILE,        SDL_IGNORE)
    ' render scale quality: 0 point, 1 linear, 2 anisotropic
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1") 
End If

initsdl:
' rescale fonts to screensize
ttffontsize     = fix(screenheight / 100 * 3) 
offsetfonty     = fix(ttffontsize / (screenheight / 500))
fontsizeclock   = 10 + ttffontsize
fontsizedate    = fix(0.9 * ttffontsize)
fontsizemono	= fix(0.8 * ttffontsize)

' setup regular sdl glass aka window
if fullscreen then
    SDL_ShowCursor(SDL_DISABLE)
#ifdef __FB_WIN32__
    glass = SDL_CreateWindow( "imageviewer", null, null, screenwidth, screenheight, SDL_WINDOW_BORDERLESS)
#else
	glass = SDL_CreateWindow( "imageviewer", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, screenwidth, screenheight, SDL_WINDOW_BORDERLESS or SDL_WINDOW_FULLSCREEN_DESKTOP)
#endif
else
    SDL_ShowCursor(SDL_ENABLE)
    ' note need to set x,y window cordinate on windows otherwise the window is boderless
    glass = SDL_CreateWindow( "imageviewer", 100, 100, screenwidth, screenheight, SDL_WINDOW_RESIZABLE)
end if
if (glass = NULL) Then
	SDL_Quit()
    logentry("fatal", "abnormal termination sdl2 could not create window")
EndIf
renderer = SDL_CreateRenderer(glass, -1, SDL_RENDERER_ACCELERATED Or SDL_RENDERER_PRESENTVSYNC)

if (renderer = NULL) Then	
	SDL_Quit()
    logentry("fatal", "abnormal termination sdl2 could not create renderer")
EndIf

' init SDL_ttf
if (TTF_Init() = Not 0) Then 
    SDL_Quit()
    logentry("fatal", "sdl2 ttf could not be initlized error: " + *SDL_GetError())
EndIf
ttffontdef      = TTF_OpenFont(ttffont, ttffontsize)
ttffontclock    = TTF_OpenFont(ttffont, fontsizeclock)
ttffontdate     = TTF_OpenFont(ttffont, fontsizedate)
ttffontdefmono  = TTF_OpenFont(ttffontmono, fontsizemono)

' setup dim screen dimensions
dimscreen.x = 0
dimscreen.y = 0
dimscreen.w = screenwidth
dimscreen.h = screenheight

imagepanel.x = 0
imagepanel.y = 0
imagepanel.w = screenwidth
imagepanel.h = screenheight

' todo remove just in case, background on launch used to be here

' gamepad
dim deadzone as integer = 8192
Dim As SDL_GameController Ptr controller = NULL
If (SDL_Init(SDL_INIT_GAMECONTROLLER) = not NULL) Then 
    logentry("error", "sdl2 gamecontroller could not be initlized error: " + *SDL_GetError())
End If
controller = SDL_GameControllerOpen(0)
If (controller = NULL) Then
    logentry("error", "unable to open gamepad - sdl error: " & *SDL_GetError())
else
    SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, "0")
    logentry("notice", "gamepad detected " & *SDL_GameControllerName(controller))
end if

' possible fix for unrecognized gamepad https://github.com/gabomdq/SDL_GameControllerDB
'SDL_GameControllerAddMappingsFromFile("gamecontrollerdb.txt")
Dim As ZString Ptr map = SDL_GameControllerMapping(controller)

' gamepad map debug
'Print *SDL_GameControllerName(controller)
'print *map
'sleep 3000

' play first item
playmedia(currentitem)

if instr(1, filename, ".gls") > 0 then
	running      = false
	glrunning    = true
	glfullscreen = false
	shader.CompileFile(filename)
	inittime = currenttime
	launch = "shader " + launch
else
	SDL_HideWindow(glglass)
end if

if glrunning = false then
    SDL_DestroyTexture(background_surface) : background_surface = 0
    background_surface = IMG_LoadTexture(renderer, filename)
    ' verify load image
    if ( background_surface = NULL ) Then
        'cleanup(background, image, renderer, window)
        IMG_Quit()
        SDL_Quit()
        logentry("fatal", "abnormal termination sdl2 could not create texture")
    End If
end if

' work around to init first image regular sdl
if glrunning = false then
    ' tricky todo check this used to show first image without delay in imagepanel
    inittime = SDL_GetTicks() - interval
    ' todo check placement screen
    select case clockposistion
        case "bottomleft" 
            ' display clock in bottom corner left
            clockposx = 30
            clockposy = screenheight - fontsizeclock * 3.5f
        case "bottomright" 
            ' display clock in bottom corner right
            clockposx = screenwidth  - fontsizeclock * 6.0f
            clockposy = screenheight - fontsizeclock * 3.5f
        case "topleft" 
            ' display clock in top corner left
            clockposx = fontsizeclock
            clockposy = fontsizeclock
        case "topright" 
            ' display clock in top corner right
            clockposx = screenwidth  - fontsizeclock * 6.0f
            clockposy = fontsizeclock
    end select
end if ' end glrunning false

' background on launch
SDL_RenderClear(renderer)
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE)
    SDL_SetRenderDrawColor(renderer, backgrondcolor.r, backgrondcolor.g, backgrondcolor.b, backgrondcolor.a)
    SDL_RenderFillRect(renderer, @imagepanel)
    texture = renderText(ucase(launch), ttffontclock, ttffontgrey, 0, renderer)
    SDL_QueryTexture(texture, NULL, NULL, @iW, @iH)
    renderTexture(texture, renderer, screenwidth * 0.5f - iW * 0.5f, screenheight * 0.5f, 0, null, SDL_FLIP_NONE)
    SDL_DestroyTexture(texture) : texture = 0
SDL_RenderPresent(renderer)
sdl_delay(900)

' main shadertoy sdl
While glrunning
' todo phase out wayland hack
#ifdef __FB_WIN32__
    SDL_RaiseWindow(glglass)
#endif
    While SDL_PollEvent(@event)
        select case event.type
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_ESCAPE
                SDL_GL_DeleteContext(glContext)
                SDL_DestroyWindow(glglass) : glglass = 0
                glrunning = False
                running = false
				exit while
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_CLOSE
                SDL_GL_DeleteContext(glContext)
                SDL_DestroyWindow(glglass) : glglass = 0
                glrunning = false
                running   = false
				exit while
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_MINIMIZED
                SDL_HideWindow(glglass)
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_RESTORED
                SDL_ShowWindow(glglass)
				SDL_RaiseWindow(glglass)

/' slows down rendering shader				
' Get current window size
Dim w As long, h As long
' Get current window position (may return 0,0 on Wayland, but worth trying)
Dim x As long, y As long
SDL_GetWindowSize(glglass, @w, @h)
SDL_GetWindowPosition(glglass, @x, @y)
print w
print h
print x
print y
'/
' todo phase out wayland hack
#ifdef __FB_WIN32__
            ' keep gl window in place relative to regular sdl window
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_MOVED
                SDL_GetWindowPosition(glass, @w2, @h2)
                sdl_setwindowposition(glglass, w2, h2)
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_RESIZED
                SDL_GetWindowPosition(glass, @w2, @h2)
                sdl_setwindowposition(glglass, w2, h2)
#endif
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_F11
                SDL_GL_DeleteContext(glContext)
                SDL_DestroyRenderer(renderer)
                SDL_DestroyWindow(glass) : glass = 0
                SDL_DestroyWindow(glglass) : glglass = 0
                select case fullscreen
                    case true
                        ' enable or disable mouse cursor in window
                        screenwidth  = 1280
                        screenheight = 720
                        fullscreen = false
                        goto initgl
                    case false
                        screenwidth  = desktopw
                        screenheight = desktoph
                        fullscreen = true
                        sdl_setwindowposition(glglass, 0, 0)
                        goto initgl
                end select
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_RIGHT
                ' get next shader in folder if avaiable
                currentitem = listnext(listtype, playtype, currentitem)
                if playtype = "shuffle" then
                    setsequence(currentitem)
                end if
                playmedia(currentitem)
                if shader.CompileFile(filename) = false then
                    logentry("error", "error compiling " & filename)
                end if
                inittime = currenttime
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_LEFT
                ' get previous shader in folder if avaiable
                currentitem = listprevious(listtype, playtype, currentitem)
                playmedia(currentitem)
                if shader.CompileFile(filename) = false then
                    logentry("error", "error compiling " & filename)
                end if
                inittime = currenttime
        end select
    Wend

    ' timer
    currenttime = SDL_GetTicks()
    if (currenttime > inittime + interval * 2) and launch = "shader slideshow" then
        ' get next shader in folder if avaiable
        currentitem = listnext(listtype, playtype, currentitem)
        if playtype = "shuffle" then
            setsequence(currentitem)
        end if
        playmedia(currentitem)
        ' todo needs better handeling funky behaivour
        if shader.CompileFile(filename) = false then
            logentry("error", "error compiling " & filename)
        end if
        inittime = currenttime
    end if

	' init shader posistion
	v3.x = screenwidth
	v3.y = screenheight
	v3.z = v3.x/v3.y

    ' enable shader
    glUseProgram(Shader.ProgramObject)
    tNow = Timer()

    ' get uniforms locations in shader program
    var iGlobalTime = glGetUniformLocation(Shader.ProgramObject,"iGlobalTime")
    var iTime       = glGetUniformLocation(Shader.ProgramObject,"iTime")
    var iResolution = glGetUniformLocation(Shader.ProgramObject,"iResolution")
    var iMouse      = glGetUniformLocation(Shader.ProgramObject,"iMouse")
    var iDate       = glGetUniformLocation(Shader.ProgramObject,"iDate")
    glUniform3f(iResolution, v3.x, v3.y, v3.z)
    glUniform4f(idate, year(now), month(now), day(now), (hour(now) * 60 * 60) + (minute(now) * 60) + second(now) + (epoch - fix(epoch)))
    glUniform1f(iGlobalTime, tNow - tStart)
    glUniform1f(iTime, tNow - tStart)
    glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT)
    glRectf (-1.0, -1.0, 1.0, 1.0)

    ' update screen
    SDL_GL_SwapWindow(glglass)
' todo phase out wayland hack
' needed to render main glsdl window
#ifdef __FB_LINUX__
    SDL_RenderClear(renderer)
        boundbox.x = 0
        boundbox.y = 0
        boundbox.w = screenwidth
        boundbox.h = screenheight
        SDL_RenderDrawRect(renderer, @boundbox)
        SDL_RenderFillRect(renderer, @boundbox)
    SDL_RenderPresent(renderer)
#endif

    SDL_SetWindowTitle(glass, "shadertoy sdl2 file: " & filename)
' todo phase out wayland hack
' reduce cpu usage affects shader animation
' todo figure out why animation slows down too much on wayland
#ifdef __FB_WIN32__
	SDL_Delay(25)
#endif
Wend

' main
while running
    ' screen dimmer / saver timer in microseconds
    currenttime = SDL_GetTicks()

    while SDL_PollEvent(@event) <> 0
        ' basic interaction
        select case event.type
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_ESCAPE
				running = False
				exit while
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_CLOSE
                running = False
				exit while
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_F1
                if showhelp then showhelp = false else showhelp = true end if
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_F11
                SDL_DestroyTexture(background_surface) : background_surface = 0
                SDL_DestroyRenderer(renderer)
                SDL_DestroyWindow(glass) : glass = 0
                select case fullscreen
                    case true
                        screenwidth  = 1280
                        screenheight = 720
                        fullscreen = false
                        ' reset rotation and zoomtype
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                        goto initsdl
                    case false
                        screenwidth  = desktopw
                        screenheight = desktoph
                        fullscreen = true
                        ' reset rotation and zoomtype
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                        goto initsdl
                end select
            ' zoom manual
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_PLUS
                zoomtype = "zoomin"
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_MINUS
                zoomtype = "zoomout"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_PERIOD
                zoomtype = "zoomsmallimage"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_Z
                select case zoomtype
                    case "scaled"
                        zoomtype = "zoomsmallimage"
                    case "zoomsmallimage"
                        zoomtype = "stretch"
                    case "stretch"
                        zoomtype = "scaled"
                end select
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_SPACE
                ' reset rotation and zoomtype
                rotateangle = 0
                zoomtype = "zoomsmallimage"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_I
                if showimagename then showimagename = false else showimagename = true end if
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_M
                if showmetric then showmetric = false else showmetric = true end if
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_RIGHT
                ' get next image in folder if avaiable
                currentitem = listnext(listtype, playtype, currentitem)
                if playtype = "shuffle" then
                    setsequence(currentitem)
                end if
                playmedia(currentitem)
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_LEFT
                ' get previous image in folder if avaiable
                currentitem = listprevious(listtype, playtype, currentitem)
                playmedia(currentitem)
            ' rotate clockwise
            case SDL_KEYDOWN and (event.key.keysym.sym = SDLK_R or event.key.keysym.sym = SDLK_RETURN)
                if rotateangle > -270 then
                rotateangle = rotateangle - 90
                else
                 rotateangle = 0
                end if
            ' navigation mouse
            case SDL_MOUSEWHEEL
                'scroll up
                if event.wheel.y > 0 then
                    zoomtype = "zoomin"
                'scroll down
                elseif event.wheel.y < 0 then
                    zoomtype = "zoomout"
                end if
                'scroll right
                if event.wheel.x > 0 then
                    'nop
                ' scroll left
                elseif event.wheel.x < 0 then
                    'nop
                end if
            case SDL_MOUSEBUTTONDOWN
                ' button
                select case event.button.button
                    case SDL_BUTTON_LEFT
                        ' get next image in folder if avaiable
                        currentitem = listnext(listtype, playtype, currentitem)
                        if playtype = "shuffle" then
                            setsequence(currentitem)
                        end if
                        playmedia(currentitem)
                    case SDL_BUTTON_MIDDLE
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                    case SDL_BUTTON_RIGHT
                        ' get previous image in folder if avaiable
                        currentitem = listprevious(listtype, playtype, currentitem)
                        playmedia(currentitem)
                end select
            case SDL_MOUSEBUTTONUP
                'nop
            ' navigation gamepad dpad and A button
            case SDL_CONTROLLERBUTTONUP
                select case event.cbutton.button    
                    case SDL_CONTROLLER_BUTTON_DPAD_LEFT
                        ' get previous image in folder if avaiable
                        currentitem = listprevious(listtype, playtype, currentitem)
                        playmedia(currentitem)
                    case SDL_CONTROLLER_BUTTON_DPAD_RIGHT
                        ' get next image in folder if avaiable
                        currentitem = listnext(listtype, playtype, currentitem)
                        if playtype = "shuffle" then
                            setsequence(currentitem)
                        end if
                        playmedia(currentitem)
                    case SDL_CONTROLLER_BUTTON_DPAD_DOWN
                        zoomtype = "zoomout"
                    case SDL_CONTROLLER_BUTTON_DPAD_UP
                        zoomtype = "zoomin"
                    case SDL_CONTROLLER_BUTTON_A
                        zoomtype = "zoomsmallimage"
                        rotateangle = 0
                    case SDL_CONTROLLER_BUTTON_B
                        if rotateangle > -270 then
                            rotateangle = rotateangle - 90
                        else
                            rotateangle = 0
                        end if
                    case SDL_CONTROLLER_BUTTON_X
						' toggle imagename display
						if showimagename then showimagename = false else showimagename = true end if
                    case SDL_CONTROLLER_BUTTON_Y
                        select case zoomtype
                            case "scaled"
                                zoomtype = "zoomsmallimage"
                            case "zoomsmallimage"
                                zoomtype = "stretch"
                            case "stretch"
                                zoomtype = "scaled"
                        end select
                    case SDL_CONTROLLER_BUTTON_START
						if showhelp then showhelp = false else showhelp = true end if
                    case SDL_CONTROLLER_BUTTON_BACK
						if showmetric then showmetric = false else showmetric = true end if
                end select
            case SDL_CONTROLLERAXISMOTION
                if event.caxis.value > deadzone or event.caxis.value < -deadzone then
                '    axisinput(event.caxis.axis, event.caxis.value, mousespeed)
                    select case event.caxis.axis
                        case SDL_CONTROLLER_AXIS_TRIGGERRIGHT
                            zoomtype = "zoomin"
                        case SDL_CONTROLLER_AXIS_TRIGGERLEFT
                            zoomtype = "zoomout"
                    end select
                end if
            case SDL_CONTROLLERDEVICEADDED
                SDL_free(map)
                SDL_GameControllerClose(controller)
                controller = SDL_GameControllerOpen(0)
                logentry("notice", "switched to game controller: " & *SDL_GameControllerName(controller))
                map = SDL_GameControllerMapping(controller)
        end select		
    wend
select case launch
case "image"
    ' scaling image
    SDL_QueryTexture(background_surface, NULL, NULL, @iW, @iH)
	' reset background after help or metrics
	SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE)
	SDL_SetRenderDrawColor(renderer, backgrondcolor.r, backgrondcolor.g, backgrondcolor.b, backgrondcolor.a)
    select case zoomtype
        ' scale image
        case "scaled"
            chk = scaledfit(screenwidth, screenheight, iW, iH, scaledw, scaledh, imagex, imagey)
            imposx = (screenwidth * 0.5f) - iW * 0.5
            imposy = (screenheight * 0.5f) - iH * 0.5
            scaledw = iW
            scaledh = iH
        case "zoomsmallimage"
            scale = resizebyaspectratio(screenwidth, screenheight, iW, iH)
            imposx = (screenwidth * 0.5f) - abs(scale * iW) * 0.5
            imposy = (screenheight * 0.5f) - abs(scale * iH) * 0.5
            scaledw = scale * iW
            scaledh = scale * iH
        case "stretch"
            imposx = 0
            imposy = 0
            scaledw = screenwidth
            scaledh = screenheight
        ' used for manual resize image
        ' todo split from plus and minus and restore orginal zoom setting check bounds
        case "zoomout"
            scaledw = scaledw - 5 * (iW / iH)
            scaledh = scaledh - 5
            imposx = imposx + 2.5 * (iW / iH)
            imposy = imposy + 2.5
            zoomtype = ""
        case "zoomin"
            scaledw = scaledw + 5 * (iW / iH)
            scaledh = scaledh + 5
            imposx = imposx - 2.5 * (iW / iH)
            imposy = imposy - 2.5
            zoomtype = ""
    end select

    SDL_RenderClear(renderer)
        'display image
        imagepanel.x = imposx
        imagepanel.y = imposy
        imagepanel.w = scaledw
        imagepanel.h = scaledh
        SDL_RenderCopyEx(renderer, background_surface, null, @imagepanel, rotateangle, null, rotateimage)
        ' screen dimming or screensaver
        if (currenttime > screensaveinittime + screensaveinterval) then
            select case screensavetype
                case "dimscreen"
                    ' fade to black
                    if fade > 0 then
                        SDL_SetTextureColorMod(background_surface, fade, fade, fade)
                        fade -= fadetime
                    end if
                    screensaveactive = true
                case "displayoff"
                    ' ignore for now
                    'SendMessage(HWND_BROADCAST, WM_SYSCOMMAND, SC_MONITORPOWER, 2)
            end select
        end if
		' display image name
        if showimagename then
            select case imagenametype
                case "folder"
                    dummy = left(filename, instrrev(filename, pathchar) -1)
                    imagename = mid(dummy, instrrev(dummy, pathchar) + 1)
                case "file"
                    'imagename = mid(left(filename, len(filename) - instr(filename, pathchar) -1), InStrRev(filename, pathchar) + 1, len(filename))
					imagename = mid(filename, InStrRev(filename, pathchar) + 1, InStrRev(filename, ".") - InStrRev(filename, pathchar) - 1)				
                case "fullpath"
                    imagename = filename
            end select
            ' special case mp3 file
            if mp3file <> "" then
                imagename = mid(left(mp3file, len(mp3file) - instr(mp3file, pathchar) -1), InStrRev(mp3file, pathchar) + 1, len(mp3file))
            end if

            texture = renderText(lcase(imagename), ttffontdate, ttfcolor, 0, renderer)
            renderTexture(texture, renderer, clockposx, clockposy + fontsizeclock + fontsizedate, 0, null, SDL_FLIP_NONE)
            SDL_DestroyTexture(texture) : texture = 0
        end if
        ' dsplay navigation help
        if showhelp then
			infopanel.x = 0
			infopanel.y = 0
			infopanel.w = screenwidth * 0.60f
			infopanel.h = screenheight
			SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND)
			SDL_SetRenderDrawColor(renderer, infopanelcolor.r, infopanelcolor.g, infopanelcolor.b, 120)
			SDL_RenderDrawRect(renderer, @infopanel)
			SDL_RenderFillRect(renderer, @infopanel)		
            texture = renderText(lcase(helptext), ttffontdefmono, ttfcolor, 0, renderer)
            renderTexture(texture, renderer, clockposx - 10, 10 + fontsizeclock + fontsizedate, 0, null, SDL_FLIP_NONE)
            SDL_DestroyTexture(texture) : texture = 0
        end if

      ' dsplay image metrics
        if showmetric then
			infopanel.x = 0
			infopanel.y = 0
			infopanel.w = screenwidth * 0.4f
			infopanel.h = screenheight
			SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND)
			SDL_SetRenderDrawColor(renderer, infopanelcolor.r, infopanelcolor.g, infopanelcolor.b, 120)
			SDL_RenderDrawRect(renderer, @infopanel)
			SDL_RenderFillRect(renderer, @infopanel)
			metrictext = "width  " & iW & newline _
			& "height " & iH & newline _ 
			& "type   " & getimagetype(filename) & newline _
			& "size   " & convertbytesize(listrec.listsize(currentitem)) & newline _
			& "date   " & listrec.listdate(currentitem)
		
            texture = renderText(lcase(metrictext), ttffontdefmono, ttfcolor, 0, renderer)
            renderTexture(texture, renderer, clockposx - 10, 10 + fontsizeclock + fontsizedate, 0, null, SDL_FLIP_NONE)
            SDL_DestroyTexture(texture) : texture = 0
        end if
    SDL_RenderPresent(renderer)

case "slideshow"
	datetime = Now()

    ' timer
    currenttime = SDL_GetTicks()
    if (currenttime > inittime + interval) then
        ' init effects
        fade = 0
        fxinittime = currenttime

        ' bookmark previous image for crossfade
        dummy = filename
        SDL_DestroyTexture(temp_surface) : temp_surface = 0
        ' get next image in folder if avaiable
        currentitem = listnext(listtype, playtype, currentitem)
        if playtype = "shuffle" then
            setsequence(currentitem)
        end if
        playmedia(currentitem)
        ' if image can not be loaded skip to next file
        if background_surface = null then
            currentitem = listnext(listtype, playtype, currentitem)
            if playtype = "shuffle" then
                setsequence(currentitem)
            end if
            playmedia(currentitem)
            dummy = filename
        end if

        ' add alpha for crossfade
        dsurf = IMG_Load(dummy)
        SDL_SetSurfaceAlphaMod(dsurf, 0)
        esurf = SDL_ConvertSurfaceFormat(dsurf, SDL_PIXELFORMAT_RGBA32, 0)
		if temp_surface <> null then SDL_DestroyTexture(temp_surface) : temp_surface = 0
		temp_surface = SDL_CreateTextureFromSurface(renderer, esurf)
        SDL_FreeSurface(dsurf)
        SDL_FreeSurface(esurf)

        ' scaling image
        SDL_QueryTexture(background_surface, NULL, NULL, @iW, @iH)
        select case zoomtype
            case "scaled"
                chk = scaledfit(screenwidth, screenheight, iW, iH, scaledw, scaledh, imagex, imagey)
                imagepanel.x = imagex
                imagepanel.y = imagey
                imagepanel.w = scaledw
                imagepanel.h = scaledh
            ' setup ken burns fx
            case "zoomsmallimage"
                scale = resizebyaspectratio(screenwidth, screenheight, iW, iH)
                randomize
                effectzoom = fxzoomrnd(int(rnd * 3) + 1)
                effectfade = fxfadernd(int(rnd * 5) + 1)
                if scale < 0.5 then
                    scale = 0.45f + scale
                else
                    scale = 0.35f + scale
                end if
                select case effectzoom
                    case "none"
                        'nop
                    case "zoomout"
                        scale = scale * 1.75f
                    case "zoomin"
                        'nop
                end select
                if iW < 0.8f * screenwidth then
                    imagepanel.w = 1.25f * scale * iW
                    imagepanel.h = 1.25f * scale * iH
                    effectzoom = "none"
                    effectpan = fxpanrnd(int(rnd * (5 - 3) + 3))
                else
                    imagepanel.w = scale * iW
                    imagepanel.h = scale * iH
                    effectpan = fxpanrnd(int(rnd * (3 - 1) + 1))
                end if
                ' init position
                select case effectpan
                case "left2right"
                    imagepanel.x = (screenwidth * 0.35f) - imagepanel.w * 0.5
                    imagepanel.y = (screenheight * 0.5f) - imagepanel.h * 0.5
                case "right2left"
                    imagepanel.x = (screenwidth * 0.65f) - imagepanel.w * 0.5
                    imagepanel.y = (screenheight * 0.5f) - imagepanel.h * 0.5
                case "bottom2top"
                    imagepanel.x = (screenwidth * 0.50f) - imagepanel.w * 0.5
                    imagepanel.y = (screenheight * 0.65f) - imagepanel.h * 0.5
                case "top2bottom"
                    imagepanel.x = (screenwidth * 0.50f) - imagepanel.w * 0.5
                    imagepanel.y = (screenheight * 0.35f) - imagepanel.h * 0.5
                end select
            case "stretch"
                imagepanel.x = 0
                imagepanel.y = 0
                imagepanel.w = screenwidth
                imagepanel.h = screenheight
        end select
        inittime = currenttime
    end if
'effectzoom = "none"
'effectpan = "none"
'print fadetime
'effectfade = "sepiain"
'print effectzoom
'print effectpan
'print effectfade
    ' timer effects
    if (currenttime < fxinittime + interval) then
        select case effectzoom
            case "zoomout"
                imagepanel.w = imagepanel.w - fadetime * (iW / iH)
                imagepanel.h = imagepanel.h - fadetime
                imagepanel.x = imagepanel.x + fadetime * (iW / iH)
                imagepanel.y = imagepanel.y + fadetime
            case "zoomin"
                imagepanel.w = imagepanel.w + fadetime * (iW / iH)
                imagepanel.h = imagepanel.h + fadetime
                imagepanel.x = imagepanel.x - fadetime * (iW / iH)
                imagepanel.y = imagepanel.y - fadetime
            case "none"
                'nop 
        end select
        select case effectpan
            case "left2right"
                imagepanel.x = imagepanel.x + fadetime
            case "right2left"
                imagepanel.x = imagepanel.x - fadetime
            case "top2bottom"
                imagepanel.y = imagepanel.y + fadetime
            case "bottom2top"
                imagepanel.y = imagepanel.y - fadetime
            case "none"
                ' nop
        end select
        ' todo find out why this is needed
        if fade < 0 then fade = 0 end if
        if fade < 256 then
            ' special case mp3 file
            if mp3file <> "" then
                effectfade = "fadein"
            end if
            select case effectfade
				case "fadein"
					SDL_SetTextureColorMod(background_surface, fade, fade, fade)
				case "sepiaout"
					SDL_SetTextureColorMod(background_surface, min(112 + fade, 255), min(66 + fade, 255), min(20 + fade, 255))
				case "sepiain"
					SDL_SetTextureColorMod(background_surface, min(255 - 112 + fade, 255), min(255 - 66 + fade, 255), min(255 - 20 + fade, 255))
				case "crossfade"
					SDL_SetTextureAlphaMod(temp_surface, 255 - fade)
				case "none"
					SDL_SetTextureColorMod(background_surface, 255, 255, 255)
            end select
            fade += 2.0f * fadetime
        end if    
    end if

    SDL_RenderClear(renderer)
        SDL_RenderCopyEx(renderer, background_surface, null, @imagepanel, rotateangle, null, rotateimage)
        select case effectfade
            case "crossfade"
                SDL_RenderCopyEx(renderer, temp_surface, null, @imagepanel, rotateangle, null, rotateimage)
        end select

        ' clock
        texture = renderText(format(datetime, timeformat), ttffontclock, ttfcolor, 0, renderer)
        renderTexture(texture, renderer, clockposx, clockposy, 0, null, SDL_FLIP_NONE)
		if texture <> null then SDL_DestroyTexture(texture) : texture = 0

        ' date
		select case datedisplay
			case "full" 
				ddatetime = langenday(dayofweek) & ", " & day(datetime) & " " & langenmonth(month(datetime)) & " " & year(datetime)
			case "short"
				ddatetime = left(langenday(dayofweek), 3) & ", " & day(datetime) & " " & left(langenmonth(month(datetime)), 3) & " " & year(datetime)
			case "os"    
				ddatetime = format(datetime, dateformat)
		end select

        texture = renderText(ddatetime, ttffontdate, ttfcolor, 0, renderer)
        renderTexture(texture, renderer, clockposx, clockposy + fontsizeclock, 0, null, SDL_FLIP_NONE)
		if texture <> null then SDL_DestroyTexture(texture) : texture = 0

        ' display image name
        select case imagenametype
            case "folder"
                dummy = left(filename, instrrev(filename, pathchar) -1)
                imagename = mid(dummy, instrrev(dummy, pathchar) + 1)
            case "file"
				' todo figure out why this is an issue on linux returns <filename>.j 
				' its the drive vs mountpoint print left(filename, len(filename) - instr(filename, pathchar) -3)
                'imagename = mid(left(filename, len(filename) - instr(filename, pathchar) -1), InStrRev(filename, pathchar) + 1, len(filename))
				imagename = mid(filename, InStrRev(filename, pathchar) + 1, InStrRev(filename, ".") - InStrRev(filename, pathchar) - 1)				
            case "fullpath"
                imagename = filename
        end select
        ' special case mp3 file
        if mp3file <> "" then
            imagename = mid(left(mp3file, len(mp3file) - instr(mp3file, pathchar) -1), InStrRev(mp3file, pathchar) + 1, len(mp3file))
        end if

        texture = renderText(lcase(imagename), ttffontdate, ttfcolor, 0, renderer)
        renderTexture(texture, renderer, clockposx, clockposy + fontsizeclock + fontsizedate, 0, null, SDL_FLIP_NONE)
		if texture <> null then SDL_DestroyTexture(texture) : texture = 0
    SDL_RenderPresent(renderer)

end select ' image slideshow toggle

    ' sync fps and decrease cpu usage
    fpscurrent = syncfps(fps)
    if fullscreen then
        ' nop
    else
        if mp3chk then
            SDL_SetWindowTitle(glass, "imageviewer - " + mp3file  + " - " & fpscurrent & " fps")' / refresh monitor = " & desktopr)
        else
            SDL_SetWindowTitle(glass, "imageviewer - " + filename + " - " & fpscurrent & " fps")' / refresh monitor = " & desktopr)
        end if
    end if
wend

' clean up sdl resources
cleanup(pathchar, appname)
logentry("terminate", "normal termination " + appname)
