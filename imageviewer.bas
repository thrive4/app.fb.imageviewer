' TwinklebearDev SDL 2.0 Tutorial Lesson 3: SDL Extension Libraries
' Translated to FreeBASIC by Michael "h4tt3n" Schmidt Nissen, march 2017
' http://www.willusher.io/sdl2%20tutorials/2013/08/18/lesson-3-sdl-extension-libraries
' tweaked for fb and sdl2 sept 2022 by thrive4
' supported image formats .bmp, .gif, .jpg, .mp3, .png, .pcx, .jpeg, .tff, .webp

#include once "SDL2/SDL.bi"
#include once "SDL2/SDL_image.bi"
#include once "utilfile.bas"
#include once "shuffleplay.bas"
#cmdline "app.rc"

' setup imageviewer
dim event           as SDL_Event
dim running         as boolean = True
dim screenwidth     As integer = 1280
dim screenheight    As integer = 720
dim fullscreen      as boolean = false
dim fps             as ulong   = 30
dim fpscurrent      as ulong
dim desktopw        as integer
dim desktoph        as integer
dim desktopr        as integer
dim rotateimage     as SDL_RendererFlip = SDL_FLIP_NONE
dim rotateangle     as double = 0

'zoomtype options stretch, scaled, zoomsmallimage
dim zoomtype        as string = "zoomsmallimage"
dim shared dummy    as string
dim shared mp3chk   as boolean
dummy = ""
mp3chk = false
' get desktop info
ScreenInfo desktopw, desktoph,,,desktopr

' setup list of images for background
dim filename    as string
dim fileext     as string = ""
dim imagefolder as string
dim imagetypes  as string = ".bmp, .gif, .jpg, .mp3, .png, .pcx, .jpeg, .svg, .tff" 
dim playtype    as string = "linear"

' screensaver
'dim screensavepath     as string  = backgroundpath
dim screensaveinterval as integer = 30 * 1000 ' in seconds
dim screensaveactive   as boolean = false
dim screensaveinittime as integer = 0
dim screensavetype     as string  = "dimscreen"
dim currenttime        as integer = 0
dim fadetime           as single = ((screensaveinterval / 1000) * 1.25) / (screensaveinterval / 1000)
dim fade               as integer = 255

' define area dim screen
dim dimscreen as SDL_Rect

' main area for rendering
dim mrect  as SDL_Rect
dim center as SDL_Point

' init zoom
dim imagex  as integer
dim imagey  as integer
dim imposx  as single
dim imposy  as single
dim scaledw as single
dim scaledh as single
dim scale   as single

' scaling image
Dim As Integer iW, iH

' restore size no zoom
Dim oposx    as integer = imposx
Dim oposy    as integer = imposy
Dim oscaledw as integer = scaledw
Dim oscaledh as integer = scaledh

' navigation default values
dim kback as integer
' init app with config file if present conf.ini
dim itm     as string
dim inikey  as string
dim inival  as string
dim inifile as string = exepath + "\conf\conf.ini"
dim f as long
if FileExists(inifile) = false then
    logentry("error", inifile + "file does not excist")
else 
    f = readfromfile(inifile)
    Do Until EOF(f)
        Line Input #f, itm
        if instr(1, itm, "=") > 1 then
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
                    case "logtype"
                        logtype = inival
                    case "kback"
                        ' nop
                    case "screensaveinterval"
                        screensaveinterval = val(inival) * 1000
                        if screensaveinterval = 0 or screensaveinterval <= 10000 then
                            logentry("notice", "screensaver interval set to lower than minimum 10 sec, forced to (default) 30 sec")                            
                            screensaveinterval = 30000
                        end if
                        fadetime = ((screensaveinterval / 1000) * 1.25) / (screensaveinterval / 1000)
                    case "screensavetype"
                        screensavetype = inival
                end select
            end if
            'print inikey + " - " + inival
        end if    
    loop
    close(f)    
end if    

select case command(2)
    case "/?", "-man", ""
        displayhelp(locale)
    case "fullscreen"
        screenwidth  = desktopw
        screenheight = desktoph
        fullscreen = true
    case ""
        ' no switch    
        'displayhelp
end select

' get images if applicable override first image with preferd a specific image
imagefolder = command(1)
if instr(command(1), ".") <> 0 then
    fileext = lcase(mid(command(1), instrrev(command(1), ".")))
    if instr(1, imagetypes, fileext) = 0 then
        logentry("fatal", command(1) + " file type not supported")
    end if
    if FileExists(exepath + "\" + command(1)) = false then
        if FileExists(imagefolder) then
            'nop
        else
            logentry("fatal", imagefolder + " does not excist or is incorrect")
        end if
    else
        imagefolder = exepath + "\" + command(1)
    end if
    filename = command(1)    
    imagefolder = left(command(1), instrrev(command(1), "\") - 1)
    chk = createlist(imagefolder, imagetypes, "image")
    currentimage = setcurrentlistitem("image", filename)
else
    if instr(command(1), ":") <> 0 then
        imagefolder = command(1)
        if checkpath(imagefolder) = false then
            logentry("fatal", "error: path not found " + imagefolder)
        else
            chk = createlist(imagefolder, imagetypes, "image")
            filename = listplay(playtype, "image")
        end if
    ELSE
        imagefolder = exepath
        chk = createlist(imagefolder, imagetypes, "image")
        filename = listplay(playtype, "image")
        if chk = false then
            dummy = "no displayable files found"
            print dummy    
            logentry("notice", dummy)
            goto cleanup
        end if
    end if
end if

' check and get mp3 cover art
sub checkmp3cover(byref filename as string)
    if getmp3cover(filename) and instr(filename, ".mp3") > 0 then
        dummy  = filename
        mp3chk = true
        if FileExists(exepath + "\thumb.jpg") then
            filename = exepath + "\thumb.jpg"
        else
            filename = exepath + "\thumb.png"
        end if
    else
        dummy  = ""
        mp3chk = false
    end if
end sub

' get next or previous image
sub getimage(byref filename as string, byref dummy as string, byref mp3chk as boolean, byval playtype as string)
    filename = listplay(playtype, "image")
    checkmp3cover(filename)
    ' validate if false get next image
    if filename = "" or FileExists(filename) = false then
        filename = listplay(playtype, "image")
        checkmp3cover(filename)
    end if
end sub

' via https://www.freebasic.net/forum/viewtopic.php?t=32323 by fxm
Function syncfps(ByVal MyFps As Ulong, ByVal SkipImage As Boolean = True, ByVal Restart As Boolean = False, ByRef ImageSkipped As Boolean = False) As Ulong
    '' 'MyFps' : requested FPS value, in frames per second
    '' 'SkipImage' : optional parameter to activate the image skipping (True by default)
    '' 'Restart' : optional parameter to force the resolution acquisition, to reset to False on the next call (False by default)
    '' 'ImageSkipped' : optional parameter to inform the user that the image has been skipped (if image skipping is activated)
    '' function return : applied FPS value (true or apparent), in frames per second
    Static As Single tos
    Static As Single bias
    Static As Long count
    Static As Single sum
    ' initialization calibration
    If tos = 0 Or Restart = True Then
        Dim As Double t = Timer
        For I As Integer = 1 To 10
            Sleep 1, 1
        Next I
        Dim As Double tt = Timer
        #if Not defined(__FB_WIN32__) And Not defined(__FB_LINUX__)
        If tt < t Then t -= 24 * 60 * 60
        #endif
        tos = (tt - t) / 10 * 1000
        bias = 0
        count = 0
        sum = 0
    End If
    Static As Double t1
    Static As Long N = 1
    Static As Ulong fps
    Static As Single tf
    ' delay generation
    Dim As Double t2 = Timer
    #if Not defined(__FB_WIN32__) And Not defined(__FB_LINUX__)
    If t2 < t1 Then t1 -= 24 * 60 * 60
    #endif
    Dim As Double t3 = t2
    Dim As Single dt = (N * tf - (t2 - t1)) * 1000 - bias
    If (dt >= 3 * tos / 2) Or (SkipImage = False) Or (N >= 20) Or (fps / N <= 10) Then
        If dt <= tos Then dt = tos / 2
        Sleep dt, 1
        t2 = Timer
        #if Not defined(__FB_WIN32__) And Not defined(__FB_LINUX__)
        If t2 < t1 Then t1 -= 24 * 60 * 60 : t3 -= 24 * 60 * 60
        #endif
        fps = N / (t2 - t1)
        tf = 1 / MyFps
        t1 = t2
        ' automatic test and regulation
        Dim As Single delta = (t2 - t3) * 1000 - (dt + bias)
        If Abs(delta) > 3 * tos Then
            tos = 0
        Else
            bias += 0.1 * Sgn(delta)
        End If
        ' automatic calibation
        If dt < tos Then
            If count = 100 Then
                tos = sum / 100 * 1000
                bias = 0
                sum = 0
                count = 0
            Else
                sum += (t2 - t3)
                count += 1
            End If
        End If
        ImageSkipped = False
        N = 1
    Else
        ImageSkipped = True
        N += 1
    End If
    Return fps
End Function

' check if mp3 coverart present keep outside f11 fullscreen toggle
checkmp3cover(filename)

initsdl:
' init window and render
SDL_SetHint(SDL_HINT_VIDEO_ALLOW_SCREENSAVER, "1")
' respond to power plan settings blank display on windows set hint before sdl init video
If (SDL_Init(SDL_INIT_VIDEO) = not NULL) Then
    logentry("terminate", "sdl2 video could not be initlized error: " + *SDL_GetError())
    SDL_Quit()
else
    ' no audio needed
    SDL_QuitSubSystem(SDL_INIT_AUDIO)
    ' render scale quality: 0 point, 1 linear, 2 anisotropic
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1") 
   ' filter non used events
    SDL_EventState(SDL_FINGERMOTION,    SDL_IGNORE)
    SDL_EventState(SDL_MULTIGESTURE,    SDL_IGNORE)
    SDL_EventState(SDL_DOLLARGESTURE,   SDL_IGNORE)
End If
' setup glass aka window
Dim As SDL_Window Ptr glass
if fullscreen then
    SDL_ShowCursor(SDL_DISABLE)
    glass = SDL_CreateWindow( "imageviewer", null, null, screenwidth, screenheight, SDL_WINDOW_BORDERLESS)
else
    SDL_ShowCursor(SDL_ENABLE)
    glass = SDL_CreateWindow( "imageviewer", 100, 100, screenwidth, screenheight, SDL_WINDOW_RESIZABLE)
end if
if (glass = NULL) Then
	SDL_Quit()
    logentry("fatal", "abnormal termination sdl2 could not create window")
EndIf
Dim As SDL_Renderer Ptr renderer = SDL_CreateRenderer(glass, -1, SDL_RENDERER_ACCELERATED Or SDL_RENDERER_PRESENTVSYNC)
'SDL_SetWindowOpacity(glass, 0.5)
if (renderer = NULL) Then	
	SDL_Quit()
    logentry("fatal", "abnormal termination sdl2 could not create renderer")
EndIf
' setup dim screen dimensions
dimscreen.x = 0
dimscreen.y = 0
dimscreen.w = screenwidth
dimscreen.h = screenheight

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

' load texture
Dim As SDL_Texture Ptr background_surface
SDL_DestroyTexture(background_surface)
background_surface = IMG_LoadTexture(renderer, filename)
' verify load image
if ( background_surface = NULL ) Then
	'cleanup(background, image, renderer, window)
	IMG_Quit()
	SDL_Quit()
    logentry("fatal", "abnormal termination sdl2 could not create texture")
EndIf

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

' main
while running
    ' screen dimmer / saver timer in microseconds
    currenttime = SDL_GetTicks()

    while SDL_PollEvent(@event) <> 0
        ' basic interaction
        select case event.type
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_ESCAPE
                if screensaveactive then
                    ' nop
                else
                    running = False
                end if
            case SDL_WINDOWEVENT and event.window.event = SDL_WINDOWEVENT_CLOSE
                running = False
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_F11
                SDL_DestroyTexture(background_surface)
                SDL_DestroyRenderer(renderer)
                SDL_DestroyWindow(glass)
                select case fullscreen
                    case true
                        ' enable or disable mouse cursor in window
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
                IMG_Quit()
                close
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
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_LEFT
                ' get previous image in folder if avaiable
                getimage(filename, dummy, mp3chk, "linearmin")
                SDL_DestroyTexture(background_surface)
                background_surface = IMG_LoadTexture(renderer, filename)
                ' reset rotation and zoomtype
                rotateangle = 0
                zoomtype = "zoomsmallimage"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_RIGHT
                ' get next image in folder if avaiable
                getimage(filename, dummy, mp3chk, playtype)
                SDL_DestroyTexture(background_surface)
                background_surface = IMG_LoadTexture(renderer, filename)
                ' reset rotation
                rotateangle = 0
                zoomtype = "zoomsmallimage"
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
                        getimage(filename, dummy, mp3chk, playtype)
                        SDL_DestroyTexture(background_surface)
                        background_surface = IMG_LoadTexture(renderer, filename)
                        ' reset rotation
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                    case SDL_BUTTON_MIDDLE
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                    case SDL_BUTTON_RIGHT
                        ' get previous image in folder if avaiable
                        getimage(filename, dummy, mp3chk, "linearmin")
                        SDL_DestroyTexture(background_surface)
                        background_surface = IMG_LoadTexture(renderer, filename)
                        ' reset rotation
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                end select
            case SDL_MOUSEBUTTONUP
                'nop
            ' navigation gamepad dpad and A button
            case SDL_CONTROLLERBUTTONUP
                select case event.cbutton.button    
                    case SDL_CONTROLLER_BUTTON_DPAD_LEFT
                        ' get previous image in folder if avaiable
                        getimage(filename, dummy, mp3chk, "linearmin")
                        SDL_DestroyTexture(background_surface)
                        background_surface = IMG_LoadTexture(renderer, filename)
                        ' reset rotation
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
                    case SDL_CONTROLLER_BUTTON_DPAD_RIGHT
                        ' get next image in folder if avaiable
                        getimage(filename, dummy, mp3chk, playtype)
                        SDL_DestroyTexture(background_surface)
                        background_surface = IMG_LoadTexture(renderer, filename)
                        ' reset rotation
                        rotateangle = 0
                        zoomtype = "zoomsmallimage"
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
                    case SDL_CONTROLLER_BUTTON_Y
                        select case zoomtype
                            case "scaled"
                                zoomtype = "zoomsmallimage"
                            case "zoomsmallimage"
                                zoomtype = "stretch"
                            case "stretch"
                                zoomtype = "scaled"
                        end select
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
        if screensaveactive then
            screensaveactive   = false
            screensaveinittime = currenttime
            fade               = 255
            SDL_DestroyTexture(background_surface)
            background_surface = IMG_LoadTexture(renderer, filename)
        end if
    wend

    ' scaling image
    SDL_QueryTexture(background_surface, NULL, NULL, @iW, @iH)
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

    'display image
    SDL_RenderClear(renderer)
        mrect.x = imposx
        mrect.y = imposy
        mrect.w = scaledw
        mrect.h = scaledh
        SDL_RenderCopyEx(renderer, background_surface, null, @mrect, rotateangle, null, rotateimage)
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
    SDL_RenderPresent(renderer)

    ' sync fps and decrease cpu usage
    fpscurrent = syncfps(fps)
    if fullscreen then
        ' nop
    else
        if mp3chk then
            SDL_SetWindowTitle(glass, "imageviewer - " + dummy    + " - " & fpscurrent & " fps")' / refresh monitor = " & desktopr)
        else
            SDL_SetWindowTitle(glass, "imageviewer - " + filename + " - " & fpscurrent & " fps")' / refresh monitor = " & desktopr)
        end if
    end if
wend

cleanup:
' cleanup listplay files
delfile(exepath + "\" + "image" + ".tmp")
delfile(exepath + "\" + "image" + ".lst")
delfile(exepath + "\" + "image" + ".swp")
delfile(exepath + "\thumb.jpg")
delfile(exepath + "\thumb.png")

'cleanup background, image, renderer, glass
if chk Then
    SDL_DestroyTexture(background_surface)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(glass)
    IMG_Quit()
    SDL_Quit()
end if
close
logentry("terminate", "normal termination " + appname)
