' TwinklebearDev SDL 2.0 Tutorial Lesson 3: SDL Extension Libraries
' Translated to FreeBASIC by Michael "h4tt3n" Schmidt Nissen, march 2017
' http://www.willusher.io/sdl2%20tutorials/2013/08/18/lesson-3-sdl-extension-libraries
' tweaked for fb and sdl2 sept 2022 by thrive4
' supported image format bmp, gif, jpeg, jpg, lbm, pcx, png, pnm, svg, tga, tiff, tff, webp, xcf, xpm, xv

#include once "SDL2/SDL.bi"
#include once "SDL2/SDL_image.bi"
' dir dunction and provides constants to use for the attrib_mask parameter
#include once "vbcompat.bi"
#include once "dir.bi"
#include once "utilfile.bas"
#include once "shuffleplay.bas"

dim event as SDL_Event
dim running as boolean = True
dim screenwidth As integer   = 1280
dim screenheight As integer  = 720
dim fullscreen as boolean = false
dim desktopw as integer
dim desktoph as integer
dim desktopr as integer
dim rotateimage as SDL_RendererFlip = SDL_FLIP_NONE
dim rotateangle as double = 0
'zoomtype options stretch, scaled, zoomsmallimage
dim zoomtype as string = "zoomsmallimage"
dim dummy as string
' get desktop info
ScreenInfo desktopw, desktoph,,,desktopr

' init framerate counter
dim as integer frames,fps
dim as double tStart = Timer()
dim as double tLast=tStart

' setup list of images for background
dim filename as string
dim imagefolder as string
dim imagetypes as string = ".bmp, .gif, .jpg, .png, .pcx, .jpeg, .tff" 
dim playtype as string = "linear"

' define specific area for overlay
dim srcrect as SDL_Rect
srcrect.x = 100
srcrect.y = 0
srcrect.w = 32
srcrect.h = 32

' main area for rendering
dim mrect as SDL_Rect
dim center as SDL_Point

' init zoom
dim imagex as integer
dim imagey as integer
dim imposx as single
dim imposy as single
dim scaledw as single
dim scaledh as single
dim scale as single

' scaling image
Dim As Integer iW, iH

' restore size no zoom
Dim oposx as integer = imposx
Dim oposy as integer = imposy
Dim oscaledw as integer = scaledw
Dim oscaledh as integer = scaledh

sub displayhelp
        print ""
        print "> command line options <"
        print "file or path : imageviewer.exe g:\data\images\paris"
        print "if no file or path is specified the current dir will be scanned for an image"
        print "fullscreen   : set display to fullscreen"
        print "example      : imageviewer.exe g:\data\images\paris fullscreen"
        print ""
        print "> navigation <"
        print "arrow left :  back"
        print "arrow right:  forward"
        print "f11        :  toggle fullscreen"
        print "num plus   :  zoom in"
        print "num min    :  zoom out"
        print "num .      :  reset zoom"
        print "r          :  rotate image"
        print "esc        :  close application"
end sub

select case command(2)
    case "/?", "-man", ""
        displayhelp
    case "fullscreen"
        screenwidth  = desktopw
        screenheight = desktoph
        fullscreen = true
    case ""
        ' no switch    
        'displayhelp
end select

' get images if applicable override first image with preferd a specific image
if instr(command(1), ".") <> 0 then
    filename = command(1)    
    imagefolder = left(command(1), instrrev(command(1), "\") - 1)
    chk = createlist(imagefolder, imagetypes, "image")
else
    if instr(command(1), ":") <> 0 then
        imagefolder = command(1)
        if checkpath(imagefolder) = false then
            print "error: path not found " + imagefolder
            end
        else
            chk = createlist(imagefolder, imagetypes, "image")
            filename = listplay(playtype, "image")
        end if
    ELSE
        imagefolder = exepath
        chk = createlist(imagefolder, imagetypes, "image")
        filename = listplay(playtype, "image")
        if chk = 0 then
            PRINT "error: no displayable files found"
            end
        end if
    end if
end if    

initsdl:
Screen 0, , , &h80000000
' init window and render
If (SDL_Init(SDL_INIT_VIDEO) = not NULL) Then 
    logentry("error", "sdl2 video could not be initlized error: " + *SDL_GetError())
    SDL_Quit()
else
    ' no audio needed
    SDL_QuitSubSystem(SDL_INIT_AUDIO)
    ' render scale quality: 0 point, 1 linear, 2 anisotropic
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "1")
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
    logentry("error", "sdl2 could not create window")
	SDL_Quit()
EndIf
Dim As SDL_Renderer Ptr renderer = SDL_CreateRenderer(glass, -1, SDL_RENDERER_ACCELERATED Or SDL_RENDERER_PRESENTVSYNC)
'SDL_SetWindowOpacity(glass, 0.5)
if (renderer = NULL) Then	
    logentry("error", "sdl2 could not create renderer")
	SDL_Quit()
EndIf

' load texture
Dim As SDL_Texture Ptr background_surface
background_surface = IMG_LoadTexture(renderer, filename)

' verify load image
if ( background_surface = NULL ) Then
	'cleanup(background, image, renderer, window)
    logentry("error", "sdl2 could not create texture")
	IMG_Quit()
	SDL_Quit()
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

while running
    dim as double tNow=Timer()    
    while SDL_PollEvent(@event) <> 0
        ' basic interaction
        select case event.type
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_ESCAPE
                running = False
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
                        goto initsdl
                    case false
                        screenwidth  = desktopw
                        screenheight = desktoph
                        fullscreen = true
                        goto initsdl
                end select
            ' zoom manual
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_PLUS
                zoomtype = "zoomin"
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_MINUS
                zoomtype = "zoomout"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_KP_PERIOD
                zoomtype = "zoomsmallimage"
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_LEFT
                ' get previous image in folder if avaiable
                filename = listplay("linearmin", "image")
                SDL_DestroyTexture(background_surface)
                background_surface = IMG_LoadTexture(renderer, filename)
                ' reset rotation
                rotateangle = 0
            CASE SDL_KEYDOWN and event.key.keysym.sym = SDLK_RIGHT
                ' get next image in folder if avaiable
                filename = listplay(playtype, "image")
                SDL_DestroyTexture(background_surface)
                background_surface = IMG_LoadTexture(renderer, filename)
                ' reset rotation
                rotateangle = 0
            ' rotate clockwise
            case SDL_KEYDOWN and event.key.keysym.sym = SDLK_R
                 if rotateangle > -270 then
                    rotateangle = rotateangle - 90
                 else
                     rotateangle = 0
                 end if       
            ' restore rotate
            'if event.type = SDL_KEYDOWN and event.key.keysym.sym = SDLK_DOWN then
            '     rotateimage = SDL_FLIP_NONE
            '     rotateangle = 0
            'end if
        end select
    wend

    ' scaling image
    SDL_QueryTexture(background_surface, NULL, NULL, @iW, @iH)
    select case zoomtype
        ' scale image
        case "scaled"
            chk = scaledfit(screenwidth, screenheight, iW, iH, scaledw, scaledh, imagex, imagey)
            imposx = imagex
            imposy = imagey
            scaledw = scaledw
            scaledh = scaledh
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
    SDL_RenderPresent(renderer)
    if fullscreen then
    else
        ' framerate counter
        frames+=1
        if frames mod 60 = 0 then
            tNow=timer()
            fps = 60 / (tNow-tLast)
            tLast=tNow
        end if    
        SDL_SetWindowTitle(glass, "imageviewer - " + filename + " - " & fps & " fps / refresh monitor = " & desktopr )
    end if

    ' decrease cpu usage
    SDL_Delay(30)
wend

' cleanup listplay files
delfile(exepath + "\" + "image" + ".tmp")
delfile(exepath + "\" + "image" + ".lst")

'cleanup background, image, renderer, glass
SDL_DestroyTexture(background_surface)
SDL_DestroyRenderer(renderer)
SDL_DestroyWindow(glass)
IMG_Quit()
SDL_Quit()
close

end
