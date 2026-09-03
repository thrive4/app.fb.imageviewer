## imageviewer [![Github All Releases](https://img.shields.io/github/downloads/thrive4/app.fb.imageviewer/total.svg)]()
basic imageviewer and slideshow written in freebasic and sdl2\
supported image types or extensions:\
 .bmp, .gif, .gls, .jpg, .jpeg, .png, .pcx, .svg, .webp\

Slideshow effects include a pan scan zoom aka the\
'ken burns' effect plus fade in and crossfade.\

Special support for .mp3 this will extract the\
cover art from a mp3 and display it if present\
and .gls aka shadertoy webgl shaders.

See https://www.shadertoy.com/ for more info.

Haptic support for keyboard, mouse and gamepad.

note: 2026 august **merged** slideshow repo with imageviewer\
https://github.com/thrive4/app.fb.slideshow

## usage
imageviewer.exe "path to file or folder"\
imageviewer.exe "path to file or folder" fullscreen\
imageviewer.exe "path to file or folder" slideshow\
imageviewer.exe "path to file or folder" slideshow fullscreen\
if no file or path is specified the current dir will be scanned for an image\
if the folder has subfolder(s) these will be scanned for images as well\
or specify a path via confconf.ini

generate .m3u: imageviewer "path to file or folder" "tag" "tagquery"\
example: imageviewer.exe <mp3 drive path folder> artist beethoven\
generates the m3u file beethoven.m3u\
which then can be played by imageviewer.exe beethoven.m3u
* simple search so 195 is equivelant of ?195? or *195*
* runtime in seconds is not calculated default is #EXTINF:134
* no explicit wildcard support, only searchs on one tag
* supported tags artist, title, album, genre and year

shader demo:\
imageviewer.exe media\glsl

## install
open zip file and copy contents to preferd folder\
this application is **portable**.\
note: for windows all requirements are bundeld in zip\
      for linux see below.

## requirements
**windows**
sdl2 2.32.10.0 or up\
https://github.com/libsdl-org/SDL/releases

sdl image 2.8.10.0 or up\
https://github.com/libsdl-org/SDL_image/releases

sdl ttf 2.20.2.0 or up\
https://github.com/libsdl-org/SDL_image/releases

**linux** get if needed\
sdl2:\
sudo apt install libsdl1.2-dev\

sdl_image and sdl_ttf for sdl 2:\
sudo apt install libsdl2-image-dev libsdl2-ttf-dev\

verify or check if already installed:
ldconfig -p | grep SDL2
or / and:
pkg-config --list-all | grep SDL2

## performance
windows 7 / windows 10(1903)\
ram usage ~20MB / 20MB (pending image size)\
handles   ~160 / ~200\
threads   11 / 16\
cpu       ~1 (low) / ~2\
tested on intel i5-6600T

## navigation
```
  arrow     | dpad left  | mouse left : back
  arrow     | dpad right | mouse right: forward
  num plus  | R2         | scroll up  : zoom in
  num min   | L2         | scroll down: zoom out
  space     | button A   | scroll mid : reset zoom and scaletype
  r / enter | button B   |            : rotate
  z         | button Y   |            : scale type
  i         | button X   |            : display image name
  m         | start      |            : display image metrics
  f1        | back       |            : display help
  f11                                 : toggle fullscreen
  esc                                 : close application
```

## configuration
```
[general]
mapversion   = 1.2
appversion   = 1.8

[screen]
screenwidth  = 1280
screenheight = 720
fullscreen   = false

[application]
' options de, en, es, fr and nl
locale          = en
' options verbose, full
logtype         = full
' used for debug build
usecons         = false
' options slideshow or image
launch            = image

[clock and date display]
' place ttf in media folder
ttffont       = gisha.ttf
' options dddd, dd mmm yyyy or dd/mm/yyyy
dateformat    = dddd, dd mmm yyyy
' options hh:mm, hh:mm:ss AM/PM, hh:mm AM/PM
timeformat    = hh:mm
' clockposistion options bottomleft, bottomright, topleft, topright
clockposistion = bottomleft
' language date options os, full, short
' full and short use date.ini allowing
' to override the os language
datedisplay = short
' display filename options file, fullpath, folder
imagenametype = folder

[media]
' location images
'mediafolder = g:\data\images\flickr\alpha clock
mediafolder = /media/wah/games/data/images/flickr/alpha clock/
' options shuffle, linear
playtype = linear

[screensaver]
' time passed before screensaver starts in seconds
screensaveinterval = 180
' timer interval between showing next image in microseconds
interval = 10000
' options dimscreen
screensavetype = dimscreen
```

## special thanks
TwinklebearDev SDL 2.0 Tutorial Lesson 3\
Tutorial translating to FreeBASIC by Michael "h4tt3n" Schmidt Nissen

djpeters for freebasic shadertoy webgl intergration\
https://www.freebasic.net/forum/viewtopic.php?t=24462&hilit=shadertoy

kuan-hsu for poseidonFB works well on linux (pop os)\
https://github.com/kuan-hsu/poseidonFB

gisha.ttf       courtesy of microsoft corporation (non-commerical use)
lekton-bold.ttf courtesy of various authors (non-commerical use)

and many thanks to various authors at shadertoy\
for the demo shaders in the media\glsl folder
