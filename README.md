## imageviewer [![Github All Releases](https://img.shields.io/github/downloads/thrive4/app.fb.imageviewer/total.svg)]()
basic imageviewer written in freebasic and sdl2\
supported image types or extensions:\
 .bmp, .gif, .gls, .jpg, .jpeg, .png, .pcx, .svg, .webp\
Special support for .mp3 this will extract the\
cover art from a mp3 and display it if present\
and .gls aka shadertoy webgl shaders.

See https://www.shadertoy.com/ for more info.

Haptic support for keyboard, mouse and gamepad.

## usage
imageviewer.exe "path to file or folder"\
imageviewer.exe "path to file or folder" fullscreen\
if no file or path is specified the current dir will be scanned for an image\
if the folder has subfolder(s) these will be scanned for images as well\
or specify a path via confconf.ini

generate .m3u: imageviewer "path to file or folder" "tag" "tagquery"\
example: imageviewer.exe <mp3 drive path folder> artist beethoven\
generates the m3u file beethoven.m3u\
which then can be played by imageviewer.exe beethoven.m3u\
* simple search so 195 is equivelant of ?195? or *195*
* runtime in seconds is not calculated default is #EXTINF:134
* no explicit wildcard support, only searchs on one tag
* supported tags artist, title, album, genre and year

shader demo:\
imageviewer.exe media\glsl

## install
open zip file and copy contents to preferd folder\
this application is **portable**.

## configuration
basic config options in conf.ini\
locale          = <en, es, de, fr, nl>\
[images]\
' location images\
imagefolder = <image drive path folder>\
[screensaver]\
' time passed before screensaver starts in seconds\
screensaveinterval = 180\
' options dimscreen\
screensavetype = dimscreen

## requirements
sdl2 2.28.5.0 or up\
https://github.com/libsdl-org/SDL/releases

sdl image 2.8.1 or up\
https://github.com/libsdl-org/SDL_image/releases

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
r / enter | button B                : rotate
z         | button Y   |            : scale type
f11                                 : toggle fullscreen
esc                                 : close application
```

## special thanks
TwinklebearDev SDL 2.0 Tutorial Lesson 3\
Tutorial translating to FreeBASIC by Michael "h4tt3n" Schmidt Nissen

djpeters for freebasic shadertoy webgl intergration\
https://www.freebasic.net/forum/viewtopic.php?t=24462&hilit=shadertoy

and many thanks to various authors at shadertoy\
for the demo shaders in the media\glsl folder
