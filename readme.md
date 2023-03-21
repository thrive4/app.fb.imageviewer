## imageviewer
basic imageviewer written in freebasic and sdl2\
supported image types or extensions:\
.bmp, .gif, .jpg, .png, .pcx, .jpeg, .tff\
Special support for .mp3 this will extract the\
cover art from a mp3 and display it if present.
## usage
imageviewer.exe "path to file or folder"\
imageviewer.exe "path to file or folder" fullscreen\
if no file or path is specified the current dir will be scanned for an image\
if the folder has subfolder(s) these will be scanned for images as well
## requirements
sdl2 (32bit) v2.0.22.0\
https://github.com/libsdl-org/SDL/releases
\
sdl image (32bit) v2.6.0.0\
https://github.com/libsdl-org/SDL_image/releases
## performance
windows 7 / windows 10(1903)\
ram usage ~20MB / 20MB (pending image size)\
handles   ~160 / ~200\
threads   11 / 16\
cpu       ~1 (low) / ~2\
tested on intel i5-6600T
## navigation
arrow     | dpad left  | mouse left : back\
arrow     | dpad right | mouse right: forward\
num plus  | R2         | scroll up  : zoom in\
num min   | L2         | scroll down: zoom out\
space     | button A   | scroll mid : reset zoom and scaletype\
r / enter | button B                : rotate\
z         | button Y   |            : scale type\
f11                                 : toggle fullscreen\
esc                                 : close application
## special thanks
TwinklebearDev SDL 2.0 Tutorial Lesson 3\
Tutorial translating to FreeBASIC by Michael "h4tt3n" Schmidt Nissen
