## imageviewer
basic imageviewer written in freebasic and sdl2\
supported image types or extensions:\
.bmp, .gif, .jpg, .jpeg, .pcx, .png, .svg\
Special support for .mp3 this will extract the\
cover art from a mp3 and display it if present.\
Haptic support for keyboard, mouse and gamepad.
## usage
imageviewer.exe "path to file or folder"\
imageviewer.exe "path to file or folder" fullscreen\
if no file or path is specified the current dir will be scanned for an image\
if the folder has subfolder(s) these will be scanned for images as well\
or specify a path via \conf\conf.ini\

generate .m3u: slideshow "path to file or folder" "tag" "tagquery"\
example: slideshow.exe g:\data\mp3\classic artist beethoven\
generates the m3u file beethoven.m3u\
which then can be played by slideshow.exe beethoven.m3u
* simple search so 195 is equivelant of ?195? or \*195*
* runtime in seconds is not calculated default is #EXTINF:134
* no explicit wildcard support, only searchs on one tag
* supported tags artist, title, album, genre and year
## configuration
basic config options in conf.ini\
locale          = <en, de, fr, nl>\
[images]\
' location images\
imagefolder = g:\data\images\flickr\alpha clock
[screensaver]\
' time passed before screensaver starts in seconds\
screensaveinterval = 180\
' options dimscreen\
screensavetype = dimscreen
## requirements
sdl2 2.28.5.0 or up\
https://github.com/libsdl-org/SDL/releases
\
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
