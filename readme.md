## imageviewer
basic imageviewer written in freebasic and sdl\
supported image types or extensions:\
.bmp, .gif, .jpg, .png, .pcx, .jpeg, .tff\
32bit version tested
## usage
imageviewer.exe "path to file or folder"\
imageviewer.exe "path to file or folder" fullscreen\
if no file or path is specified the current dir will be scanned for an image\
if the folder has subfolder(s) these will be scanned for images as well 
## requirements
sdl (32bit)\
https://github.com/libsdl-org/SDL/releases
\
sdl image (32bit)\
https://github.com/libsdl-org/SDL_image/releases
## navigation
arrow left  :  back\
arrow right :  forward\
f11         :  toggle fullscreen\
num plus    :  zoom in\
num min     :  zoom out\
num .       :  reset zoom\
r           :  rotate image\
esc         :  close application
