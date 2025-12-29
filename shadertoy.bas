' largely based on https://www.freebasic.net/forum/viewtopic.php?t=24462&hilit=shadertoy by djpeters
#include once "GL/gl.bi"
#include once "GL/glext.bi"

' setup glsdl window
Dim glglass as SDL_Window ptr
dim glfullscreen as boolean = false

' init hitbox button
dim boundbox as sdl_rect
dim offsety  as single = 0.325f

' setup shadertoy
dim w2 as integer
dim h2 as integer

type vec3
  as GLfloat x,y,z
end type

' internal screen dimension gl shader
dim as vec3 v3

' define opengl proc
#define glDefine(n) dim shared as PFN##n##PROC n
' shader
glDefine(glCreateShader)
glDefine(glDeleteShader)
glDefine(glShaderSource)
glDefine(glCompileShader)
glDefine(glGetShaderiv)
' program
glDefine(glCreateProgram)
glDefine(glDeleteProgram)
glDefine(glAttachShader)
glDefine(glDetachShader)
glDefine(glLinkProgram)
glDefine(glGetProgramiv)
glDefine(glUseProgram)
' uniform
glDefine(glGetUniformLocation)
glDefine(glUniform1f)
glDefine(glUniform2f)
glDefine(glUniform3f)
glDefine(glUniform4f)
glDefine(glUniform1i)
#undef glDefine

type ShaderToy
    declare destructor
    declare function CompileFile(Filename as string) as boolean
    declare function CompileCode(Code as string) as boolean
    as GLuint FragmentShader
    as GLuint ProgramObject
end type
destructor ShaderToy
    if ProgramObject then
        glUseprogram(0)
        if FragmentShader  then
            glDetachShader(ProgramObject,FragmentShader)
            glDeleteShader(FragmentShader)
        end if
        glDeleteProgram(ProgramObject)
    end if
end destructor

dim as ShaderToy Shader
dim as integer mx,my,mb
dim as double  tStart = Timer()
dim as double  tLast  = tStart
dim as double  tNow   = Timer()
Dim As integer epoch

function ShaderToy.CompileCode(UserCode as string) as boolean
    dim as GLint logSize
    dim as GLint status
    dim as string FragmentProlog
    FragmentProlog & =!"uniform float     iGlobalTime;  // shader playback time (in seconds) deprecated in 2017\n"
    FragmentProlog & =!"uniform float     iTime;        // shader playback time (in seconds)\n"
    FragmentProlog & =!"uniform vec3      iResolution;  // viewport resolution (in pixels)\n"
    FragmentProlog & =!"uniform vec4      iMouse;       // mouse pixel coords. xy: current (if MLB down), zw: click\n"
    FragmentProlog & =!"uniform vec4      iDate;        // (year, month, day, time in seconds)\n"
    FragmentProlog & =!"uniform sampler2D iChannel0;\n"
    FragmentProlog & =!"uniform sampler2D iChannel1;\n"
    FragmentProlog & =!"uniform sampler2D iChannel2;\n"
    FragmentProlog & =!"uniform sampler2D iChannel3;\n"
    dim as string FragmentEpilog
    FragmentEpilog &= !"void main() {\n"
    FragmentEpilog &= !"  vec4 color;\n"
    FragmentEpilog &= !"  // call user shader\n"
    FragmentEpilog &= !"  mainImage(color, gl_FragCoord.xy);\n"
    FragmentEpilog &= !"  color.w = 1.0;\n"
    FragmentEpilog &= !"  gl_FragColor = color;\n"
    FragmentEpilog &= !"}\n"

    dim as string FragmentCode = FragmentProlog & UserCode & FragmentEpilog
    ' cleanup previous shader  
    glUseprogram(0)
    glDeleteProgram(ProgramObject)  : ProgramObject  = 0

    FragmentShader = glCreateShader(GL_FRAGMENT_SHADER)

    if FragmentShader=0 then
        logentry("error", "glCreateShader(GL_FRAGMENT_SHADER) failed.")
        return false
    end if
    dim as GLchar ptr pCode=strptr(FragmentCode)
    glShaderSource (FragmentShader, 1, @pCode, NULL)
    glCompileShader(FragmentShader)
    glGetShaderiv  (FragmentShader, GL_COMPILE_STATUS, @status)
    if status = GL_FALSE then
        glGetShaderiv(FragmentShader, GL_INFO_LOG_LENGTH, @logSize)
        logentry("error", "glCompileShader(FragmentShader) failed ")
        glDeleteShader(FragmentShader) : FragmentShader = 0
        return false
    end if

    ProgramObject = glCreateProgram()
    if ProgramObject = 0 then
        logentry("error", "glCreateProgram() failed.")
        glDeleteShader(FragmentShader) : FragmentShader = 0
        return false
    end if
    glAttachShader(ProgramObject,FragmentShader)
    glLinkProgram (ProgramObject)
    glGetProgramiv(ProgramObject, GL_LINK_STATUS, @status)
    if (status = GL_FALSE) then
        glGetProgramiv(ProgramObject, GL_INFO_LOG_LENGTH, @logSize)
        logentry("error", "glLinkProgram() failed.")
        glDeleteShader(FragmentShader) : FragmentShader = 0
        return false
    end if

    ' cleanup aka detach shader  
    glDeleteShader (FragmentShader) : FragmentShader = 0
    logSize   = 0

    return true

end function

function ShaderToy.CompileFile(filename as string) as boolean
    dim as string code
    var hFile = FreeFile()
    if open(filename,for input, as #hFile) then
        logentry("error", filename + " shader path or file not found.")
        return false
    end if
    while not eof(hFile)
        dim as string aLine
        line input #hFile,aLine
        code &= aLine & !"\n"
    wend
    close #hFile
    return CompileCode(code)
end function

initgl:
' create a window with an opengl context
if fullscreen then
    glglass = SDL_CreateWindow("sdl2 opengl", null, null, screenwidth, screenheight, SDL_WINDOW_OPENGL _
                                or SDL_WINDOW_BORDERLESS or SDL_WINDOW_SHOWN)
        ' set vec3 iResolution
            v3.x = screenwidth
            v3.y = screenheight
            v3.z = v3.x/v3.y
else
    glglass = SDL_CreateWindow("sdl2 opengl", 100, 100, screenwidth, screenheight, SDL_WINDOW_OPENGL _
                                or SDL_WINDOW_BORDERLESS  or SDL_WINDOW_SHOWN)
        ' set vec3 iResolution
            v3.x = screenwidth 
            v3.y = screenheight
            v3.z = v3.x/v3.y

' use for setting to specific location
'    glglass = SDL_CreateWindow("sdl2 opengl", 100, 100, screenwidth * 0.35f, screenheight * 0.35f, SDL_WINDOW_OPENGL _
'                                or SDL_WINDOW_BORDERLESS or SDL_WINDOW_ALWAYS_ON_TOP or SDL_WINDOW_SHOWN)
'        SDL_SetWindowPosition(glglass,_
'        ((w2 + screenwidth) - screenwidth * 0.35f) - (screenwidth * 0.05f),_
'        ((h2 + screenheight) - screenheight * 0.25f) - (screenheight * 0.4f))
        ' set vec3 iResolution
'            v3.x = screenwidth  * 0.35f
'            v3.y = screenheight * 0.35f
'            v3.z = v3.x/v3.y
end if
' inital timing animation
tStart = Timer()
tLast  = tStart

' create opengl context bound to sdl
Dim As SDL_GLContext glContext = SDL_GL_CreateContext(glglass)
#define glProc(n) n = SDL_GL_GetProcAddress(#n) : if n = 0 then print "shadertoy opengl proc issue"
' shader
glProc(glCreateShader)
glProc(glDeleteShader)
glProc(glShaderSource)
glProc(glCompileShader)
glProc(glGetShaderiv)
' program
glProc(glCreateProgram)
glProc(glDeleteProgram)
glProc(glAttachShader)
glProc(glDetachShader)
glProc(glLinkProgram)
glProc(glGetProgramiv)
glProc(glUseProgram)
' uniform
glProc(glGetUniformLocation)
glProc(glUniform1f)
glProc(glUniform2f)
glProc(glUniform3f)
glProc(glUniform4f)
glProc(glUniform1i)
#undef glProc
