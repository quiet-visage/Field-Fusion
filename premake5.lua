function include_msdf()
  includedirs "src/"
  links "msdflib"
end

workspace "msdf"
  language "C++"
  cppdialect "C++17"
  configurations {"Debug", "Release"}
  links {
    "GLEW",
    "EGL", 
    "GL",  
    "freetype",
    "GLU", 
    "glfw",
  }
  filter "configurations:Debug"
  defines {"DEBUG"}
  symbols "On"

  filter "configurations:Release"
    optimize "Full"

project "msdflib"
  files "src/**"
  kind "StaticLib"
  includedirs "/usr/include/freetype2"

project "demo"
  kind "WindowedApp"
  files {"demo/**"}
  includedirs {"/usr/include/freetype2"}
  include_msdf()
