function include_ff()
  includedirs "src/"
  links "fieldfusion"
end

workspace "fieldfusion"
  language "C++"
  cppdialect "C++17"
  configurations {"release", "debug"}
  links {
    "GLEW",
    "EGL", 
    "GL",  
    "freetype",
    "GLU", 
  }
  filter "configurations:debug"
  defines {"DEBUG"}
  symbols "On"

  filter "configurations:release"
    optimize "Full"

project "fieldfusion"
  files "src/**"
  includedirs "/usr/include/freetype2"
  kind "StaticLib"

project "demo"
  includedirs "/usr/include/freetype2"
  kind "WindowedApp"
  files {"demo/**"}
  links "glfw"
  include_ff()
