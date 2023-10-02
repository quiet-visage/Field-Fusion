function include_ff()
  includedirs "src/"
  links "fieldfusion"
end

workspace "fieldfusion"
  language "C++"
  cppdialect "C++17"
  configurations {"Release", "Debug"}
  links {
    "GLEW",
    "EGL", 
    "GL",  
    "freetype",
    "GLU", 
  }
  filter "configurations:Debug"
  defines {"DEBUG"}
  symbols "On"
  includedirs "/usr/include/freetype2"

  filter "configurations:Release"
    optimize "Full"

project "fieldfusion"
  files "src/**"
  include_freetype()
  kind "StaticLib"

project "demo"
  kind "WindowedApp"
  files {"demo/**"}
  links "glfw"
  include_ff()
