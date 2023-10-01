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
    "glfw",
  }
  filter "configurations:Debug"
  defines {"DEBUG"}
  symbols "On"

  filter "configurations:Release"
    optimize "Full"

project "fieldfusion"
  files "src/**"
  kind "StaticLib"
  includedirs "/usr/include/freetype2"

project "demo"
  kind "WindowedApp"
  files {"demo/**"}
  includedirs {"/usr/include/freetype2"}
  include_ff()
