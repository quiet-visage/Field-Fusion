function IncludeFf()
  includedirs "src/"
  links "fieldfusion"
end

function CopyHeaders()
  postbuildcommands { "{MKDIR} %{cfg.buildtarget.directory}/include" }
  postbuildcommands { "{COPY} src/*.hh %{cfg.buildtarget.directory}/include" }
end

function CopyJetbrains()
  postbuildcommands { "{COPY} fonts/jetbrainsfont %{cfg.buildtarget.directory}/" }
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
  CopyHeaders()

project "demo"
  includedirs "/usr/include/freetype2"
  kind "WindowedApp"
  files {"demo/**"}
  links "glfw"
  IncludeFf()
  CopyJetbrains()
