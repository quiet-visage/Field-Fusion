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
  filter "configurations:debug"
  defines {"DEBUG"}
  symbols "On"


  filter "configurations:release"
    optimize "Full"

project "demo"
  kind "WindowedApp"
  buildoptions {"-Wall", "-Wextra"}
  includedirs {"external/", "src/", "/usr/include/freetype2"}
  files {"demo/**"}
  links "freetype"
  links "glfw"
  CopyJetbrains()
