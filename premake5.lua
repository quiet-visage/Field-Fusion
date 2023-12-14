function CopyHeaders()
  postbuildcommands { "{MKDIR} %{cfg.buildtarget.directory}/include" }
  postbuildcommands { "{COPY} src/*.hh %{cfg.buildtarget.directory}/include" }
end

function CopyJetbrains()
  postbuildcommands { "{COPY} fonts/jetbrainsfont %{cfg.buildtarget.directory}/" }
end

workspace "fieldfusion"
  language "c"
  cdialect "c11"
  configurations {"address_sanitize", "release", "debug"}

  filter "configurations:address_sanitize"
  buildoptions {"-fsanitize=address", "-g3"}
  linkoptions {"-fsanitize=address"}
  symbols "On"

  filter "configurations:debug"
  defines {"DEBUG"}
  symbols "On"

  filter "configurations:release"
    optimize "Full"

project "fieldfusion"
  kind "StaticLib"
  configurations {"release", "debug"}
  includedirs "/usr/include/freetype2"
  includedirs "external/"
  files "src/**"
  links "freetype"
  links 'm'


project "demo"
  kind "WindowedApp"
  buildoptions {"-Wall", "-Wextra"}
  includedirs {"external/", "src/", "/usr/include/freetype2"}
  files {"demo/**"}
  links "freetype"
  links "glfw"
  links "fieldfusion"
  links 'm'
  CopyJetbrains()
