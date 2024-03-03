cmake_minimum_required(VERSION 3.5)

if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_CURRENT_SOURCE_DIR})

  if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
  endif(NOT CMAKE_BUILD_TYPE)

  message(
    "* build type options are : None Debug Release RelWithDebInfo MinSizeRel Sanitize"
  )
  message("* build type is : ${CMAKE_BUILD_TYPE}")

endif()

project(field_fusion C)

set(GLAD_HEADER_DIR
    "external/"
    CACHE STRING "glad header only library include directory")
set(FREETYPE_DIR
    "external/freetype-2.13.2/include"
    CACHE STRING "freetype2 include directory")

file(GLOB SRCS src/*.c src/*.h)

add_library(field_fusion STATIC ${SRCS})
target_compile_options(field_fusion PRIVATE "-Wall")
set_property(TARGET field_fusion PROPERTY C_STANDARD 11)

add_subdirectory("external/freetype-2.13.2")

target_include_directories(field_fusion
                           PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(field_fusion PRIVATE ${FREETYPE_DIR})
target_include_directories(field_fusion PRIVATE ${GLAD_HEADER_DIR})

target_link_libraries(field_fusion PUBLIC freetype)
target_link_libraries(field_fusion PUBLIC m)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/ff_shaders/ DESTINATION ff_shaders)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/ff_fonts/ DESTINATION ff_fonts)

option(BUILD_DEMO "Build demo" OFF)
if(BUILD_DEMO)
  add_subdirectory("demo/")
endif()

set(CMAKE_C_FLAGS_SANITIZE
    "-O0 -g -fsanitize=address"
    CACHE STRING "Flags used by the C compiler during sanitize builds." FORCE)
set(CMAKE_EXE_LINKER_FLAGS_SANITIZE
    "-fsanitize=address"
    CACHE STRING "Flags used for linking binaries during sanitize builds."
          FORCE)

