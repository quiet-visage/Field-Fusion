cmake_minimum_required(VERSION 3.5)

project(field_fusion_demo C)

option(SANITIZE "address sanitize build" OFF)
if(SANITIZE)
  add_compile_options(-fsanitize=address)
  add_link_options(-fsanitize=address)
endif(SANITIZE)

add_executable(${PROJECT_NAME} "demo.c")
set_property(TARGET field_fusion_demo PROPERTY C_STANDARD 11)

add_subdirectory(external/glfw-3.4)
target_include_directories(
  ${PROJECT_NAME} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src
                          external/glfw-3.4/include)

target_link_libraries(${PROJECT_NAME} PRIVATE field_fusion)
target_link_libraries(${PROJECT_NAME} PRIVATE glfw)
