cmake_minimum_required(VERSION 3.5)

project(field_fusion_demo C)

option(SANITIZE "address sanitize build" OFF)
if (SANITIZE)
  add_compile_options(-fsanitize=address)
  add_link_options(-fsanitize=address)
endif(SANITIZE)

add_executable(${PROJECT_NAME} "demo.c")
set_property(TARGET field_fusion_demo PROPERTY C_STANDARD 11)

target_link_libraries(${PROJECT_NAME} PUBLIC field_fusion)
target_link_libraries(${PROJECT_NAME} PUBLIC glfw)
target_include_directories(${PROJECT_NAME} PRIVATE ${CMAKE_SOURCE_DIR}/src)


