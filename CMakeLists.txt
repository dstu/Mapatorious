cmake_minimum_required(VERSION 2.6)

set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)
include(MacroEnsureOutOfSourceBuild)

macro_ensure_out_of_source_build("Don't build directly in the source tree. Move to the build directory and run 'cmake .. [options]' there.")

add_definitions(-std=c++0x)

include_directories(include)

add_subdirectory(src)
