cmake_minimum_required(VERSION 3.15)
project(aoc19_f00ale)

set(CMAKE_CXX_STANDARD 17)

add_compile_options(-Wall -Wextra -march=native)

add_executable(stub src/stub.cpp)

add_executable(p01 src/p01.cpp)
add_executable(p02 src/p02.cpp)
add_executable(p03 src/p03.cpp)
add_executable(p04 src/p04.cpp)
add_executable(p05 src/p05.cpp)
add_executable(p06 src/p06.cpp)
