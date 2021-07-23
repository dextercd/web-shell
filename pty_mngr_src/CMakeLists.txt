cmake_minimum_required(VERSION 3.18)

project(pty_mngr_src)

add_executable(pty_mngr
    src/main.cpp
    src/fatal.cpp)

target_compile_features(pty_mngr
    PUBLIC
        cxx_std_17)


install(TARGETS pty_mngr
    DESTINATION "pty_mngr/")
