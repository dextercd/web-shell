cmake_minimum_required(VERSION 3.19)

project(web_terminal_wasm)

add_subdirectory(extern/katerm)

add_library(wasm-terminal MODULE
    src/wasm_export.cpp)

include(GenerateExportHeader)
generate_export_header(wasm-terminal
    BASE_NAME wasm)

target_include_directories(wasm-terminal
    PRIVATE ${CMAKE_SOURCE_DIR}/extern/wasm-headers/
    PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

set_target_properties(wasm-terminal PROPERTIES
    CXX_EXTENSIONS OFF
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN ON
    PREFIX ""
    SUFFIX ".wasm")

target_compile_features(wasm-terminal
    PRIVATE cxx_std_17)

target_link_libraries(wasm-terminal
    PRIVATE
        terminal-interface)

target_compile_options(wasm-terminal PUBLIC -fno-exceptions)

target_link_options(wasm-terminal PUBLIC -Wl,-gc-sections -mexec-model=reactor)

install(TARGETS wasm-terminal
    DESTINATION "wasm-terminal/")
