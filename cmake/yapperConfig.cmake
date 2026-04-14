cmake_minimum_required(VERSION 3.15)

get_filename_component(_YAPPER_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" DIRECTORY)
get_filename_component(_YAPPER_ROOT "${_YAPPER_CMAKE_DIR}/.." ABSOLUTE)

if(CMAKE_HOST_WIN32)
    set(_YAPPER_BIN_NAME "yapper.exe")
else()
    set(_YAPPER_BIN_NAME "yapper")
endif()

set(_YAPPER_BIN "${_YAPPER_ROOT}/target/release/${_YAPPER_BIN_NAME}")

# Build the binary at configure time if it isn't already present.
if(NOT EXISTS "${_YAPPER_BIN}")
    find_program(CARGO_EXECUTABLE cargo)
    if(NOT CARGO_EXECUTABLE)
        message(FATAL_ERROR
            "yapper: cargo not found in PATH and ${_YAPPER_BIN} is missing. "
            "Install a Rust toolchain or pre-build yapper with `cargo build --release`.")
    endif()
    message(STATUS "yapper: building release binary (cargo build --release)")
    execute_process(
        COMMAND "${CARGO_EXECUTABLE}" build --release
        WORKING_DIRECTORY "${_YAPPER_ROOT}"
        RESULT_VARIABLE _yapper_build_result)
    if(NOT _yapper_build_result EQUAL 0)
        message(FATAL_ERROR "yapper: cargo build --release failed (exit ${_yapper_build_result})")
    endif()
endif()

if(NOT EXISTS "${_YAPPER_BIN}")
    message(FATAL_ERROR "yapper: expected binary at ${_YAPPER_BIN} after build, but it is missing")
endif()

if(NOT TARGET yapper::yapper)
    add_executable(yapper::yapper IMPORTED GLOBAL)
    set_target_properties(yapper::yapper PROPERTIES
        IMPORTED_LOCATION "${_YAPPER_BIN}")
endif()

# yapper_generate_lexer(<input> <output>)
#
# Adds a build-time rule that runs yapper on <input> and writes the generated
# C source to <output>. Returns a custom command whose OUTPUT is the absolute
# path of the generated file; add that path to a target's sources (or to a
# custom target's DEPENDS) to wire it into the build graph.
#
# <input>  is resolved relative to CMAKE_CURRENT_SOURCE_DIR.
# <output> is resolved relative to CMAKE_CURRENT_BINARY_DIR.
function(yapper_generate_lexer INPUT OUTPUT)
    get_filename_component(_in_abs "${INPUT}" ABSOLUTE
        BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
    get_filename_component(_out_abs "${OUTPUT}" ABSOLUTE
        BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}")
    get_filename_component(_out_dir "${_out_abs}" DIRECTORY)

    add_custom_command(
        OUTPUT "${_out_abs}"
        COMMAND "${CMAKE_COMMAND}" -E make_directory "${_out_dir}"
        COMMAND yapper::yapper "${_in_abs}" "${_out_abs}"
        DEPENDS "${_in_abs}" yapper::yapper
        COMMENT "yapper: generating ${OUTPUT} from ${INPUT}"
        VERBATIM)
endfunction()

set(yapper_FOUND TRUE)
