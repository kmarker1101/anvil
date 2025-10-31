cmake_minimum_required(VERSION 3.13)
project(anvil)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find dependencies
# zstd is required because this system's LLVM was built with zstd support
find_package(zstd REQUIRED QUIET)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Add LLVM include directories
include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# Link against LLVM libraries
llvm_map_components_to_libnames(llvm_libs
    core
    executionengine
    interpreter
    mcjit
    native
    support
    target
    aarch64codegen
    aarch64asmparser
    aarch64desc
    aarch64info
)

# Fetch Catch2 for testing
include(FetchContent)
FetchContent_Declare(
    Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG v3.5.2
)
FetchContent_MakeAvailable(Catch2)

# Enable testing
enable_testing()

# Find readline
find_library(READLINE_LIBRARY NAMES readline)
find_path(READLINE_INCLUDE_DIR readline/readline.h)

if(NOT READLINE_LIBRARY)
    message(WARNING "readline library not found - REPL will use basic input")
endif()

# Main executable
add_executable(anvil
    src/main.cpp
    src/dictionary.cpp
    src/primitives_registry.cpp
    src/execution_engine.cpp
    src/runtime_helpers.cpp
    src/number_parser.cpp
    src/terminal.c
)
target_link_libraries(anvil ${llvm_libs})
target_include_directories(anvil PRIVATE src)

# Link readline if found
if(READLINE_LIBRARY)
    target_link_libraries(anvil ${READLINE_LIBRARY})
    target_include_directories(anvil PRIVATE ${READLINE_INCLUDE_DIR})
    target_compile_definitions(anvil PRIVATE HAVE_READLINE)
endif()

# Test executable
add_executable(anvil_tests
    tests/test_primitives.cpp
    tests/test_input_buffer.cpp
    tests/test_string_primitives.cpp
    tests/test_dictionary.cpp
    tests/test_parser.cpp
    tests/test_ast.cpp
    tests/test_compiler.cpp
    tests/test_interpreter.cpp
    tests/test_stdlib.cpp
    tests/test_variables.cpp
    tests/test_emit.cpp
    tests/test_terminal.cpp
    src/dictionary.cpp
    src/primitives_registry.cpp
    src/execution_engine.cpp
    src/runtime_helpers.cpp
    src/number_parser.cpp
    src/terminal.c
)
target_link_libraries(anvil_tests PRIVATE Catch2::Catch2WithMain ${llvm_libs})
target_include_directories(anvil_tests PRIVATE src)

# Register tests with CTest
include(CTest)
include(Catch)
catch_discover_tests(anvil_tests)

# Minimal runtime library for AOT-compiled programs
add_library(anvil_runtime STATIC src/runtime.c src/terminal.c)
target_include_directories(anvil_runtime PUBLIC src)

# Concatenate stdlib files in dependency order
set(STDLIB_FILES
    ${CMAKE_SOURCE_DIR}/forth/stdlib/00-stack.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/10-arithmetic.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/20-comparison.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/22-arithmetic-extended.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/25-stack-advanced.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/30-memory.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/40-strings.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/50-io.fth
    ${CMAKE_SOURCE_DIR}/forth/stdlib/60-parsing.fth
)

add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/stdlib.fth
    COMMAND ${CMAKE_COMMAND} -E cat ${STDLIB_FILES} > ${CMAKE_BINARY_DIR}/stdlib.fth
    DEPENDS ${STDLIB_FILES}
    COMMENT "Concatenating stdlib files"
)

# Create a custom target that depends on the concatenated file
add_custom_target(copy_stdlib ALL
    DEPENDS ${CMAKE_BINARY_DIR}/stdlib.fth
)
