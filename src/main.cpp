#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <memory>
#include <cstring>
#include <vector>
#include <cstdlib>

// Platform-specific includes for cross-platform AOT compilation support
// Supported platforms: macOS, Linux, Windows
#ifdef __APPLE__
#include <mach-o/dyld.h>
#elif defined(_WIN32)
#include <windows.h>
#else
// Linux/Unix
#include <unistd.h>
#include <linux/limits.h>
#endif

#ifdef HAVE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "execution_engine.h"
#include "compiler.h"
#include "parser.h"
#include "ast.h"
#include "dictionary.h"
#include "primitives_registry.h"
#include "aot_compiler.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/Cloning.h>

using namespace anvil;

// REPL state
struct ReplState {
    ExecutionContext ctx;
    ExecutionMode mode;
    llvm::LLVMContext llvm_ctx;
    std::unique_ptr<llvm::Module> module;
    int module_counter;

    ReplState(ExecutionMode m)
        : mode(m), module_counter(0) {
        // Initialize stacks
        ctx.dsp = 0;
        ctx.rsp = 0;

        // Create initial module
        module = std::make_unique<llvm::Module>("anvil_module_0", llvm_ctx);
    }

    // Get a fresh module name
    std::string get_module_name() {
        return "anvil_module_" + std::to_string(module_counter++);
    }
};

// Print stack contents
void print_stack(const ExecutionContext& ctx) {
    if (ctx.dsp == 0) {
        std::cout << "<stack empty>";
    } else {
        std::cout << "<" << ctx.dsp << "> ";
        for (size_t i = 0; i < ctx.dsp; i++) {
            std::cout << ctx.data_stack[i];
            if (i < ctx.dsp - 1) {
                std::cout << " ";
            }
        }
    }
    std::cout << std::endl;
}

// Execute a line of Forth code
bool execute_line(const std::string& line, ReplState& state) {
    if (line.empty()) {
        return true; // Empty line
    }

    // Only skip if it's a single-line comment (doesn't contain newlines)
    if (line[0] == '\\' && line.find('\n') == std::string::npos) {
        return true; // Single-line comment
    }

    try {
        // Check for INCLUDE directive
        std::string trimmed = line;
        // Trim leading whitespace
        size_t start = trimmed.find_first_not_of(" \t\r\n");
        if (start != std::string::npos) {
            trimmed = trimmed.substr(start);
        }

        // Check if line starts with INCLUDE (case-insensitive)
        if (trimmed.size() >= 7) {
            std::string prefix = trimmed.substr(0, 7);
            std::transform(prefix.begin(), prefix.end(), prefix.begin(), ::toupper);

            // Make sure it's followed by whitespace or end of string (not part of a larger word)
            bool valid_include = (prefix == "INCLUDE") &&
                                 (trimmed.size() == 7 || std::isspace(trimmed[7]));

            if (valid_include) {
                // Extract filename (everything after INCLUDE, trimmed)
                std::string filename = trimmed.substr(7);
                // Trim whitespace from filename
                size_t file_start = filename.find_first_not_of(" \t\r\n");
                size_t file_end = filename.find_last_not_of(" \t\r\n");
                if (file_start != std::string::npos && file_end != std::string::npos) {
                    filename = filename.substr(file_start, file_end - file_start + 1);

                    // Load and execute the file
                    std::ifstream file(filename);
                    if (!file.is_open()) {
                        std::cerr << "Error: Could not open file: " << filename << "\n";
                        return true;
                    }

                    // Read entire file and expand nested INCLUDEs
                    std::stringstream expanded_source;
                    std::string line;
                    while (std::getline(file, line)) {
                        // Check if this line is an INCLUDE directive
                        std::string trimmed_line = line;
                        size_t trim_start = trimmed_line.find_first_not_of(" \t\r\n");
                        if (trim_start != std::string::npos) {
                            trimmed_line = trimmed_line.substr(trim_start);
                        }

                        if (trimmed_line.size() >= 7) {
                            std::string prefix = trimmed_line.substr(0, 7);
                            std::transform(prefix.begin(), prefix.end(), prefix.begin(), ::toupper);
                            bool is_include = (prefix == "INCLUDE") &&
                                            (trimmed_line.size() == 7 || std::isspace(trimmed_line[7]));

                            if (is_include) {
                                // Extract nested filename
                                std::string nested_file = trimmed_line.substr(7);
                                size_t nested_start = nested_file.find_first_not_of(" \t\r\n");
                                size_t nested_end = nested_file.find_last_not_of(" \t\r\n");
                                if (nested_start != std::string::npos && nested_end != std::string::npos) {
                                    nested_file = nested_file.substr(nested_start, nested_end - nested_start + 1);

                                    // Recursively load nested file
                                    std::ifstream nested(nested_file);
                                    if (nested.is_open()) {
                                        expanded_source << nested.rdbuf();
                                        nested.close();
                                    } else {
                                        std::cerr << "Error: Could not open nested file: " << nested_file << "\n";
                                    }
                                }
                                continue;
                            }
                        }

                        // Not an INCLUDE line, add it to source
                        expanded_source << line << "\n";
                    }
                    file.close();

                    std::string source = expanded_source.str();

                    // Parse the entire file as one unit
                    ASTBuilder file_builder;
                    auto file_ast = file_builder.parse(source);

                    if (!file_ast) {
                        std::cerr << "Parse error in file: " << filename << "\n";
                        return true;
                    }

                    // Compile it
                    Compiler compiler(state.llvm_ctx, *state.module);
                    llvm::Function* func = compiler.compile(file_ast.get());

                    if (!func) {
                        std::cerr << "Compilation error in file: " << filename << "\n";
                        return true;
                    }

                    // Verify function
                    std::string error_str;
                    llvm::raw_string_ostream error_stream(error_str);
                    if (llvm::verifyFunction(*func, &error_stream)) {
                        std::cerr << "Verification failed in file " << filename << ": " << error_str << "\n";
                        return true;
                    }

                    // Check if this is a definition or executable code
                    bool is_definition = (func->getName().str().find("__main") == std::string::npos);

                    if (is_definition) {
                        // Definition was added to dictionary
                        std::cout << "ok\n";
                    } else {
                        // Execute the code
                        auto module_clone = llvm::CloneModule(*state.module);

                        std::string engine_error;
                        AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
                            std::move(module_clone), state.mode, &engine_error);

                        if (!engine) {
                            std::cerr << "Engine creation failed: " << engine_error << "\n";
                            return true;
                        }

                        engine->execute(func, &state.ctx);
                        delete engine;
                        std::cout << "ok\n";
                    }

                    return true;
                } else {
                    std::cerr << "Error: INCLUDE requires a filename\n";
                    return true;
                }
            }
        }

        // Parse to AST
        ASTBuilder builder;
        auto ast = builder.parse(line);

        if (!ast) {
            std::cerr << "Parse error\n";
            return true;
        }

        // Use the shared module so all definitions are in the same module
        Compiler compiler(state.llvm_ctx, *state.module);
        llvm::Function* func = compiler.compile(ast.get());

        if (!func) {
            std::cerr << "Compilation error\n";
            return true;
        }

        // Verify function
        std::string error_str;
        llvm::raw_string_ostream error_stream(error_str);
        if (llvm::verifyFunction(*func, &error_stream)) {
            std::cerr << "Verification failed: " << error_str << "\n";
            return true;
        }

        // Check if this is a definition (starts with function name, not "__main")
        bool is_definition = (func->getName().str().find("__main") == std::string::npos);

        if (is_definition) {
            // Definition was added to dictionary, no need to execute
            std::cout << "ok\n";
        } else {
            // Regular code - create execution engine and run
            // Note: We can't move the module since we need it for future definitions
            // So we create a clone for execution
            auto module_clone = llvm::CloneModule(*state.module);

            std::string engine_error;
            AnvilExecutionEngine* engine = AnvilExecutionEngine::create(
                std::move(module_clone), state.mode, &engine_error);

            if (!engine) {
                std::cerr << "Engine creation failed: " << engine_error << "\n";
                return true;
            }

            // Execute with current context
            engine->execute(func, &state.ctx);

            delete engine;

            // Print " ok" after execution (space ensures separation from any output)
            std::cout << " ok\n";
        }

    } catch (const std::exception& e) {
        // Don't use cursor positioning on error - just print error normally
        std::cout << "\n";  // Move to new line first
        std::cerr << "Error: " << e.what() << "\n";
        // Reset stacks to recover from error
        state.ctx.dsp = 0;
        state.ctx.rsp = 0;

        // Clean up any partially-created __main functions from the module
        std::vector<llvm::Function*> to_remove;
        for (auto& func : state.module->functions()) {
            if (func.getName().contains("__main")) {
                to_remove.push_back(&func);
            }
        }
        for (auto* func : to_remove) {
            func->eraseFromParent();
        }
    }

    return true;
}

// Get the directory containing the anvil executable
std::string get_executable_dir() {
    char path[1024];

#ifdef __APPLE__
    uint32_t size = sizeof(path);
    if (_NSGetExecutablePath(path, &size) == 0) {
        std::string exe_path(path);
        size_t last_slash = exe_path.find_last_of('/');
        if (last_slash != std::string::npos) {
            return exe_path.substr(0, last_slash);
        }
    }
#elif defined(_WIN32)
    if (GetModuleFileNameA(NULL, path, sizeof(path)) != 0) {
        std::string exe_path(path);
        size_t last_slash = exe_path.find_last_of("\\/");
        if (last_slash != std::string::npos) {
            return exe_path.substr(0, last_slash);
        }
    }
#else
    // Linux/Unix
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len != -1) {
        path[len] = '\0';
        std::string exe_path(path);
        size_t last_slash = exe_path.find_last_of('/');
        if (last_slash != std::string::npos) {
            return exe_path.substr(0, last_slash);
        }
    }
#endif

    return ".";
}

// Helper to create a temporary directory
std::string create_temp_dir() {
#ifdef _WIN32
    char temp_path[MAX_PATH];
    char temp_dir[MAX_PATH];
    if (GetTempPathA(MAX_PATH, temp_path) == 0) {
        return "";
    }
    if (GetTempFileNameA(temp_path, "anvil", 0, temp_dir) == 0) {
        return "";
    }
    // GetTempFileName creates a file, we need a directory
    DeleteFileA(temp_dir);
    if (!CreateDirectoryA(temp_dir, NULL)) {
        return "";
    }
    return std::string(temp_dir);
#else
    char temp_dir[] = "/tmp/anvil_build_XXXXXX";
    if (!mkdtemp(temp_dir)) {
        return "";
    }
    return std::string(temp_dir);
#endif
}

// Helper to remove a directory recursively
bool remove_directory(const std::string& dir) {
#ifdef _WIN32
    std::string cmd = "rmdir /S /Q \"" + dir + "\"";
    return std::system(cmd.c_str()) == 0;
#else
    std::string cmd = "rm -rf \"" + dir + "\"";
    return std::system(cmd.c_str()) == 0;
#endif
}

// Helper to copy a file
bool copy_file(const std::string& src, const std::string& dst) {
#ifdef _WIN32
    return CopyFileA(src.c_str(), dst.c_str(), FALSE) != 0;
#else
    std::string cmd = "cp \"" + src + "\" \"" + dst + "\"";
    return std::system(cmd.c_str()) == 0;
#endif
}

// Helper to make file executable (no-op on Windows)
void make_executable(const std::string& path) {
#ifndef _WIN32
    std::string cmd = "chmod +x \"" + path + "\"";
    std::system(cmd.c_str());
#endif
}

// Get platform-specific path separator
std::string get_path_sep() {
#ifdef _WIN32
    return "\\";
#else
    return "/";
#endif
}

// Get platform-specific compiler command
std::string get_compiler_cmd() {
#ifdef _WIN32
    // Try cl.exe (MSVC), then gcc, then clang
    if (std::system("where cl.exe >nul 2>&1") == 0) {
        return "cl.exe";
    } else if (std::system("where gcc.exe >nul 2>&1") == 0) {
        return "gcc.exe";
    } else if (std::system("where clang.exe >nul 2>&1") == 0) {
        return "clang.exe";
    }
    return "cc"; // fallback
#else
    // Unix: prefer cc, but try gcc and clang as fallbacks
    if (std::system("which cc >/dev/null 2>&1") == 0) {
        return "cc";
    } else if (std::system("which gcc >/dev/null 2>&1") == 0) {
        return "gcc";
    } else if (std::system("which clang >/dev/null 2>&1") == 0) {
        return "clang";
    }
    return "cc"; // fallback
#endif
}

// Get platform-specific runtime library name
std::string get_runtime_lib_name() {
#ifdef _WIN32
    return "anvil_runtime.lib";
#else
    return "libanvil_runtime.a";
#endif
}

// Compile Forth source file to executable
bool compile_file(const std::string& input_file, const std::string& output_file) {
    // Read input file
    std::ifstream file(input_file);
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file: " << input_file << "\n";
        return false;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    // Parse to AST
    ASTBuilder builder;
    auto ast = builder.parse(source);

    if (!ast) {
        std::cerr << "Parse error\n";
        return false;
    }

    // Compile to LLVM IR
    llvm::LLVMContext llvm_ctx;
    llvm::Module module("anvil_aot", llvm_ctx);
    Compiler compiler(llvm_ctx, module);

    // Load standard library first
    std::string exe_dir = get_executable_dir();
    std::string stdlib_path = exe_dir + get_path_sep() + "stdlib.fth";
    std::ifstream stdlib_file(stdlib_path);
    if (stdlib_file.is_open()) {
        std::stringstream stdlib_buffer;
        stdlib_buffer << stdlib_file.rdbuf();
        std::string stdlib_source = stdlib_buffer.str();
        stdlib_file.close();

        // Parse and compile stdlib
        ASTBuilder stdlib_builder;
        auto stdlib_ast = stdlib_builder.parse(stdlib_source);
        if (stdlib_ast) {
            compiler.compile(stdlib_ast.get());
        }
    }

    // Compile the code
    llvm::Function* func = compiler.compile(ast.get());

    if (!func) {
        std::cerr << "Compilation error\n";
        return false;
    }

    // Rename the function to __anvil_main for the runtime
    func->setName("__anvil_main");

    // Verify function
    std::string error_str;
    llvm::raw_string_ostream error_stream(error_str);
    if (llvm::verifyFunction(*func, &error_stream)) {
        std::cerr << "Verification failed: " << error_str << "\n";
        return false;
    }

    // Create temporary directory for build artifacts
    std::string temp_dir = create_temp_dir();
    if (temp_dir.empty()) {
        std::cerr << "Error: Could not create temporary directory\n";
        return false;
    }

    std::string path_sep = get_path_sep();
    std::string temp_obj = temp_dir + path_sep + "program.o";
    std::string temp_exe = temp_dir + path_sep + "program";
#ifdef _WIN32
    temp_exe += ".exe";
#endif

    // Compile to object file
    std::string error_msg;
    if (!compile_to_object(&module, temp_obj, &error_msg)) {
        std::cerr << "Object file generation failed: " << error_msg << "\n";
        remove_directory(temp_dir);
        return false;
    }

    // Find runtime library
    std::string runtime_lib = exe_dir + path_sep + get_runtime_lib_name();

    // Link with runtime library
    std::string compiler_cmd = get_compiler_cmd();
    std::string link_cmd = "\"" + compiler_cmd + "\" \"" + temp_obj + "\" \"" + runtime_lib + "\" -o \"" + temp_exe + "\" 2>&1";

    FILE* link_process = popen(link_cmd.c_str(), "r");
    if (!link_process) {
        std::cerr << "Error: Could not run linker\n";
        remove_directory(temp_dir);
        return false;
    }

    char link_output[256];
    bool link_had_output = false;
    while (fgets(link_output, sizeof(link_output), link_process)) {
        if (!link_had_output) {
            std::cerr << "Linker output:\n";
            link_had_output = true;
        }
        std::cerr << link_output;
    }

    int link_status = pclose(link_process);
    if (link_status != 0) {
        std::cerr << "Error: Linking failed\n";
        remove_directory(temp_dir);
        return false;
    }

    // Copy executable to output location
    if (!copy_file(temp_exe, output_file)) {
        std::cerr << "Error: Could not copy executable to " << output_file << "\n";
        remove_directory(temp_dir);
        return false;
    }

    // Make executable (Unix only)
    make_executable(output_file);

    // Cleanup temporary directory
    remove_directory(temp_dir);

    std::cout << "Successfully compiled " << input_file << " to " << output_file << "\n";
    return true;
}

// Print usage information
void print_usage(const char* program_name) {
    std::cout << "Usage: " << program_name << " [OPTIONS] [FILE]\n\n";
    std::cout << "Options:\n";
    std::cout << "  --no-jit          Use LLVM interpreter instead of JIT (slower, good for debugging)\n";
    std::cout << "  --jit             Use JIT compilation (default, faster)\n";
    std::cout << "  --compile FILE    Compile FILE to executable\n";
    std::cout << "  -o OUTPUT         Specify output file for --compile (default: a.out)\n";
    std::cout << "  --help, -h        Show this help message\n\n";
    std::cout << "Interactive REPL commands:\n";
    std::cout << "  .s                Show stack contents\n";
    std::cout << "  quit, exit, bye   Exit the REPL\n";
    std::cout << "  help              Show help\n\n";
    std::cout << "Examples:\n";
    std::cout << "  " << program_name << "                    # Start interactive REPL\n";
    std::cout << "  " << program_name << " --compile prog.fth     # Compile to a.out\n";
    std::cout << "  " << program_name << " --compile prog.fth -o prog  # Compile to prog\n\n";
}

int main(int argc, char** argv) {
    // Parse command-line arguments
    ExecutionMode mode = ExecutionMode::JIT; // Default
    bool compile_mode = false;
    std::string input_file;
    std::string output_file = "a.out";

    for (int i = 1; i < argc; i++) {
        if (std::strcmp(argv[i], "--no-jit") == 0) {
            mode = ExecutionMode::Interpreter;
        } else if (std::strcmp(argv[i], "--jit") == 0) {
            mode = ExecutionMode::JIT;
        } else if (std::strcmp(argv[i], "--compile") == 0) {
            compile_mode = true;
            if (i + 1 < argc) {
                input_file = argv[++i];
            } else {
                std::cerr << "Error: --compile requires an input file\n\n";
                print_usage(argv[0]);
                return 1;
            }
        } else if (std::strcmp(argv[i], "-o") == 0) {
            if (i + 1 < argc) {
                output_file = argv[++i];
            } else {
                std::cerr << "Error: -o requires an output file\n\n";
                print_usage(argv[0]);
                return 1;
            }
        } else if (std::strcmp(argv[i], "--help") == 0 || std::strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else {
            std::cerr << "Unknown option: " << argv[i] << "\n\n";
            print_usage(argv[0]);
            return 1;
        }
    }

    // Handle compile mode
    if (compile_mode) {
        initialize_aot();
        initialize_primitives();
        return compile_file(input_file, output_file) ? 0 : 1;
    }

    // Initialize LLVM and primitives for REPL mode
    initialize_llvm(mode);
    initialize_primitives();

    // Disable buffering for interpreter mode to see output immediately
    if (mode == ExecutionMode::Interpreter) {
        setvbuf(stdout, NULL, _IONBF, 0);
    }

    // Print banner
    std::cout << "Anvil Forth Compiler (LLVM " << LLVM_VERSION_STRING << ")\n";
    std::cout << "Mode: " << (mode == ExecutionMode::JIT ? "JIT" : "Interpreter") << "\n";
    std::cout << "Type help for help, quit to exit\n";

    // Create REPL state
    ReplState state(mode);

    // Load standard library silently (suppress "ok" output)
    std::string exe_dir = get_executable_dir();
    std::string path_sep = get_path_sep();
    std::string stdlib_path = exe_dir + path_sep + "stdlib.fth";
    std::ifstream stdlib_file(stdlib_path);
    if (stdlib_file.is_open()) {
        std::stringstream buffer;
        buffer << stdlib_file.rdbuf();
        std::string stdlib_source = buffer.str();
        stdlib_file.close();

        // Redirect stdout to suppress "ok" messages during stdlib load
        std::streambuf* old_cout = std::cout.rdbuf();
        std::ostringstream null_stream;
        std::cout.rdbuf(null_stream.rdbuf());

        execute_line(stdlib_source, state);

        // Restore stdout
        std::cout.rdbuf(old_cout);
    }
    // If stdlib doesn't exist, continue without it

    // REPL loop
    using_history();

    while (true) {
        char* input = readline("");  // Empty prompt - we'll echo input ourselves

        // EOF (Ctrl-D)
        if (!input) {
            std::cout << "\n";
            break;
        }

        std::string line(input);
        free(input);

        // Handle exit commands immediately (no echo, no history)
        if (line == "quit" || line == "exit" || line == "bye") {
            break;
        }

        // Add non-empty lines to history (but not exit commands)
        if (!line.empty()) {
            add_history(line.c_str());
        }

        // Handle special REPL commands
        if (line == ".s") {
            std::cout << line << "\n";
            print_stack(state.ctx);
            continue;
        } else if (line == "help") {
            std::cout << line << "\n";
            std::cout << "REPL commands:\n";
            std::cout << "  .s          Show stack\n";
            std::cout << "  quit        Exit\n";
            std::cout << "  help        Show this help\n";
            continue;
        }

        // Echo input with cursor positioning for normal commands
        std::cout << "\033[1A\033[2K" << line;  // Move up, clear line, print input
        if (!line.empty()) {
            std::cout << " ";  // Space before output
        }

        if (!execute_line(line, state)) {
            break;
        }
    }

    return 0;
}
