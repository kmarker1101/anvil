#include <iostream>
#include <string>
#include <memory>
#include <cstring>
#include <vector>

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
    if (line.empty() || line[0] == '\\') {
        return true; // Comment or empty line
    }

    try {
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

            // Print "ok" after execution (no leading space - already printed by caller)
            std::cout << "ok\n";
        }

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
    }

    return true;
}

// Print usage information
void print_usage(const char* program_name) {
    std::cout << "Usage: " << program_name << " [OPTIONS]\n\n";
    std::cout << "Options:\n";
    std::cout << "  --no-jit          Use LLVM interpreter instead of JIT (slower, good for debugging)\n";
    std::cout << "  --jit             Use JIT compilation (default, faster)\n";
    std::cout << "  --help, -h        Show this help message\n\n";
    std::cout << "Interactive REPL commands:\n";
    std::cout << "  .s                Show stack contents\n";
    std::cout << "  quit, exit, bye   Exit the REPL\n";
    std::cout << "  help              Show help\n\n";
}

int main(int argc, char** argv) {
    // Parse command-line arguments
    ExecutionMode mode = ExecutionMode::JIT; // Default

    for (int i = 1; i < argc; i++) {
        if (std::strcmp(argv[i], "--no-jit") == 0) {
            mode = ExecutionMode::Interpreter;
        } else if (std::strcmp(argv[i], "--jit") == 0) {
            mode = ExecutionMode::JIT;
        } else if (std::strcmp(argv[i], "--help") == 0 || std::strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        } else {
            std::cerr << "Unknown option: " << argv[i] << "\n\n";
            print_usage(argv[0]);
            return 1;
        }
    }

    // Initialize LLVM and primitives
    initialize_llvm(mode);
    initialize_primitives();

    // Disable buffering for interpreter mode to see output immediately
    if (mode == ExecutionMode::Interpreter) {
        setvbuf(stdout, NULL, _IONBF, 0);
    }

    // Print banner
    std::cout << "Anvil Forth Compiler (LLVM " << LLVM_VERSION_STRING << ")\n";
    std::cout << "Mode: " << (mode == ExecutionMode::JIT ? "JIT" : "Interpreter") << "\n";
    std::cout << "Type help for help, quit to exit\n\n";

    // Create REPL state
    ReplState state(mode);

    // REPL loop
#ifdef HAVE_READLINE
    // Use readline with history
    using_history();

    while (true) {
        char* input = readline("> ");

        // EOF (Ctrl-D)
        if (!input) {
            std::cout << "\n";
            break;
        }

        std::string line(input);

        // Add non-empty lines to history
        if (!line.empty()) {
            add_history(input);
        }

        free(input);

        // Handle special REPL commands
        if (line == "quit" || line == "exit" || line == "bye") {
            break;
        } else if (line == ".s") {
            print_stack(state.ctx);
            continue;
        } else if (line == "help") {
            std::cout << "REPL commands:\n";
            std::cout << "  .s          Show stack\n";
            std::cout << "  quit        Exit\n";
            std::cout << "  help        Show this help\n";
            continue;
        }

        if (!execute_line(line, state)) {
            break;
        }
    }
#else
    // Fallback to basic input without readline
    std::string line;
    while (true) {
        std::cout << "> ";
        std::cout.flush();

        if (!std::getline(std::cin, line)) {
            std::cout << "\n";
            break; // EOF
        }

        // Handle special REPL commands
        if (line == "quit" || line == "exit" || line == "bye") {
            break;
        } else if (line == ".s") {
            print_stack(state.ctx);
            continue;
        } else if (line == "help") {
            std::cout << "REPL commands:\n";
            std::cout << "  .s          Show stack\n";
            std::cout << "  quit        Exit\n";
            std::cout << "  help        Show this help\n";
            continue;
        }

        if (!execute_line(line, state)) {
            break;
        }
    }
#endif

    std::cout << "Goodbye!\n";
    return 0;
}
