#ifndef ANVIL_EXECUTION_ENGINE_H
#define ANVIL_EXECUTION_ENGINE_H

#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/Interpreter.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <memory>
#include <string>
#include <cstdio>   // for printf
#include <cstdlib>  // for exit
#include <unistd.h> // for write

#include "stack.h"

namespace anvil {

// Execution mode for Anvil
enum class ExecutionMode {
    JIT,          // JIT compilation (default, fast)
    Interpreter   // LLVM interpreter (slower, good for debugging)
};

// Wrapper around LLVM ExecutionEngine that supports both JIT and interpreter modes
class AnvilExecutionEngine {
private:
    llvm::ExecutionEngine* engine_;
    ExecutionMode mode_;

public:
    // Create execution engine with specified mode
    // Takes ownership of the module
    static AnvilExecutionEngine* create(std::unique_ptr<llvm::Module> module,
                                        ExecutionMode mode,
                                        std::string* error_str = nullptr) {
        llvm::EngineBuilder builder(std::move(module));

        if (error_str) {
            builder.setErrorStr(error_str);
        }

        // Set engine kind based on mode
        if (mode == ExecutionMode::Interpreter) {
            builder.setEngineKind(llvm::EngineKind::Interpreter);
        } else {
            builder.setEngineKind(llvm::EngineKind::JIT);
        }

        llvm::ExecutionEngine* engine = builder.create();

        if (!engine) {
            return nullptr;
        }

        return new AnvilExecutionEngine(engine, mode);
    }

    // Execute a compiled function with an execution context
    void execute(llvm::Function* func, ExecutionContext* ctx) {
        if (mode_ == ExecutionMode::Interpreter) {
            // Use LLVM interpreter
            // Map external functions that the interpreter needs to call
            llvm::Module* module = func->getParent();

            // Map printf
            if (llvm::Function* printf_func = module->getFunction("printf")) {
                engine_->addGlobalMapping(printf_func, (void*)&printf);
            }

            // Map fflush
            if (llvm::Function* fflush_func = module->getFunction("fflush")) {
                engine_->addGlobalMapping(fflush_func, (void*)&fflush);
            }

            // Map exit
            if (llvm::Function* exit_func = module->getFunction("exit")) {
                engine_->addGlobalMapping(exit_func, (void*)&exit);
            }

            // Map write (used by TYPE primitive)
            if (llvm::Function* write_func = module->getFunction("write")) {
                engine_->addGlobalMapping(write_func, (void*)&write);
            }

            std::vector<llvm::GenericValue> args;
            llvm::GenericValue ctx_arg;
            ctx_arg.PointerVal = ctx;
            args.push_back(ctx_arg);

            engine_->runFunction(func, args);
        } else {
            // Use JIT - get function pointer and call directly
            void (*compiled_func)(ExecutionContext*) =
                reinterpret_cast<void (*)(ExecutionContext*)>(
                    engine_->getFunctionAddress(func->getName().str())
                );

            if (compiled_func) {
                compiled_func(ctx);
            }
        }
    }

    // Get the underlying LLVM execution engine
    llvm::ExecutionEngine* get_engine() const {
        return engine_;
    }

    // Get execution mode
    ExecutionMode get_mode() const {
        return mode_;
    }

    ~AnvilExecutionEngine() {
        delete engine_;
    }

private:
    AnvilExecutionEngine(llvm::ExecutionEngine* engine, ExecutionMode mode)
        : engine_(engine), mode_(mode) {}

    // Prevent copying
    AnvilExecutionEngine(const AnvilExecutionEngine&) = delete;
    AnvilExecutionEngine& operator=(const AnvilExecutionEngine&) = delete;
};

// Initialize LLVM for the specified execution mode
inline void initialize_llvm(ExecutionMode mode) {
    static bool initialized = false;
    if (!initialized) {
        if (mode == ExecutionMode::JIT) {
            llvm::InitializeNativeTarget();
            llvm::InitializeNativeTargetAsmPrinter();
            llvm::InitializeNativeTargetAsmParser();
        }
        // Note: Interpreter mode doesn't require native target initialization
        initialized = true;
    }
}

} // namespace anvil

#endif // ANVIL_EXECUTION_ENGINE_H
