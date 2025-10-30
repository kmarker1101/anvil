// Minimal runtime for AOT-compiled Anvil programs
// This provides only the execution context - no REPL, no JIT, no interpreter

#include <stdint.h>
#include <stddef.h>

// Stack sizes
#define DATA_STACK_SIZE 1024
#define RETURN_STACK_SIZE 1024

// Execution context - must match stack.h layout
typedef struct {
    int64_t data_stack[DATA_STACK_SIZE];
    int64_t return_stack[RETURN_STACK_SIZE];
    size_t dsp;
    size_t rsp;
} ExecutionContext;

// Forward declaration of the compiled main function
extern void __anvil_main(ExecutionContext* ctx);

// Main entry point
int main(int argc, char** argv) {
    // Initialize execution context
    ExecutionContext ctx;
    ctx.dsp = 0;
    ctx.rsp = 0;

    // Execute compiled Forth code
    __anvil_main(&ctx);

    return 0;
}
