#ifndef ANVIL_STACK_H
#define ANVIL_STACK_H

#include <cstddef>
#include <cstdint>

namespace anvil {

// Stack sizes (in cells, where a cell is 64 bits)
constexpr size_t DATA_STACK_SIZE = 1024;
constexpr size_t RETURN_STACK_SIZE = 1024;

// Execution context containing both Forth stacks
struct ExecutionContext {
    int64_t data_stack[DATA_STACK_SIZE];
    int64_t return_stack[RETURN_STACK_SIZE];

    // Stack pointers (point to next free slot)
    size_t dsp;  // Data stack pointer
    size_t rsp;  // Return stack pointer

    ExecutionContext() : dsp(0), rsp(0) {}
};

} // namespace anvil

#endif // ANVIL_STACK_H
