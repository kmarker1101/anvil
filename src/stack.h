#ifndef ANVIL_STACK_H
#define ANVIL_STACK_H

#include <cstddef>
#include <cstdint>

namespace anvil {

// Stack sizes (in cells, where a cell is 64 bits)
constexpr size_t DATA_STACK_SIZE = 1024;
constexpr size_t RETURN_STACK_SIZE = 1024;
constexpr size_t DATA_SPACE_SIZE = 65536;  // 64KB data space for variables

// Execution context containing both Forth stacks and data space
struct ExecutionContext {
    int64_t data_stack[DATA_STACK_SIZE];
    int64_t return_stack[RETURN_STACK_SIZE];
    uint8_t data_space[DATA_SPACE_SIZE];  // Memory for variables/allotted space

    // Stack pointers (point to next free slot)
    size_t dsp;  // Data stack pointer
    size_t rsp;  // Return stack pointer
    size_t here; // Data space pointer (next free byte)

    ExecutionContext() : dsp(0), rsp(0), here(0) {}
};

} // namespace anvil

#endif // ANVIL_STACK_H
