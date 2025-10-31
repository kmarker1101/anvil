#ifndef ANVIL_EXECUTION_CONTEXT_LAYOUT_H
#define ANVIL_EXECUTION_CONTEXT_LAYOUT_H

#include "stack.h"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DerivedTypes.h>

namespace anvil {

// Field indices for ExecutionContext struct
// IMPORTANT: These must match the field order in stack.h ExecutionContext
enum ExecutionContextField {
    CTX_DATA_STACK = 0,
    CTX_RETURN_STACK = 1,
    CTX_DATA_SPACE = 2,
    CTX_TIB = 3,
    CTX_DSP = 4,
    CTX_RSP = 5,
    CTX_HERE = 6,
    CTX_TO_IN = 7,
    CTX_NUM_TIB = 8,
    CTX_FIELD_COUNT = 9
};

// Helper to create LLVM StructType for ExecutionContext
// This ensures all code uses the same struct layout
inline llvm::StructType* create_execution_context_type(
    llvm::IRBuilder<>& builder,
    llvm::LLVMContext& context)
{
    using namespace llvm;

    ArrayType* stack_array_type = ArrayType::get(builder.getInt64Ty(), DATA_STACK_SIZE);
    ArrayType* data_space_array_type = ArrayType::get(builder.getInt8Ty(), DATA_SPACE_SIZE);
    ArrayType* tib_array_type = ArrayType::get(builder.getInt8Ty(), TIB_SIZE);

    return StructType::create(context, {
        stack_array_type,       // CTX_DATA_STACK
        stack_array_type,       // CTX_RETURN_STACK
        data_space_array_type,  // CTX_DATA_SPACE
        tib_array_type,         // CTX_TIB
        builder.getInt64Ty(),   // CTX_DSP
        builder.getInt64Ty(),   // CTX_RSP
        builder.getInt64Ty(),   // CTX_HERE
        builder.getInt64Ty(),   // CTX_TO_IN
        builder.getInt64Ty()    // CTX_NUM_TIB
    }, "ExecutionContext");
}

} // namespace anvil

#endif // ANVIL_EXECUTION_CONTEXT_LAYOUT_H
