#ifndef ANVIL_COMPILER_H
#define ANVIL_COMPILER_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>

#include <memory>
#include <string>
#include <unordered_map>

#include "ast.h"
#include "dictionary.h"
#include "primitives.h"
#include "primitives_registry.h"
#include "stack.h"

namespace anvil {

// Compiler that generates LLVM IR from AST
// Supports all three execution modes: Interpreter, JIT, AOT
class Compiler {
private:
    llvm::LLVMContext& context_;
    llvm::Module& module_;
    llvm::IRBuilder<> builder_;

    // Current execution context type (for function signatures)
    llvm::StructType* ctx_type_;

    // Current function being compiled
    llvm::Function* current_function_;

    // Pointers to current stack locations (when inside a function)
    llvm::Value* data_stack_ptr_;
    llvm::Value* return_stack_ptr_;
    llvm::Value* dsp_ptr_;
    llvm::Value* rsp_ptr_;

    // Stack of loop exit blocks for LEAVE support
    std::vector<llvm::BasicBlock*> loop_exit_stack_;

    // Initialize ExecutionContext struct type
    void init_context_type() {
        llvm::ArrayType* stack_array_type =
            llvm::ArrayType::get(builder_.getInt64Ty(), DATA_STACK_SIZE);

        ctx_type_ = llvm::StructType::create(context_, {
            stack_array_type,      // data_stack
            stack_array_type,      // return_stack
            builder_.getInt64Ty(), // dsp
            builder_.getInt64Ty()  // rsp
        }, "ExecutionContext");
    }

    // Create a function with signature: void func(ExecutionContext*)
    llvm::Function* create_function(const std::string& name) {
        llvm::Type* ctx_ptr_type = llvm::PointerType::get(context_, 0);
        llvm::FunctionType* func_type = llvm::FunctionType::get(
            builder_.getVoidTy(),
            {ctx_ptr_type},
            false
        );

        return llvm::Function::Create(
            func_type,
            llvm::Function::ExternalLinkage,
            name,
            module_
        );
    }

    // Setup stack pointers from ExecutionContext parameter
    void setup_stack_pointers(llvm::Function* func) {
        llvm::Value* ctx_ptr = func->getArg(0);

        // Get pointers to stack arrays and counters
        data_stack_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, 0, "data_stack_ptr");
        return_stack_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, 1, "return_stack_ptr");
        dsp_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, 2, "dsp_ptr");
        rsp_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, 3, "rsp_ptr");
    }

    // Compile an AST node to LLVM IR
    void compile_node(ASTNode* node) {
        if (!node) return;

        switch (node->type) {
            case ASTNodeType::LITERAL:
                compile_literal(static_cast<LiteralNode*>(node));
                break;

            case ASTNodeType::WORD_CALL:
                compile_word_call(static_cast<WordCallNode*>(node));
                break;

            case ASTNodeType::SEQUENCE:
                compile_sequence(static_cast<SequenceNode*>(node));
                break;

            case ASTNodeType::DEFINITION:
                compile_definition(static_cast<DefinitionNode*>(node));
                break;

            case ASTNodeType::IF_THEN:
                compile_if_then(static_cast<IfThenNode*>(node));
                break;

            case ASTNodeType::IF_ELSE_THEN:
                compile_if_else_then(static_cast<IfElseThenNode*>(node));
                break;

            case ASTNodeType::BEGIN_UNTIL:
                compile_begin_until(static_cast<BeginUntilNode*>(node));
                break;

            case ASTNodeType::BEGIN_WHILE_REPEAT:
                compile_begin_while_repeat(static_cast<BeginWhileRepeatNode*>(node));
                break;

            case ASTNodeType::DO_LOOP:
            case ASTNodeType::DO_PLUS_LOOP:
                compile_do_loop(static_cast<DoLoopNode*>(node));
                break;

            case ASTNodeType::STRING_LITERAL:
                compile_string_literal(static_cast<StringLiteralNode*>(node));
                break;
        }
    }

    // Compile a literal (pushes number onto stack)
    void compile_literal(LiteralNode* node) {
        emit_lit(builder_, data_stack_ptr_, dsp_ptr_, node->value);
    }

    // Compile a word call
    void compile_word_call(WordCallNode* node) {
        // Check for LEAVE (special loop control word)
        std::string upper_name = node->word_name;
        std::transform(upper_name.begin(), upper_name.end(), upper_name.begin(),
                      [](unsigned char c) { return std::toupper(c); });

        if (upper_name == "LEAVE") {
            // LEAVE exits the current loop
            if (!loop_exit_stack_.empty()) {
                builder_.CreateBr(loop_exit_stack_.back());
                // Create a new block for unreachable code after LEAVE
                llvm::BasicBlock* after_leave = llvm::BasicBlock::Create(
                    context_, "after_leave", current_function_
                );
                builder_.SetInsertPoint(after_leave);
            }
            // If not in a loop, ignore LEAVE (should be an error in real implementation)
            return;
        }

        // Check if it's a primitive
        const PrimitiveEmitFn* prim_fn = global_primitives.get_primitive(node->word_name);
        if (prim_fn) {
            // Emit inline IR for primitive
            (*prim_fn)(builder_, data_stack_ptr_, return_stack_ptr_, dsp_ptr_, rsp_ptr_);
            return;
        }

        // Otherwise look up in dictionary (user-defined word)
        const DictionaryEntry* entry = global_dictionary.find_word(node->word_name);

        if (entry) {
            // If the word has an LLVM function, emit a direct call
            if (entry->llvm_func) {
                llvm::Value* ctx_ptr = current_function_->getArg(0);
                builder_.CreateCall(entry->llvm_func, {ctx_ptr});
            } else {
                // Fallback: use EXECUTE for words without LLVM functions
                // (This shouldn't happen in normal compilation but keeps compatibility)
                llvm::Value* xt_int = builder_.getInt64(
                    reinterpret_cast<int64_t>(entry->xt)
                );

                // Push xt onto stack
                adjust_dsp(builder_, dsp_ptr_, 1);
                store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, xt_int);

                // Execute it
                llvm::Value* ctx_ptr = current_function_->getArg(0);
                emit_execute(builder_, data_stack_ptr_, dsp_ptr_, ctx_ptr);
            }
        } else {
            // Word not found - for now, we'll just ignore it
            // In a real implementation, this should be a compile-time error
            // or we should generate code to check at runtime
        }
    }

    // Compile a sequence of nodes
    void compile_sequence(SequenceNode* node) {
        for (auto& child : node->children) {
            compile_node(child.get());
        }
    }

    // Compile a word definition: : NAME ... ;
    void compile_definition(DefinitionNode* node) {
        // Create a new function for this word
        llvm::Function* word_func = create_function(node->name);

        // Create entry basic block
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", word_func);

        // Save previous insertion point and function context
        llvm::BasicBlock* prev_insert_block = builder_.GetInsertBlock();
        llvm::Function* prev_func = current_function_;
        llvm::Value* prev_data_stack = data_stack_ptr_;
        llvm::Value* prev_return_stack = return_stack_ptr_;
        llvm::Value* prev_dsp = dsp_ptr_;
        llvm::Value* prev_rsp = rsp_ptr_;

        // Set up new function context
        builder_.SetInsertPoint(entry);
        current_function_ = word_func;

        // Setup stack pointers
        setup_stack_pointers(word_func);

        // Compile the body
        compile_node(node->body.get());

        // Add return
        builder_.CreateRetVoid();

        // Restore previous context
        if (prev_insert_block) {
            builder_.SetInsertPoint(prev_insert_block);
        }
        current_function_ = prev_func;
        data_stack_ptr_ = prev_data_stack;
        return_stack_ptr_ = prev_return_stack;
        dsp_ptr_ = prev_dsp;
        rsp_ptr_ = prev_rsp;

        // Add word to dictionary with LLVM function
        ExecutionToken xt = reinterpret_cast<ExecutionToken>(
            reinterpret_cast<void*>(word_func)
        );
        global_dictionary.add_word(node->name, xt, WORD_NORMAL, word_func);
    }

    // Compile IF...THEN
    void compile_if_then(IfThenNode* node) {
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(
            context_, "then", current_function_
        );
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(
            context_, "merge", current_function_
        );

        // Emit 0BRANCH: if zero (false), skip to merge; if non-zero (true), execute then
        emit_0branch(builder_, data_stack_ptr_, dsp_ptr_, merge_block, then_block);

        // Compile then branch
        builder_.SetInsertPoint(then_block);
        compile_node(node->then_branch.get());
        builder_.CreateBr(merge_block);

        // Continue at merge
        builder_.SetInsertPoint(merge_block);
    }

    // Compile IF...ELSE...THEN
    void compile_if_else_then(IfElseThenNode* node) {
        llvm::BasicBlock* then_block = llvm::BasicBlock::Create(
            context_, "then", current_function_
        );
        llvm::BasicBlock* else_block = llvm::BasicBlock::Create(
            context_, "else", current_function_
        );
        llvm::BasicBlock* merge_block = llvm::BasicBlock::Create(
            context_, "merge", current_function_
        );

        // Emit 0BRANCH: if zero (false), go to else_block; if non-zero (true), go to then_block
        emit_0branch(builder_, data_stack_ptr_, dsp_ptr_, else_block, then_block);

        // Compile then branch
        builder_.SetInsertPoint(then_block);
        compile_node(node->then_branch.get());
        builder_.CreateBr(merge_block);

        // Compile else branch
        builder_.SetInsertPoint(else_block);
        compile_node(node->else_branch.get());
        builder_.CreateBr(merge_block);

        // Continue at merge
        builder_.SetInsertPoint(merge_block);
    }

    // Compile BEGIN...UNTIL
    void compile_begin_until(BeginUntilNode* node) {
        llvm::BasicBlock* loop_block = llvm::BasicBlock::Create(
            context_, "loop", current_function_
        );
        llvm::BasicBlock* exit_block = llvm::BasicBlock::Create(
            context_, "exit", current_function_
        );

        // Jump to loop
        builder_.CreateBr(loop_block);

        // Compile loop body
        builder_.SetInsertPoint(loop_block);
        compile_node(node->body.get());

        // Test condition: if zero (false), loop again; if non-zero (true), exit
        emit_0branch(builder_, data_stack_ptr_, dsp_ptr_, loop_block, exit_block);

        // Continue after loop
        builder_.SetInsertPoint(exit_block);
    }

    // Compile BEGIN...WHILE...REPEAT
    void compile_begin_while_repeat(BeginWhileRepeatNode* node) {
        llvm::BasicBlock* loop_block = llvm::BasicBlock::Create(
            context_, "loop", current_function_
        );
        llvm::BasicBlock* body_block = llvm::BasicBlock::Create(
            context_, "body", current_function_
        );
        llvm::BasicBlock* exit_block = llvm::BasicBlock::Create(
            context_, "exit", current_function_
        );

        // Jump to loop
        builder_.CreateBr(loop_block);

        // Compile condition body
        builder_.SetInsertPoint(loop_block);
        compile_node(node->condition_body.get());

        // WHILE test: if zero, exit; if non-zero, continue to body
        emit_0branch(builder_, data_stack_ptr_, dsp_ptr_, exit_block, body_block);

        // Compile repeat body
        builder_.SetInsertPoint(body_block);
        compile_node(node->loop_body.get());
        builder_.CreateBr(loop_block);

        // Continue after loop
        builder_.SetInsertPoint(exit_block);
    }

    // Compile DO...LOOP with full loop index support
    void compile_do_loop(DoLoopNode* node) {
        // Stack at entry: ( limit start -- )
        // We need to move limit and start to return stack

        // Create basic blocks for loop
        llvm::BasicBlock* loop_body = llvm::BasicBlock::Create(
            context_, "loop_body", current_function_
        );
        llvm::BasicBlock* loop_test = llvm::BasicBlock::Create(
            context_, "loop_test", current_function_
        );
        llvm::BasicBlock* loop_exit = llvm::BasicBlock::Create(
            context_, "loop_exit", current_function_
        );

        // Push loop exit block for LEAVE support
        loop_exit_stack_.push_back(loop_exit);

        // Pop start and limit from data stack and push to return stack
        // Stack: ( limit start -- )
        // Return stack after: ( -- start limit )
        emit_two_to_r(builder_, data_stack_ptr_, return_stack_ptr_, dsp_ptr_, rsp_ptr_);

        // Jump to loop test
        builder_.CreateBr(loop_test);

        // Compile loop body
        builder_.SetInsertPoint(loop_body);
        compile_node(node->body.get());

        // After loop body, increment or add to index
        if (node->is_plus_loop) {
            // +LOOP: pop increment from data stack, add to index
            // Stack: ( n -- )
            llvm::Value* increment = load_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0);
            adjust_dsp(builder_, dsp_ptr_, -1);

            // Load current index from return stack [rsp-1] (start/index is at top)
            llvm::Value* rsp = builder_.CreateLoad(builder_.getInt64Ty(), rsp_ptr_, "rsp");
            llvm::Value* idx_pos = builder_.CreateSub(rsp, builder_.getInt64(1), "idx_pos");
            llvm::Value* idx_ptr = builder_.CreateGEP(builder_.getInt64Ty(),
                                                       return_stack_ptr_, idx_pos, "idx_ptr");
            llvm::Value* current_idx = builder_.CreateLoad(builder_.getInt64Ty(), idx_ptr, "current_idx");

            // Add increment to index
            llvm::Value* new_idx = builder_.CreateAdd(current_idx, increment, "new_idx");

            // Store new index back to return stack
            builder_.CreateStore(new_idx, idx_ptr);
        } else {
            // LOOP: increment index by 1
            // Load current index from return stack [rsp-1] (start/index is at top)
            llvm::Value* rsp = builder_.CreateLoad(builder_.getInt64Ty(), rsp_ptr_, "rsp");
            llvm::Value* idx_pos = builder_.CreateSub(rsp, builder_.getInt64(1), "idx_pos");
            llvm::Value* idx_ptr = builder_.CreateGEP(builder_.getInt64Ty(),
                                                       return_stack_ptr_, idx_pos, "idx_ptr");
            llvm::Value* current_idx = builder_.CreateLoad(builder_.getInt64Ty(), idx_ptr, "current_idx");

            // Increment by 1
            llvm::Value* new_idx = builder_.CreateAdd(current_idx, builder_.getInt64(1), "new_idx");

            // Store new index back to return stack
            builder_.CreateStore(new_idx, idx_ptr);
        }

        // Jump to loop test
        builder_.CreateBr(loop_test);

        // Loop test: check if index < limit
        builder_.SetInsertPoint(loop_test);

        // Load index and limit from return stack
        // Return stack layout: [limit, start/index] with index on top
        llvm::Value* rsp = builder_.CreateLoad(builder_.getInt64Ty(), rsp_ptr_, "rsp");

        // Load limit at [rsp-2]
        llvm::Value* limit_pos = builder_.CreateSub(rsp, builder_.getInt64(2), "limit_pos");
        llvm::Value* limit_ptr = builder_.CreateGEP(builder_.getInt64Ty(),
                                                     return_stack_ptr_, limit_pos, "limit_ptr");
        llvm::Value* loop_limit = builder_.CreateLoad(builder_.getInt64Ty(), limit_ptr, "loop_limit");

        // Load index at [rsp-1]
        llvm::Value* idx_pos = builder_.CreateSub(rsp, builder_.getInt64(1), "idx_pos");
        llvm::Value* idx_ptr = builder_.CreateGEP(builder_.getInt64Ty(),
                                                   return_stack_ptr_, idx_pos, "idx_ptr");
        llvm::Value* loop_idx = builder_.CreateLoad(builder_.getInt64Ty(), idx_ptr, "loop_idx");

        // Compare: index < limit
        llvm::Value* continue_loop = builder_.CreateICmpSLT(loop_idx, loop_limit, "continue_loop");

        // Branch: if true, continue loop; if false, exit
        builder_.CreateCondBr(continue_loop, loop_body, loop_exit);

        // Loop exit: clean up return stack
        builder_.SetInsertPoint(loop_exit);

        // Pop index and limit from return stack
        emit_two_from_r(builder_, data_stack_ptr_, return_stack_ptr_, dsp_ptr_, rsp_ptr_);

        // Drop the two values from data stack (we don't need them)
        adjust_dsp(builder_, dsp_ptr_, -2);

        // Pop loop exit block from stack
        loop_exit_stack_.pop_back();
    }

    // Compile string literal for S" (pushes addr and len onto stack)
    void compile_string_literal(StringLiteralNode* node) {
        // Create a global string constant
        llvm::Constant* str_constant = llvm::ConstantDataArray::getString(
            context_, node->value, false  // false = don't null-terminate
        );

        // Create a global variable to hold the string
        llvm::GlobalVariable* str_global = new llvm::GlobalVariable(
            module_,
            str_constant->getType(),
            true,  // isConstant
            llvm::GlobalValue::PrivateLinkage,
            str_constant,
            ".str"
        );

        // Get pointer to the string data
        llvm::Value* str_ptr = builder_.CreateBitCast(
            str_global,
            llvm::PointerType::get(context_, 0),
            "str_ptr"
        );

        // Convert pointer to int64 for Forth stack
        llvm::Value* str_addr = builder_.CreatePtrToInt(
            str_ptr,
            builder_.getInt64Ty(),
            "str_addr"
        );

        // Get string length
        llvm::Value* str_len = builder_.getInt64(node->value.length());

        // Push address and length onto data stack
        // Stack effect: ( -- addr len )
        // Push len first, then addr (so addr ends up on TOS for array indexing)
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, str_len);
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, str_addr);
    }

public:
    Compiler(llvm::LLVMContext& context, llvm::Module& module)
        : context_(context),
          module_(module),
          builder_(context),
          ctx_type_(nullptr),
          current_function_(nullptr),
          data_stack_ptr_(nullptr),
          return_stack_ptr_(nullptr),
          dsp_ptr_(nullptr),
          rsp_ptr_(nullptr) {
        init_context_type();
    }

    // Compile an AST tree
    // Returns the top-level function if it's a definition, nullptr otherwise
    llvm::Function* compile(ASTNode* ast) {
        if (!ast) return nullptr;

        // If it's a definition, compile it and return the function
        if (ast->type == ASTNodeType::DEFINITION) {
            compile_definition(static_cast<DefinitionNode*>(ast));
            return module_.getFunction(static_cast<DefinitionNode*>(ast)->name);
        }

        // Otherwise, create an anonymous function to hold the code
        llvm::Function* main_func = create_function("__main");
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", main_func);
        builder_.SetInsertPoint(entry);

        current_function_ = main_func;
        setup_stack_pointers(main_func);

        // Compile the AST
        compile_node(ast);

        // Add return
        builder_.CreateRetVoid();

        return main_func;
    }
};

} // namespace anvil

#endif // ANVIL_COMPILER_H
