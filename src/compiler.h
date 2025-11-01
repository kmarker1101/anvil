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
#include <iostream>

#include "ast.h"
#include "dictionary.h"
#include "primitives.h"
#include "primitives_registry.h"
#include "stack.h"
#include "execution_context_layout.h"

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
    llvm::Value* data_space_ptr_;
    llvm::Value* dsp_ptr_;
    llvm::Value* rsp_ptr_;
    llvm::Value* here_ptr_;
    llvm::Value* tib_ptr_;
    llvm::Value* to_in_ptr_;
    llvm::Value* num_tib_ptr_;

    // Stack of loop exit blocks for LEAVE support
    std::vector<llvm::BasicBlock*> loop_exit_stack_;

    // Compile-time data space allocation tracker
    // Tracks the offset into data_space for VARIABLE/CONSTANT allocations
    size_t compile_time_here_;

    // Initialize ExecutionContext struct type
    void init_context_type() {
        ctx_type_ = create_execution_context_type(builder_, context_);
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

        // Get pointers to stack arrays and counters using centralized field indices
        data_stack_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_DATA_STACK, "data_stack_ptr");
        return_stack_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_RETURN_STACK, "return_stack_ptr");
        data_space_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_DATA_SPACE, "data_space_ptr");
        tib_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_TIB, "tib_ptr");
        dsp_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_DSP, "dsp_ptr");
        rsp_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_RSP, "rsp_ptr");
        here_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_HERE, "here_ptr");
        to_in_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_TO_IN, "to_in_ptr");
        num_tib_ptr_ = builder_.CreateStructGEP(ctx_type_, ctx_ptr, CTX_NUM_TIB, "num_tib_ptr");
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

            case ASTNodeType::DOT_QUOTE:
                compile_dot_quote(static_cast<DotQuoteNode*>(node));
                break;

            case ASTNodeType::VARIABLE:
                compile_variable(static_cast<VariableNode*>(node));
                break;

            case ASTNodeType::CONSTANT:
                compile_constant(static_cast<ConstantNode*>(node));
                break;

            case ASTNodeType::TICK:
                compile_tick(static_cast<TickNode*>(node));
                break;
        }
    }

    // Compile a literal (pushes number onto stack)
    void compile_literal(LiteralNode* node) {
        emit_lit(builder_, data_stack_ptr_, dsp_ptr_, node->value);
    }

    // Compile a word call
    void compile_word_call(WordCallNode* node) {
        // Check for special compile-time words
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

        if (upper_name == "EXIT") {
            // EXIT returns from the current word immediately
            if (!current_function_) {
                throw std::runtime_error("EXIT used outside of definition");
            }
            builder_.CreateRetVoid();
            // Create a new block for any unreachable code after EXIT
            llvm::BasicBlock* after_exit = llvm::BasicBlock::Create(
                context_, "after_exit", current_function_
            );
            builder_.SetInsertPoint(after_exit);
            return;
        }

        if (upper_name == "RECURSE") {
            // RECURSE calls the word currently being defined
            if (!current_function_) {
                throw std::runtime_error("RECURSE used outside of definition");
            }
            llvm::Value* ctx_ptr = current_function_->getArg(0);
            builder_.CreateCall(current_function_, {ctx_ptr});
            return;
        }

        // Check if it's a primitive
        const PrimitiveEmitFn* prim_fn = global_primitives.get_primitive(node->word_name);
        if (prim_fn) {
            // Emit inline IR for primitive
            (*prim_fn)(builder_, data_stack_ptr_, return_stack_ptr_, data_space_ptr_,
                      dsp_ptr_, rsp_ptr_, here_ptr_, tib_ptr_, to_in_ptr_, num_tib_ptr_);
            return;
        }

        // Otherwise look up in dictionary (user-defined word)
        const DictionaryEntry* entry = global_dictionary.find_word(node->word_name);

        if (entry) {
            // Look up the function in the current module by name
            // This ensures we call the right function even when modules are cloned
            llvm::Function* func = module_.getFunction(node->word_name);

            if (func) {
                // Direct call to the function in the current module
                llvm::Value* ctx_ptr = current_function_->getArg(0);
                builder_.CreateCall(func, {ctx_ptr});
            } else if (entry->llvm_func) {
                // Fallback: use the cached function pointer if not in current module
                // This handles cases where the function hasn't been cloned yet
                llvm::Value* ctx_ptr = current_function_->getArg(0);
                builder_.CreateCall(entry->llvm_func, {ctx_ptr});
            } else {
                // Final fallback: use EXECUTE for words without LLVM functions
                // (This shouldn't happen in normal compilation but keeps compatibility)
                llvm::Value* xt_int = builder_.getInt64(
                    reinterpret_cast<int64_t>(entry->xt)
                );

                // Push xt onto stack
                adjust_dsp(builder_, dsp_ptr_, 1);
                store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, xt_int);

                // Execute it
                emit_execute(builder_, data_stack_ptr_, dsp_ptr_);
            }
        } else {
            throw std::runtime_error(node->word_name + " ?");
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
        llvm::Value* prev_data_space = data_space_ptr_;
        llvm::Value* prev_dsp = dsp_ptr_;
        llvm::Value* prev_rsp = rsp_ptr_;
        llvm::Value* prev_here = here_ptr_;

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
        data_space_ptr_ = prev_data_space;
        dsp_ptr_ = prev_dsp;
        rsp_ptr_ = prev_rsp;
        here_ptr_ = prev_here;

        // Add word to dictionary with LLVM function
        ExecutionToken xt = reinterpret_cast<ExecutionToken>(
            reinterpret_cast<void*>(word_func)
        );
        global_dictionary.add_word(node->name, xt, WORD_NORMAL, word_func);
    }

    // Compile VARIABLE: allocates space at compile-time and creates a word that pushes the address
    void compile_variable(VariableNode* node) {
        // Allocate space at compile time by reserving a fixed offset in data_space
        size_t var_offset = compile_time_here_;
        compile_time_here_ += 8;  // Allocate one cell (8 bytes)

        // Create a function that computes and pushes: data_space_ptr + offset
        llvm::Function* var_func = create_function(node->name);
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", var_func);

        llvm::BasicBlock* prev_insert_block = builder_.GetInsertBlock();
        llvm::Function* prev_func = current_function_;
        llvm::Value* prev_data_stack = data_stack_ptr_;
        llvm::Value* prev_return_stack = return_stack_ptr_;
        llvm::Value* prev_data_space = data_space_ptr_;
        llvm::Value* prev_dsp = dsp_ptr_;
        llvm::Value* prev_rsp = rsp_ptr_;
        llvm::Value* prev_here = here_ptr_;

        builder_.SetInsertPoint(entry);
        current_function_ = var_func;
        setup_stack_pointers(var_func);

        // Calculate address: data_space + var_offset
        llvm::Type *i8_ptr_type = llvm::PointerType::get(context_, 0);
        llvm::Value *data_space_i8 = builder_.CreateBitCast(data_space_ptr_, i8_ptr_type);
        llvm::Value *var_addr_ptr = builder_.CreateGEP(
            builder_.getInt8Ty(), data_space_i8,
            builder_.getInt64(var_offset), "var_addr_ptr");

        // Convert to int64
        llvm::Value *var_addr = builder_.CreatePtrToInt(var_addr_ptr, builder_.getInt64Ty(), "var_addr");

        // Push address onto stack
        llvm::Value* dsp = builder_.CreateLoad(builder_.getInt64Ty(), dsp_ptr_, "dsp");
        llvm::Value* stack_top = builder_.CreateGEP(builder_.getInt64Ty(), data_stack_ptr_, dsp, "stack_top");
        builder_.CreateStore(var_addr, stack_top);
        llvm::Value* new_dsp = builder_.CreateAdd(dsp, builder_.getInt64(1), "new_dsp");
        builder_.CreateStore(new_dsp, dsp_ptr_);

        builder_.CreateRetVoid();

        if (prev_insert_block) {
            builder_.SetInsertPoint(prev_insert_block);
        }
        current_function_ = prev_func;
        data_stack_ptr_ = prev_data_stack;
        return_stack_ptr_ = prev_return_stack;
        data_space_ptr_ = prev_data_space;
        dsp_ptr_ = prev_dsp;
        rsp_ptr_ = prev_rsp;
        here_ptr_ = prev_here;

        ExecutionToken xt = reinterpret_cast<ExecutionToken>(
            reinterpret_cast<void*>(var_func)
        );
        global_dictionary.add_word(node->name, xt, WORD_NORMAL, var_func);
    }

    // Compile CONSTANT: creates a word that pushes a literal value
    void compile_constant(ConstantNode* node) {
        // Create a function that just pushes the constant value
        // The value is embedded in the LLVM IR as a literal
        llvm::Function* const_func = create_function(node->name);
        llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", const_func);

        llvm::BasicBlock* prev_insert_block = builder_.GetInsertBlock();
        llvm::Function* prev_func = current_function_;
        llvm::Value* prev_data_stack = data_stack_ptr_;
        llvm::Value* prev_return_stack = return_stack_ptr_;
        llvm::Value* prev_data_space = data_space_ptr_;
        llvm::Value* prev_dsp = dsp_ptr_;
        llvm::Value* prev_rsp = rsp_ptr_;
        llvm::Value* prev_here = here_ptr_;

        builder_.SetInsertPoint(entry);
        current_function_ = const_func;
        setup_stack_pointers(const_func);

        // Push the constant value onto stack using emit_lit
        emit_lit(builder_, data_stack_ptr_, dsp_ptr_, node->value);

        builder_.CreateRetVoid();

        if (prev_insert_block) {
            builder_.SetInsertPoint(prev_insert_block);
        }
        current_function_ = prev_func;
        data_stack_ptr_ = prev_data_stack;
        return_stack_ptr_ = prev_return_stack;
        data_space_ptr_ = prev_data_space;
        dsp_ptr_ = prev_dsp;
        rsp_ptr_ = prev_rsp;
        here_ptr_ = prev_here;

        ExecutionToken xt = reinterpret_cast<ExecutionToken>(
            reinterpret_cast<void*>(const_func)
        );
        global_dictionary.add_word(node->name, xt, WORD_NORMAL, const_func);
    }

    // Compile ' name (tick)
    // Looks up the word and pushes its execution token onto the stack
    // Now uses runtime FIND to get the correct JIT-compiled address
    void compile_tick(TickNode* node) {
        // Create a string literal in the data segment for the word name
        llvm::Constant* name_str = llvm::ConstantDataArray::getString(
            context_, node->name, false
        );

        llvm::GlobalVariable* name_global = new llvm::GlobalVariable(
            module_,
            name_str->getType(),
            true,  // is constant
            llvm::GlobalValue::PrivateLinkage,
            name_str,
            ".str.tick." + node->name
        );

        // Get pointer to the string
        llvm::Value* name_ptr = builder_.CreatePointerCast(
            name_global,
            llvm::PointerType::get(context_, 0),
            "name_ptr"
        );

        // Convert pointer to int64 for the stack
        llvm::Value* name_addr = builder_.CreatePtrToInt(
            name_ptr,
            builder_.getInt64Ty(),
            "name_addr"
        );

        // Push string address onto stack
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, name_addr);

        // Push string length onto stack
        llvm::Value* name_len = builder_.getInt64(node->name.length());
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, name_len);

        // Call FIND to look up the word at runtime
        emit_find(builder_, data_stack_ptr_, dsp_ptr_);

        // FIND returns ( xt flag )
        // We need to check the flag and throw an error if not found
        // For now, we'll just leave both on the stack
        // The flag will indicate success (-1) or failure (0)

        // Pop the flag
        llvm::Value* flag = load_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0);
        adjust_dsp(builder_, dsp_ptr_, -1);

        // Create blocks for found and not-found cases
        llvm::BasicBlock* found_block = llvm::BasicBlock::Create(
            context_, "tick_found", current_function_
        );
        llvm::BasicBlock* not_found_block = llvm::BasicBlock::Create(
            context_, "tick_not_found", current_function_
        );

        // Check if found (flag == -1)
        llvm::Value* is_found = builder_.CreateICmpEQ(flag, builder_.getInt64(-1), "is_found");
        builder_.CreateCondBr(is_found, found_block, not_found_block);

        // Not found block: print error and exit
        builder_.SetInsertPoint(not_found_block);

        // Declare printf for error message
        llvm::FunctionType* printf_type = llvm::FunctionType::get(
            builder_.getInt32Ty(),
            {llvm::PointerType::get(context_, 0)},
            true
        );
        llvm::FunctionCallee printf_func = module_.getOrInsertFunction("printf", printf_type);

        // Create error message
        std::string error_msg = "' " + node->name + " ? (word not found)\n";
        llvm::Constant* error_str = llvm::ConstantDataArray::getString(context_, error_msg, true);
        llvm::GlobalVariable* error_global = new llvm::GlobalVariable(
            module_,
            error_str->getType(),
            true,
            llvm::GlobalValue::PrivateLinkage,
            error_str,
            ".str.tick.error"
        );
        llvm::Value* error_ptr = builder_.CreatePointerCast(
            error_global,
            llvm::PointerType::get(context_, 0)
        );

        builder_.CreateCall(printf_func, {error_ptr});

        // Declare exit function
        llvm::FunctionType* exit_type = llvm::FunctionType::get(
            builder_.getVoidTy(),
            {builder_.getInt32Ty()},
            false
        );
        llvm::FunctionCallee exit_func = module_.getOrInsertFunction("exit", exit_type);
        builder_.CreateCall(exit_func, {builder_.getInt32(1)});
        builder_.CreateUnreachable();

        // Found block: XT is already on stack, continue
        builder_.SetInsertPoint(found_block);
        // XT is now on top of stack, ready to be used
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

    // Compile DO...LOOP or ?DO...LOOP with full loop index support
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

        // For ?DO, check if start < limit before entering loop
        if (node->is_question_do) {
            // Duplicate start and limit to check condition
            // Stack: ( limit start -- limit start )
            llvm::Value* start_val = load_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0);
            llvm::Value* limit_val = load_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 1);

            // Compare: start < limit
            llvm::Value* should_loop = builder_.CreateICmpSLT(start_val, limit_val, "should_loop");

            // Create blocks for conditional entry
            llvm::BasicBlock* setup_block = llvm::BasicBlock::Create(
                context_, "loop_setup", current_function_
            );
            llvm::BasicBlock* skip_block = llvm::BasicBlock::Create(
                context_, "loop_skip", current_function_
            );

            // If start >= limit, skip to skip_block to just pop values
            builder_.CreateCondBr(should_loop, setup_block, skip_block);

            // Skip block: pop values from stack without running loop
            builder_.SetInsertPoint(skip_block);
            adjust_dsp(builder_, dsp_ptr_, -2);  // Drop limit and start
            builder_.CreateBr(loop_exit);

            // Setup block: move to return stack
            builder_.SetInsertPoint(setup_block);
        }

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

    // Compile dot-quote for ." (prints string immediately)
    void compile_dot_quote(DotQuoteNode* node) {
        // Create a global string constant (same as S")
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
            ".str.dotquote"
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
        // Stack effect: ( -- addr len ) temporarily for TYPE
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, str_len);
        adjust_dsp(builder_, dsp_ptr_, 1);
        store_stack_at_depth(builder_, data_stack_ptr_, dsp_ptr_, 0, str_addr);

        // Call TYPE to print the string
        // TYPE consumes ( addr len -- ), leaving stack empty
        emit_type(builder_, data_stack_ptr_, dsp_ptr_);
    }

public:
    Compiler(llvm::LLVMContext& context, llvm::Module& module, size_t initial_here = 0)
        : context_(context),
          module_(module),
          builder_(context),
          ctx_type_(nullptr),
          current_function_(nullptr),
          data_stack_ptr_(nullptr),
          return_stack_ptr_(nullptr),
          data_space_ptr_(nullptr),
          dsp_ptr_(nullptr),
          rsp_ptr_(nullptr),
          here_ptr_(nullptr),
          compile_time_here_(initial_here) {
        init_context_type();
    }

    // Get current compile-time HERE value (for persistent REPL state)
    size_t get_compile_time_here() const {
        return compile_time_here_;
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

        // If it's a sequence, check if all children are definitions/variables/constants
        if (ast->type == ASTNodeType::SEQUENCE) {
            auto seq = static_cast<SequenceNode*>(ast);
            bool all_declarations = true;
            for (auto& child : seq->children) {
                if (child->type != ASTNodeType::DEFINITION &&
                    child->type != ASTNodeType::VARIABLE &&
                    child->type != ASTNodeType::CONSTANT) {
                    all_declarations = false;
                    break;
                }
            }

            // If all children are declarations (definitions/variables/constants), compile them directly
            if (all_declarations) {
                llvm::Function* last_func = nullptr;
                for (auto& child : seq->children) {
                    if (child->type == ASTNodeType::DEFINITION) {
                        auto def = static_cast<DefinitionNode*>(child.get());
                        compile_definition(def);
                        last_func = module_.getFunction(def->name);
                    } else if (child->type == ASTNodeType::VARIABLE) {
                        auto var = static_cast<VariableNode*>(child.get());
                        compile_variable(var);
                        last_func = module_.getFunction(var->name);
                    } else if (child->type == ASTNodeType::CONSTANT) {
                        auto con = static_cast<ConstantNode*>(child.get());
                        compile_constant(con);
                        last_func = module_.getFunction(con->name);
                    }
                }
                return last_func;
            }
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
