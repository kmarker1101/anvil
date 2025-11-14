// interpreter.rs - Bytecode interpreter for Forth

use crate::bytecode::{Bytecode, Instruction};
use crate::primitives::VM;

/// Bytecode interpreter that executes Forth instructions
pub struct Interpreter {
    pub vm: VM,
    /// Call stack for tracking return addresses
    call_stack: Vec<usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            vm: VM::new(),
            call_stack: Vec::new(),
        }
    }

    /// Execute bytecode starting at the given address
    pub fn execute(&mut self, bytecode: &Bytecode, start_addr: usize) -> Result<(), String> {
        let mut ip = start_addr; // Instruction pointer

        loop {
            if ip >= bytecode.len() {
                return Err(format!("IP out of bounds: {}", ip));
            }

            match &bytecode[ip] {
                Instruction::Primitive(prim) => {
                    self.vm
                        .execute_primitive(*prim)
                        .map_err(|e| format!("Primitive error: {:?}", e))?;
                    ip += 1;
                }

                Instruction::PushLiteral(value) => {
                    self.vm.data_stack.push(*value);
                    ip += 1;
                }

                Instruction::Jump(addr) => {
                    ip = *addr;
                }

                Instruction::JumpIfZero(addr) => {
                    let val = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("JumpIfZero: {:?}", e))?;
                    if val == 0 {
                        ip = *addr;
                    } else {
                        ip += 1;
                    }
                }

                Instruction::JumpIfNotZero(addr) => {
                    let val = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("JumpIfNotZero: {:?}", e))?;
                    if val != 0 {
                        ip = *addr;
                    } else {
                        ip += 1;
                    }
                }

                Instruction::Call(addr) => {
                    // Push return address onto call stack
                    self.call_stack.push(ip + 1);
                    ip = *addr;
                }

                Instruction::Return => {
                    // Pop return address from call stack
                    if let Some(return_addr) = self.call_stack.pop() {
                        ip = return_addr;
                    } else {
                        // Top-level return - exit execution
                        return Ok(());
                    }
                }

                Instruction::PushVariable(offset) => {
                    self.vm.data_stack.push(*offset as i64);
                    ip += 1;
                }

                Instruction::PushConstant(value) => {
                    self.vm.data_stack.push(*value);
                    ip += 1;
                }

                Instruction::DoSetup => {
                    // Pop start and limit from data stack
                    // Stack has: ( limit start -- ) so start is on top
                    let start = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("DoSetup start: {:?}", e))?;
                    let limit = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("DoSetup limit: {:?}", e))?;

                    // Push to loop stack: [index, limit]
                    self.vm.loop_stack.push(start);
                    self.vm.loop_stack.push(limit);
                    ip += 1;
                }

                Instruction::QuestionDoSetup(end_addr) => {
                    // Pop start and limit from data stack
                    // Stack has: ( limit start -- ) so start is on top
                    let start = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("QuestionDoSetup start: {:?}", e))?;
                    let limit = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("QuestionDoSetup limit: {:?}", e))?;

                    // If start == limit, skip the loop entirely
                    if start == limit {
                        ip = *end_addr;
                    } else {
                        // Push to loop stack: [index, limit]
                        self.vm.loop_stack.push(start);
                        self.vm.loop_stack.push(limit);
                        ip += 1;
                    }
                }

                Instruction::LoopCheck(end_addr) => {
                    // Get current index and limit from loop stack
                    if self.vm.loop_stack.depth() < 2 {
                        return Err("LoopCheck: loop stack underflow".to_string());
                    }

                    let limit = self.vm.loop_stack.peek().map_err(|e| format!("LoopCheck limit: {:?}", e))?;

                    // Get index (second from top)
                    let depth = self.vm.loop_stack.depth();
                    let idx = self.vm.loop_stack.get(depth - 2)
                        .ok_or("LoopCheck: can't get index")?;

                    // Increment index
                    let new_idx = idx + 1;

                    // Check if loop should continue
                    if new_idx >= limit {
                        // Loop done - clean up and jump to end
                        self.vm.loop_stack.pop().map_err(|e| format!("LoopCheck pop limit: {:?}", e))?;
                        self.vm.loop_stack.pop().map_err(|e| format!("LoopCheck pop index: {:?}", e))?;
                        ip = *end_addr;
                    } else {
                        // Update index and continue loop
                        self.vm.loop_stack.set(depth - 2, new_idx)
                            .map_err(|e| format!("LoopCheck set index: {:?}", e))?;
                        ip += 1;
                    }
                }

                Instruction::PlusLoopCheck(end_addr) => {
                    // Pop increment from data stack
                    let increment = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("PlusLoopCheck increment: {:?}", e))?;

                    // Get current index and limit from loop stack
                    if self.vm.loop_stack.depth() < 2 {
                        return Err("PlusLoopCheck: loop stack underflow".to_string());
                    }

                    let limit = self.vm.loop_stack.peek().map_err(|e| format!("PlusLoopCheck limit: {:?}", e))?;

                    let depth = self.vm.loop_stack.depth();
                    let idx = self.vm.loop_stack.get(depth - 2)
                        .ok_or("PlusLoopCheck: can't get index")?;

                    // Increment index
                    let new_idx = idx + increment;

                    // Check termination condition (handles both positive and negative increments)
                    let should_exit = if increment >= 0 {
                        new_idx >= limit
                    } else {
                        new_idx < limit
                    };

                    if should_exit {
                        // Loop done - clean up and jump to end
                        self.vm.loop_stack.pop().map_err(|e| format!("PlusLoopCheck pop limit: {:?}", e))?;
                        self.vm.loop_stack.pop().map_err(|e| format!("PlusLoopCheck pop index: {:?}", e))?;
                        ip = *end_addr;
                    } else {
                        // Update index and continue loop
                        self.vm.loop_stack.set(depth - 2, new_idx)
                            .map_err(|e| format!("PlusLoopCheck set index: {:?}", e))?;
                        ip += 1;
                    }
                }

                Instruction::LoopEnd => {
                    // Clean up loop stack (should already be done by LoopCheck, but just in case)
                    if self.vm.loop_stack.depth() >= 2 {
                        self.vm.loop_stack.pop().ok();
                        self.vm.loop_stack.pop().ok();
                    }
                    ip += 1;
                }

                Instruction::ExecuteXT => {
                    // Pop execution token (bytecode address) from data stack
                    let xt = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("ExecuteXT: {:?}", e))?;

                    // Push return address and jump to XT
                    self.call_stack.push(ip + 1);
                    ip = xt as usize;
                }

                Instruction::FindWord(dict_snapshot) => {
                    // ( c-addr -- c-addr 0 | xt 1 | xt -1 )
                    let c_addr = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("FindWord: {:?}", e))? as usize;

                    // Read counted string from memory
                    if c_addr >= self.vm.memory.len() {
                        return Err(format!("FindWord: c-addr out of bounds: {}", c_addr));
                    }

                    let count = self.vm.memory[c_addr] as usize;
                    if c_addr + 1 + count > self.vm.memory.len() {
                        return Err("FindWord: string extends beyond memory".to_string());
                    }

                    let word_bytes = &self.vm.memory[c_addr + 1..c_addr + 1 + count];
                    let word_str = String::from_utf8_lossy(word_bytes).to_uppercase();

                    // Look up in dictionary snapshot
                    if let Some((xt, is_immediate)) = dict_snapshot.get(&word_str) {
                        // Found user-defined word: push xt and immediacy flag
                        self.vm.data_stack.push(*xt as i64);
                        self.vm.data_stack.push(if *is_immediate { 1 } else { -1 });
                    } else if crate::primitives::Primitive::from_name(&word_str).is_some() {
                        // Found primitive: return dummy xt and non-immediate flag
                        self.vm.data_stack.push(1);
                        self.vm.data_stack.push(-1);
                    } else {
                        // Not found: push c-addr and 0
                        self.vm.data_stack.push(c_addr as i64);
                        self.vm.data_stack.push(0);
                    }

                    ip += 1;
                }

                Instruction::CompileCall(addr) => {
                    // This instruction can only be executed at compile-time
                    // It signals that compilation should happen
                    // Store the address in pending_compile_call for the compiler to handle
                    self.vm.pending_compile_call = Some(*addr);
                    ip += 1;
                }

                Instruction::CompilePrimitive(prim) => {
                    // This instruction can only be executed at compile-time
                    // Store the primitive in pending_compile_primitive for the compiler to handle
                    self.vm.pending_compile_primitive = Some(*prim);
                    ip += 1;
                }

                Instruction::DefineConstant => {
                    // DefineConstant: ( n "name" -- )
                    // Pop value from stack
                    let value = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("DefineConstant: {:?}", e))?;

                    // Parse the next word from input using WORD primitive
                    // Push space delimiter (ASCII 32)
                    self.vm.data_stack.push(32);
                    self.vm.execute_primitive(crate::primitives::Primitive::Word)
                        .map_err(|e| format!("DefineConstant WORD: {:?}", e))?;

                    // WORD returns c-addr of counted string
                    let c_addr = self
                        .vm
                        .data_stack
                        .pop()
                        .map_err(|e| format!("DefineConstant c-addr: {:?}", e))? as usize;

                    // Read counted string from memory
                    if c_addr >= self.vm.memory.len() {
                        return Err(format!("DefineConstant: c-addr out of bounds: {}", c_addr));
                    }

                    let count = self.vm.memory[c_addr] as usize;
                    if c_addr + 1 + count > self.vm.memory.len() {
                        return Err("DefineConstant: string extends beyond memory".to_string());
                    }

                    let word_bytes = &self.vm.memory[c_addr + 1..c_addr + 1 + count];
                    let name = String::from_utf8_lossy(word_bytes).to_uppercase();

                    // Store the constant definition request for the compiler to handle
                    self.vm.pending_constant_def = Some((name.to_string(), value));

                    ip += 1;
                }
            }
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
