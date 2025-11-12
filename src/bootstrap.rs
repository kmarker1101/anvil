// bootstrap.rs - MINIMAL bootstrap compiler
// Only executes primitives and numbers. Everything else is in Forth!

use crate::interpreter::Interpreter;
use crate::primitives::Primitive;

pub struct BootstrapCompiler {
    pub interpreter: Interpreter,
}

impl BootstrapCompiler {
    pub fn new() -> Self {
        BootstrapCompiler {
            interpreter: Interpreter::new(),
        }
    }

    /// Process Forth source - tokenize and execute
    pub fn process_line(&mut self, line: &str) -> Result<(), String> {
        let tokens = self.tokenize(line);

        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];

            // Handle : (colon definition)
            if token == ":" {
                if i + 1 >= tokens.len() {
                    return Err("Expected word name after :".to_string());
                }
                let name = &tokens[i + 1];

                // Create dictionary entry
                let name_bytes = name.as_bytes();
                let temp_addr = 0x4000;
                self.interpreter.vm.memory[temp_addr..temp_addr + name_bytes.len()]
                    .copy_from_slice(name_bytes);
                self.interpreter.vm.data_stack.push(temp_addr as i64);
                self.interpreter.vm.data_stack.push(name_bytes.len() as i64);
                self.interpreter.vm.execute_primitive(Primitive::Create)
                    .map_err(|e| format!("CREATE error: {:?}", e))?;

                // Enter compile mode (STATE = 1)
                let state_bytes = 1i64.to_le_bytes();
                self.interpreter.vm.memory[0x120..0x128].copy_from_slice(&state_bytes);

                i += 2;
                continue;
            }

            // Handle ; (end definition)
            if token == ";" {
                // Compile RETURN
                self.interpreter.vm.execute_primitive(Primitive::CompileReturn)
                    .map_err(|e| format!("COMPILE-RETURN error: {:?}", e))?;

                // Exit compile mode (STATE = 0)
                let state_bytes = 0i64.to_le_bytes();
                self.interpreter.vm.memory[0x120..0x128].copy_from_slice(&state_bytes);

                i += 1;
                continue;
            }

            // Handle VARIABLE
            if token.to_uppercase() == "VARIABLE" {
                if i + 1 >= tokens.len() {
                    return Err("Expected name after VARIABLE".to_string());
                }
                let name = &tokens[i + 1];

                // Create dictionary entry
                let name_bytes = name.as_bytes();
                let temp_addr = 0x4000;
                self.interpreter.vm.memory[temp_addr..temp_addr + name_bytes.len()]
                    .copy_from_slice(name_bytes);
                self.interpreter.vm.data_stack.push(temp_addr as i64);
                self.interpreter.vm.data_stack.push(name_bytes.len() as i64);
                self.interpreter.vm.execute_primitive(Primitive::Create)
                    .map_err(|e| format!("CREATE error: {:?}", e))?;

                // Compile bytecode: LIT (addr), RETURN
                // DP now points to where bytecode should go
                // First compile the bytecode, THEN allocate variable storage after it

                // Get current DP (where bytecode goes)
                let mut dp_bytes = [0u8; 8];
                dp_bytes.copy_from_slice(&self.interpreter.vm.memory[0x118..0x120]);
                let bytecode_addr = i64::from_le_bytes(dp_bytes);

                // Compile: LIT, RETURN (this will advance DP)
                // The LIT needs to push the address where variable storage will be
                // Storage will be allocated after the code
                let var_addr = bytecode_addr + 1 + 8 + 1; // After: OPCODE(1) + VALUE(8) + RETURN(1)

                self.interpreter.vm.data_stack.push(var_addr);
                self.interpreter.vm.execute_primitive(Primitive::CompileLiteral)
                    .map_err(|e| format!("COMPILE-LIT error: {:?}", e))?;
                self.interpreter.vm.execute_primitive(Primitive::CompileReturn)
                    .map_err(|e| format!("COMPILE-RETURN error: {:?}", e))?;

                // Now allocate 8 bytes for variable storage after the code
                let mut dp_bytes = [0u8; 8];
                dp_bytes.copy_from_slice(&self.interpreter.vm.memory[0x118..0x120]);
                let storage_addr = i64::from_le_bytes(dp_bytes);
                let new_dp = storage_addr + 8;
                self.interpreter.vm.memory[0x118..0x120].copy_from_slice(&new_dp.to_le_bytes());
                // Initialize variable to 0
                self.interpreter.vm.memory[storage_addr as usize..storage_addr as usize + 8].fill(0);

                i += 2;
                continue;
            }

            // Handle CONSTANT
            if token.to_uppercase() == "CONSTANT" {
                if i + 1 >= tokens.len() {
                    return Err("Expected name after CONSTANT".to_string());
                }
                let name = &tokens[i + 1];
                let value = self.interpreter.vm.data_stack.pop()
                    .map_err(|e| format!("CONSTANT needs value on stack: {:?}", e))?;

                // Create dictionary entry
                let name_bytes = name.as_bytes();
                let temp_addr = 0x4000;
                self.interpreter.vm.memory[temp_addr..temp_addr + name_bytes.len()]
                    .copy_from_slice(name_bytes);
                self.interpreter.vm.data_stack.push(temp_addr as i64);
                self.interpreter.vm.data_stack.push(name_bytes.len() as i64);
                self.interpreter.vm.execute_primitive(Primitive::Create)
                    .map_err(|e| format!("CREATE error: {:?}", e))?;

                // Compile: LIT value, RETURN
                self.interpreter.vm.data_stack.push(value);
                self.interpreter.vm.execute_primitive(Primitive::CompileLiteral)
                    .map_err(|e| format!("COMPILE-LIT error: {:?}", e))?;
                self.interpreter.vm.execute_primitive(Primitive::CompileReturn)
                    .map_err(|e| format!("COMPILE-RETURN error: {:?}", e))?;

                i += 2;
                continue;
            }

            // Check if we're in compile mode
            let mut state_bytes = [0u8; 8];
            state_bytes.copy_from_slice(&self.interpreter.vm.memory[0x120..0x128]);
            let compiling = i64::from_le_bytes(state_bytes) != 0;

            // Try number
            if let Ok(num) = self.parse_number(token) {
                if compiling {
                    self.interpreter.vm.data_stack.push(num);
                    self.interpreter.vm.execute_primitive(Primitive::CompileLiteral)
                        .map_err(|e| format!("COMPILE-LIT error: {:?}", e))?;
                } else {
                    self.interpreter.vm.data_stack.push(num);
                }
                i += 1;
                continue;
            }

            // Look up word in memory-based dictionary (contains both primitives and user-defined)
            let name_bytes = token.as_bytes();
            let temp_addr = 0x4000;
            self.interpreter.vm.memory[temp_addr] = name_bytes.len() as u8;
            self.interpreter.vm.memory[temp_addr + 1..temp_addr + 1 + name_bytes.len()]
                .copy_from_slice(name_bytes);

            self.interpreter.vm.data_stack.push(temp_addr as i64);
            self.interpreter.vm.execute_primitive(Primitive::Find)
                .map_err(|e| format!("FIND error: {:?}", e))?;

            let flag = self.interpreter.vm.data_stack.pop()
                .map_err(|e| format!("FIND stack error: {:?}", e))?;

            if flag != 0 {
                let xt = self.interpreter.vm.data_stack.pop()
                    .map_err(|e| format!("FIND stack error: {:?}", e))? as usize;

                if compiling {
                    self.interpreter.vm.data_stack.push(xt as i64);
                    self.interpreter.vm.execute_primitive(Primitive::CompileCall)
                        .map_err(|e| format!("COMPILE-CALL error: {:?}", e))?;
                } else {
                    self.interpreter.execute_from_memory(xt)?;
                }
                i += 1;
                continue;
            } else {
                // FIND failed - pop the c-addr it left on stack
                self.interpreter.vm.data_stack.pop()
                    .map_err(|e| format!("FIND cleanup error: {:?}", e))?;
            }

            return Err(format!("Unknown word: {}", token));
        }

        Ok(())
    }

    fn tokenize(&self, line: &str) -> Vec<String> {
        let mut tokens = Vec::new();
        let mut chars = line.chars().peekable();
        let mut current = String::new();

        while let Some(ch) = chars.next() {
            match ch {
                // Line comment - skip to end of line
                '\\' if current.is_empty() => {
                    while let Some(c) = chars.next() {
                        if c == '\n' { break; }
                    }
                }

                // Paren comment
                '(' if current.is_empty() => {
                    while let Some(c) = chars.next() {
                        if c == ')' { break; }
                    }
                }

                // Whitespace
                ' ' | '\t' | '\n' | '\r' => {
                    if !current.is_empty() {
                        tokens.push(current.clone());
                        current.clear();
                    }
                }

                // Regular character
                _ => current.push(ch),
            }
        }

        if !current.is_empty() {
            tokens.push(current);
        }

        tokens
    }

    fn parse_number(&self, token: &str) -> Result<i64, std::num::ParseIntError> {
        if let Some(stripped) = token.strip_prefix('#') {
            stripped.parse::<i64>()
        } else if let Some(stripped) = token.strip_prefix('$') {
            i64::from_str_radix(stripped, 16)
        } else if let Some(stripped) = token.strip_prefix('%') {
            i64::from_str_radix(stripped, 2)
        } else {
            token.parse::<i64>()
        }
    }
}

impl Default for BootstrapCompiler {
    fn default() -> Self {
        Self::new()
    }
}
