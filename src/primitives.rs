// primitives.rs - Complete Forth Primitive Operations
// This is the foundation - everything else builds on these ~25 operations

use std::io::{self, Read, Write};

// ============================================================================
// CONSTANTS
// ============================================================================

/// Memory address for the BASE variable (stores numeric conversion radix)
pub const BASE_ADDR: usize = 0x100;

/// Memory address for the >IN variable (current parse position in input buffer)
pub const TO_IN_ADDR: usize = 0x108;

/// Memory address for the input buffer (SOURCE returns this address)
pub const INPUT_BUFFER_ADDR: usize = 0x200;

/// Maximum input buffer size
pub const INPUT_BUFFER_SIZE: usize = 1024;

/// Memory address for WORD's transient buffer (counted string output)
pub const WORD_BUFFER_ADDR: usize = 0x600;

// ============================================================================
// STACK IMPLEMENTATIONS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Stack {
    data: Vec<i64>,
}

impl Default for Stack {
    fn default() -> Self {
        Self::new()
    }
}

impl Stack {
    pub fn new() -> Self {
        Stack {
            data: Vec::with_capacity(256),
        }
    }

    pub fn push(&mut self, value: i64) {
        self.data.push(value);
    }

    pub fn pop(&mut self) -> Result<i64, ForthError> {
        self.data.pop().ok_or(ForthError::StackUnderflow)
    }

    pub fn peek(&self) -> Result<i64, ForthError> {
        self.data.last().copied().ok_or(ForthError::StackUnderflow)
    }

    pub fn depth(&self) -> usize {
        self.data.len()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get a value at the given index (0 = bottom of stack)
    pub fn get(&self, index: usize) -> Option<i64> {
        self.data.get(index).copied()
    }

    /// Set a value at the given index (0 = bottom of stack)
    pub fn set(&mut self, index: usize, value: i64) -> Result<(), ForthError> {
        if index < self.data.len() {
            self.data[index] = value;
            Ok(())
        } else {
            Err(ForthError::StackUnderflow)
        }
    }

    /// Iterate over stack values from bottom to top
    pub fn iter(&self) -> impl Iterator<Item = &i64> {
        self.data.iter()
    }

    /// Get mutable pointer to stack data (for JIT execution)
    pub fn as_mut_ptr(&mut self) -> *mut i64 {
        self.data.as_mut_ptr()
    }

    /// Set stack depth (for JIT execution)
    pub fn set_depth(&mut self, depth: usize) {
        unsafe {
            self.data.set_len(depth);
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStack {
    data: Vec<i64>,
}

impl Default for ReturnStack {
    fn default() -> Self {
        Self::new()
    }
}

impl ReturnStack {
    pub fn new() -> Self {
        ReturnStack {
            data: Vec::with_capacity(256),
        }
    }

    pub fn push(&mut self, value: i64) {
        self.data.push(value);
    }

    pub fn pop(&mut self) -> Result<i64, ForthError> {
        self.data.pop().ok_or(ForthError::ReturnStackUnderflow)
    }

    pub fn peek(&self) -> Result<i64, ForthError> {
        self.data.last().copied().ok_or(ForthError::ReturnStackUnderflow)
    }

    pub fn depth(&self) -> usize {
        self.data.len()
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn iter(&self) -> impl Iterator<Item = &i64> {
        self.data.iter()
    }

    /// Get mutable pointer to stack data (for JIT execution)
    pub fn as_mut_ptr(&mut self) -> *mut i64 {
        self.data.as_mut_ptr()
    }

    /// Set stack depth (for JIT execution)
    pub fn set_depth(&mut self, depth: usize) {
        unsafe {
            self.data.set_len(depth);
        }
    }
}

// ============================================================================
// ERROR TYPES
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ForthError {
    StackUnderflow,
    ReturnStackUnderflow,
    DivisionByZero,
    InvalidMemoryAddress,
    IoError(String),
}

impl std::fmt::Display for ForthError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ForthError::StackUnderflow => write!(f, "Stack underflow"),
            ForthError::ReturnStackUnderflow => write!(f, "Return stack underflow"),
            ForthError::DivisionByZero => write!(f, "Division by zero"),
            ForthError::InvalidMemoryAddress => write!(f, "Invalid memory address"),
            ForthError::IoError(msg) => write!(f, "I/O error: {}", msg),
        }
    }
}

impl std::error::Error for ForthError {}

// ============================================================================
// PRIMITIVE OPERATIONS
// ============================================================================

/// Macro to define all Forth primitives in a single place
/// This generates the Primitive enum, name() method, all() helper, and execute dispatcher
macro_rules! define_primitives {
    (
        $(
            $variant:ident => $name:literal : $doc:literal => $method:ident
        ),* $(,)?
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Primitive {
            $(
                #[doc = $doc]
                $variant,
            )*
        }

        impl Primitive {
            /// Get the Forth name of this primitive
            pub fn name(&self) -> &'static str {
                match self {
                    $(
                        Primitive::$variant => $name,
                    )*
                }
            }

            /// Get a primitive by its Forth name
            pub fn from_name(name: &str) -> Option<Primitive> {
                match name {
                    $(
                        $name => Some(Primitive::$variant),
                    )*
                    _ => None,
                }
            }

            /// Get all primitives as (name, primitive) pairs
            pub fn all() -> &'static [(&'static str, Primitive)] {
                &[
                    $(
                        ($name, Primitive::$variant),
                    )*
                ]
            }
        }

        // Generate the execute_primitive dispatcher for VM
        impl VM {
            pub fn execute_primitive(&mut self, prim: Primitive) -> Result<(), ForthError> {
                match prim {
                    $(
                        Primitive::$variant => self.$method(),
                    )*
                }
            }
        }
    };
}

// Define all primitives using the macro
define_primitives! {
    // Memory operations
    Fetch => "@": "@ ( addr -- n ) Fetch value from memory" => op_fetch,
    Store => "!": "! ( n addr -- ) Store value to memory" => op_store,
    CFetch => "C@": "C@ ( addr -- c ) Fetch byte from memory" => op_c_fetch,
    CStore => "C!": "C! ( c addr -- ) Store byte to memory" => op_c_store,
    Here => "HERE": "HERE ( -- addr ) Get dictionary pointer" => op_here,
    CharPlus => "CHAR+": "CHAR+ ( c-addr1 -- c-addr2 ) Add character size to address" => op_char_plus,
    Chars => "CHARS": "CHARS ( n1 -- n2 ) Size in address units of n1 characters" => op_chars,
    Base => "BASE": "BASE ( -- a-addr ) Address of cell containing current number-conversion radix" => op_base,
    ToIn => ">IN": ">IN ( -- a-addr ) Address of cell containing parse position" => op_to_in,
    Source => "SOURCE": "SOURCE ( -- c-addr u ) Address and length of input buffer" => op_source,
    Word => "WORD": "WORD ( char -- c-addr ) Parse word delimited by char, return counted string" => op_word,
    Parse => "PARSE": "PARSE ( char -- c-addr u ) Parse string delimited by char" => op_parse,
    Compare => "COMPARE": "COMPARE ( c-addr1 u1 c-addr2 u2 -- n ) Compare two strings" => op_compare,
    ToNumber => ">NUMBER": ">NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 ) Convert string to number" => op_to_number,
    SToD => "S>D": "S>D ( n -- d ) Convert single to double-cell number" => op_s_to_d,

    // Stack manipulation
    Dup => "DUP": "DUP ( n -- n n ) Duplicate top of stack" => op_dup,
    Drop => "DROP": "DROP ( n -- ) Remove top of stack" => op_drop,
    Swap => "SWAP": "SWAP ( a b -- b a ) Swap top two items" => op_swap,
    Over => "OVER": "OVER ( a b -- a b a ) Copy second item to top" => op_over,
    Rot => "ROT": "ROT ( a b c -- b c a ) Rotate top three items" => op_rot,

    // Return stack
    ToR => ">R": ">R ( n -- ) (R: -- n) Move to return stack" => op_to_r,
    FromR => "R>": "R> ( -- n ) (R: n -- ) Move from return stack" => op_from_r,
    RFetch => "R@": "R@ ( -- n ) (R: n -- n) Copy from return stack" => op_r_fetch,

    // Arithmetic
    Add => "+": "+ ( a b -- c ) Addition" => op_add,
    Sub => "-": "- ( a b -- c ) Subtraction" => op_sub,
    Mul => "*": "* ( a b -- c ) Multiplication" => op_mul,
    Div => "/": "/ ( a b -- c ) Division" => op_div,
    Mod => "MOD": "MOD ( a b -- c ) Modulo" => op_mod,

    // Comparison
    Equals => "=": "= ( a b -- flag ) Equality test" => op_equals,
    Less => "<": "< ( a b -- flag ) Less than test" => op_less,
    Greater => ">": "> ( a b -- flag ) Greater than test" => op_greater,

    // Logical
    And => "AND": "AND ( a b -- c ) Bitwise AND" => op_and,
    Or => "OR": "OR ( a b -- c ) Bitwise OR" => op_or,
    Xor => "XOR": "XOR ( a b -- c ) Bitwise XOR" => op_xor,
    Invert => "INVERT": "INVERT ( n -- ~n ) Bitwise NOT" => op_invert,

    // I/O
    Emit => "EMIT": "EMIT ( c -- ) Output character" => op_emit,
    Key => "KEY": "KEY ( -- c ) Input character" => op_key,
    Dot => ".": ". ( n -- ) Print number and space" => op_dot,
    Type => "TYPE": "TYPE ( addr len -- ) Print string from memory" => op_type,

    // Loop
    I => "I": "I ( -- n ) Get current loop index" => op_i,
    Unloop => "UNLOOP": "UNLOOP ( -- ) Discard loop control parameters" => op_unloop,

    // Stack inspection
    Depth => "DEPTH": "DEPTH ( -- n ) Get number of items on data stack" => op_depth,
}

// ============================================================================
// VIRTUAL MACHINE
// ============================================================================

pub struct VM {
    pub data_stack: Stack,
    pub return_stack: ReturnStack,
    pub memory: Vec<u8>,
    pub loop_stack: Stack, // For DO/LOOP: stores current index and limit
    pub here: usize,       // Dictionary pointer for string allocation
    pub input_length: usize, // Length of current input in buffer
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        let mut vm = VM {
            data_stack: Stack::new(),
            return_stack: ReturnStack::new(),
            memory: vec![0; 65536], // 64KB memory
            loop_stack: Stack::new(),
            here: 0x4000, // Start string allocation at 16KB
            input_length: 0,
        };

        // Initialize BASE to 10 (decimal)
        let base_bytes = 10i64.to_le_bytes();
        vm.memory[BASE_ADDR..BASE_ADDR + 8].copy_from_slice(&base_bytes);

        // Initialize >IN to 0
        let to_in_bytes = 0i64.to_le_bytes();
        vm.memory[TO_IN_ADDR..TO_IN_ADDR + 8].copy_from_slice(&to_in_bytes);

        vm
    }

    /// Set the input buffer for parsing (used by WORD, etc.)
    pub fn set_input(&mut self, input: &str) {
        let bytes = input.as_bytes();
        let len = bytes.len().min(INPUT_BUFFER_SIZE);

        // Clear the entire buffer first to avoid stale data
        self.memory[INPUT_BUFFER_ADDR..INPUT_BUFFER_ADDR + INPUT_BUFFER_SIZE].fill(0);

        // Copy input to buffer
        self.memory[INPUT_BUFFER_ADDR..INPUT_BUFFER_ADDR + len]
            .copy_from_slice(&bytes[..len]);

        // Store input length (for VM)
        self.input_length = len;

        // Store input length in memory (for LLVM access)
        let input_len_addr = INPUT_BUFFER_ADDR + INPUT_BUFFER_SIZE;
        let len_bytes = (len as i64).to_le_bytes();
        self.memory[input_len_addr..input_len_addr + 8].copy_from_slice(&len_bytes);

        // Reset >IN to 0
        let to_in_bytes = 0i64.to_le_bytes();
        self.memory[TO_IN_ADDR..TO_IN_ADDR + 8].copy_from_slice(&to_in_bytes);
    }

    /// Allocate a string in memory and return its address
    pub fn alloc_string(&mut self, s: &str) -> Result<(i64, i64), ForthError> {
        let bytes = s.as_bytes();
        let len = bytes.len();
        let addr = self.here;

        if addr + len > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        // Copy string to memory
        self.memory[addr..addr + len].copy_from_slice(bytes);
        self.here += len;

        // Return (addr, len)
        Ok((addr as i64, len as i64))
    }

    // execute_primitive is now auto-generated by the define_primitives! macro

    // ========================================================================
    // MEMORY OPERATIONS
    // ========================================================================

    fn op_fetch(&mut self) -> Result<(), ForthError> {
        // @ ( addr -- n )
        let addr = self.data_stack.pop()? as usize;
        if addr + 8 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        let bytes = &self.memory[addr..addr + 8];
        let value = i64::from_le_bytes(bytes.try_into().unwrap());
        self.data_stack.push(value);
        Ok(())
    }

    fn op_store(&mut self) -> Result<(), ForthError> {
        // ! ( n addr -- )
        let addr = self.data_stack.pop()? as usize;
        let value = self.data_stack.pop()?;
        if addr + 8 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[addr..addr + 8].copy_from_slice(&value.to_le_bytes());
        Ok(())
    }

    fn op_c_fetch(&mut self) -> Result<(), ForthError> {
        // C@ ( addr -- c )
        let addr = self.data_stack.pop()? as usize;
        if addr >= self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.data_stack.push(self.memory[addr] as i64);
        Ok(())
    }

    fn op_c_store(&mut self) -> Result<(), ForthError> {
        // C! ( c addr -- )
        let addr = self.data_stack.pop()? as usize;
        let value = self.data_stack.pop()? as u8;
        if addr >= self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[addr] = value;
        Ok(())
    }

    fn op_here(&mut self) -> Result<(), ForthError> {
        // HERE ( -- addr )
        self.data_stack.push(self.here as i64);
        Ok(())
    }

    fn op_char_plus(&mut self) -> Result<(), ForthError> {
        // CHAR+ ( c-addr1 -- c-addr2 )
        let addr = self.data_stack.pop()?;
        self.data_stack.push(addr.wrapping_add(1));
        Ok(())
    }

    fn op_chars(&mut self) -> Result<(), ForthError> {
        // CHARS ( n1 -- n2 )
        // Since characters are 1 byte, n2 = n1 * 1 = n1
        // In this implementation, CHARS is a no-op but we still pop and push for consistency
        let n = self.data_stack.pop()?;
        self.data_stack.push(n);
        Ok(())
    }

    fn op_base(&mut self) -> Result<(), ForthError> {
        // BASE ( -- a-addr )
        // Push the address of the BASE variable onto the stack
        self.data_stack.push(BASE_ADDR as i64);
        Ok(())
    }

    fn op_to_in(&mut self) -> Result<(), ForthError> {
        // >IN ( -- a-addr )
        // Push the address of the >IN variable onto the stack
        self.data_stack.push(TO_IN_ADDR as i64);
        Ok(())
    }

    fn op_source(&mut self) -> Result<(), ForthError> {
        // SOURCE ( -- c-addr u )
        // Return address and length of input buffer
        self.data_stack.push(INPUT_BUFFER_ADDR as i64);
        self.data_stack.push(self.input_length as i64);
        Ok(())
    }

    fn op_word(&mut self) -> Result<(), ForthError> {
        // WORD ( char -- c-addr )
        // Parse word delimited by char, return counted string address

        let delimiter = self.data_stack.pop()? as u8;

        // Get current >IN position
        let mut to_in_bytes = [0u8; 8];
        to_in_bytes.copy_from_slice(&self.memory[TO_IN_ADDR..TO_IN_ADDR + 8]);
        let mut pos = i64::from_le_bytes(to_in_bytes) as usize;

        // Skip leading delimiters
        while pos < self.input_length && self.memory[INPUT_BUFFER_ADDR + pos] == delimiter {
            pos += 1;
        }

        // Collect characters until delimiter or end
        let start_pos = pos;
        while pos < self.input_length && self.memory[INPUT_BUFFER_ADDR + pos] != delimiter {
            pos += 1;
        }

        let word_len = pos - start_pos;

        // Store as counted string at WORD_BUFFER_ADDR
        // First byte is length
        self.memory[WORD_BUFFER_ADDR] = word_len.min(255) as u8;

        // Copy the characters using a temporary buffer to avoid borrow checker issues
        if word_len > 0 {
            let mut temp = Vec::with_capacity(word_len);
            temp.extend_from_slice(&self.memory[INPUT_BUFFER_ADDR + start_pos..INPUT_BUFFER_ADDR + start_pos + word_len]);
            self.memory[WORD_BUFFER_ADDR + 1..WORD_BUFFER_ADDR + 1 + word_len].copy_from_slice(&temp);
        }

        // Update >IN
        let new_to_in = pos as i64;
        let to_in_bytes = new_to_in.to_le_bytes();
        self.memory[TO_IN_ADDR..TO_IN_ADDR + 8].copy_from_slice(&to_in_bytes);

        // Push address of counted string
        self.data_stack.push(WORD_BUFFER_ADDR as i64);

        Ok(())
    }

    fn op_parse(&mut self) -> Result<(), ForthError> {
        // PARSE ( char -- c-addr u )
        // Parse string delimited by char, return address and length
        // Unlike WORD, does NOT skip leading delimiters

        let delimiter = self.data_stack.pop()? as u8;

        // Get current >IN position
        let mut to_in_bytes = [0u8; 8];
        to_in_bytes.copy_from_slice(&self.memory[TO_IN_ADDR..TO_IN_ADDR + 8]);
        let pos = i64::from_le_bytes(to_in_bytes) as usize;

        // Start parsing from current position (no skipping)
        let start_pos = pos;
        let mut end_pos = pos;

        // Find delimiter or end of input
        while end_pos < self.input_length && self.memory[INPUT_BUFFER_ADDR + end_pos] != delimiter {
            end_pos += 1;
        }

        let parsed_len = end_pos - start_pos;

        // Update >IN to position after delimiter (or end of input)
        // If we found a delimiter, skip over it
        let new_to_in = if end_pos < self.input_length {
            end_pos + 1  // Skip the delimiter
        } else {
            end_pos  // At end of input
        };
        let to_in_bytes = (new_to_in as i64).to_le_bytes();
        self.memory[TO_IN_ADDR..TO_IN_ADDR + 8].copy_from_slice(&to_in_bytes);

        // Push address and length
        self.data_stack.push((INPUT_BUFFER_ADDR + start_pos) as i64);
        self.data_stack.push(parsed_len as i64);

        Ok(())
    }

    fn op_compare(&mut self) -> Result<(), ForthError> {
        // COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
        // Compare two strings
        // Return: 0 if equal, -1 if s1 < s2, 1 if s1 > s2

        let u2 = self.data_stack.pop()? as usize;
        let c_addr2 = self.data_stack.pop()? as usize;
        let u1 = self.data_stack.pop()? as usize;
        let c_addr1 = self.data_stack.pop()? as usize;

        // Validate memory addresses
        if c_addr1 + u1 > self.memory.len() || c_addr2 + u2 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        // Compare up to the length of the shorter string
        let min_len = u1.min(u2);

        for i in 0..min_len {
            let byte1 = self.memory[c_addr1 + i];
            let byte2 = self.memory[c_addr2 + i];

            if byte1 < byte2 {
                self.data_stack.push(-1);
                return Ok(());
            } else if byte1 > byte2 {
                self.data_stack.push(1);
                return Ok(());
            }
        }

        // Strings are equal up to min_len
        // Now compare lengths
        if u1 == u2 {
            self.data_stack.push(0); // Strings are identical
        } else if u1 < u2 {
            self.data_stack.push(-1); // s1 is shorter
        } else {
            self.data_stack.push(1); // s1 is longer
        }

        Ok(())
    }

    fn op_to_number(&mut self) -> Result<(), ForthError> {
        // >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        // Convert string to number, respecting BASE
        // ud1 is initial double number (we'll treat as single for now)
        // Returns updated number and remaining unparsed string

        let u1 = self.data_stack.pop()? as usize;
        let c_addr1 = self.data_stack.pop()? as usize;
        let ud1 = self.data_stack.pop()? as u64; // Initial value

        // Get current BASE
        let mut base_bytes = [0u8; 8];
        base_bytes.copy_from_slice(&self.memory[BASE_ADDR..BASE_ADDR + 8]);
        let base = i64::from_le_bytes(base_bytes) as u32;

        if !(2..=36).contains(&base) {
            return Err(ForthError::InvalidMemoryAddress); // Invalid base
        }

        // Validate memory address
        if c_addr1 + u1 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        let mut result = ud1;
        let mut pos = 0;

        // Process characters
        while pos < u1 {
            let ch = self.memory[c_addr1 + pos] as char;

            // Convert character to digit value
            let digit_value = if ch.is_ascii_digit() {
                (ch as u32) - ('0' as u32)
            } else if ch.is_ascii_uppercase() {
                (ch as u32) - ('A' as u32) + 10
            } else if ch.is_ascii_lowercase() {
                (ch as u32) - ('a' as u32) + 10
            } else {
                // Not a valid digit, stop processing
                break;
            };

            // Check if digit is valid for current base
            if digit_value >= base {
                break;
            }

            // Accumulate: result = result * base + digit
            result = result.wrapping_mul(base as u64).wrapping_add(digit_value as u64);
            pos += 1;
        }

        // Push results: ud2, c-addr2, u2
        self.data_stack.push(result as i64); // ud2
        self.data_stack.push((c_addr1 + pos) as i64); // c-addr2 (remaining string)
        self.data_stack.push((u1 - pos) as i64); // u2 (remaining length)

        Ok(())
    }

    fn op_s_to_d(&mut self) -> Result<(), ForthError> {
        // S>D ( n -- d )
        // Convert single-cell number to double-cell with same value
        // For negative n, high cell is -1; for non-negative n, high cell is 0
        let n = self.data_stack.pop()?;
        let high = if n < 0 { -1 } else { 0 };
        self.data_stack.push(n);    // low cell
        self.data_stack.push(high);  // high cell
        Ok(())
    }

    // ========================================================================
    // STACK MANIPULATION
    // ========================================================================

    fn op_dup(&mut self) -> Result<(), ForthError> {
        // DUP ( n -- n n )
        let value = self.data_stack.peek()?;
        self.data_stack.push(value);
        Ok(())
    }

    fn op_drop(&mut self) -> Result<(), ForthError> {
        // DROP ( n -- )
        self.data_stack.pop()?;
        Ok(())
    }

    fn op_swap(&mut self) -> Result<(), ForthError> {
        // SWAP ( a b -- b a )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(b);
        self.data_stack.push(a);
        Ok(())
    }

    fn op_over(&mut self) -> Result<(), ForthError> {
        // OVER ( a b -- a b a )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a);
        self.data_stack.push(b);
        self.data_stack.push(a);
        Ok(())
    }

    fn op_rot(&mut self) -> Result<(), ForthError> {
        // ROT ( a b c -- b c a )
        let c = self.data_stack.pop()?;
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(b);
        self.data_stack.push(c);
        self.data_stack.push(a);
        Ok(())
    }

    // ========================================================================
    // RETURN STACK
    // ========================================================================

    fn op_to_r(&mut self) -> Result<(), ForthError> {
        // >R ( n -- ) (R: -- n)
        let value = self.data_stack.pop()?;
        self.return_stack.push(value);
        Ok(())
    }

    fn op_from_r(&mut self) -> Result<(), ForthError> {
        // R> ( -- n ) (R: n -- )
        let value = self.return_stack.pop()?;
        self.data_stack.push(value);
        Ok(())
    }

    fn op_r_fetch(&mut self) -> Result<(), ForthError> {
        // R@ ( -- n ) (R: n -- n)
        let value = self.return_stack.peek()?;
        self.data_stack.push(value);
        Ok(())
    }

    // ========================================================================
    // ARITHMETIC
    // ========================================================================

    fn op_add(&mut self) -> Result<(), ForthError> {
        // + ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a.wrapping_add(b));
        Ok(())
    }

    fn op_sub(&mut self) -> Result<(), ForthError> {
        // - ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a.wrapping_sub(b));
        Ok(())
    }

    fn op_mul(&mut self) -> Result<(), ForthError> {
        // * ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a.wrapping_mul(b));
        Ok(())
    }

    fn op_div(&mut self) -> Result<(), ForthError> {
        // / ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        if b == 0 {
            return Err(ForthError::DivisionByZero);
        }
        self.data_stack.push(a / b);
        Ok(())
    }

    fn op_mod(&mut self) -> Result<(), ForthError> {
        // MOD ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        if b == 0 {
            return Err(ForthError::DivisionByZero);
        }
        self.data_stack.push(a % b);
        Ok(())
    }

    // ========================================================================
    // COMPARISON
    // ========================================================================

    fn op_equals(&mut self) -> Result<(), ForthError> {
        // = ( a b -- flag )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(if a == b { -1 } else { 0 });
        Ok(())
    }

    fn op_less(&mut self) -> Result<(), ForthError> {
        // < ( a b -- flag )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(if a < b { -1 } else { 0 });
        Ok(())
    }

    fn op_greater(&mut self) -> Result<(), ForthError> {
        // > ( a b -- flag )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(if a > b { -1 } else { 0 });
        Ok(())
    }

    // ========================================================================
    // LOGICAL
    // ========================================================================

    fn op_and(&mut self) -> Result<(), ForthError> {
        // AND ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a & b);
        Ok(())
    }

    fn op_or(&mut self) -> Result<(), ForthError> {
        // OR ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a | b);
        Ok(())
    }

    fn op_xor(&mut self) -> Result<(), ForthError> {
        // XOR ( a b -- c )
        let b = self.data_stack.pop()?;
        let a = self.data_stack.pop()?;
        self.data_stack.push(a ^ b);
        Ok(())
    }

    fn op_invert(&mut self) -> Result<(), ForthError> {
        // INVERT ( n -- ~n )
        let value = self.data_stack.pop()?;
        self.data_stack.push(!value);
        Ok(())
    }

    // ========================================================================
    // I/O
    // ========================================================================

    fn op_emit(&mut self) -> Result<(), ForthError> {
        // EMIT ( c -- )
        let c = self.data_stack.pop()? as u8;
        io::stdout()
            .write_all(&[c])
            .map_err(|e| ForthError::IoError(e.to_string()))?;
        io::stdout()
            .flush()
            .map_err(|e| ForthError::IoError(e.to_string()))?;
        Ok(())
    }

    fn op_key(&mut self) -> Result<(), ForthError> {
        // KEY ( -- c )
        let mut buffer = [0u8; 1];
        io::stdin()
            .read_exact(&mut buffer)
            .map_err(|e| ForthError::IoError(e.to_string()))?;
        self.data_stack.push(buffer[0] as i64);
        Ok(())
    }

    fn op_dot(&mut self) -> Result<(), ForthError> {
        // . ( n -- )
        // Pop number from stack and print it followed by a space
        let n = self.data_stack.pop()?;
        print!("{} ", n);
        io::stdout()
            .flush()
            .map_err(|e| ForthError::IoError(e.to_string()))?;
        Ok(())
    }

    fn op_type(&mut self) -> Result<(), ForthError> {
        // TYPE ( addr len -- )
        // Print string from memory
        let len = self.data_stack.pop()? as usize;
        let addr = self.data_stack.pop()? as usize;

        if addr + len > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        let bytes = &self.memory[addr..addr + len];
        let s = String::from_utf8_lossy(bytes);
        print!("{}", s);
        io::stdout()
            .flush()
            .map_err(|e| ForthError::IoError(e.to_string()))?;
        Ok(())
    }

    // ========================================================================
    // LOOP COUNTER
    // ========================================================================

    fn op_i(&mut self) -> Result<(), ForthError> {
        // I ( -- n )
        // Push the current loop index onto the data stack
        // Loop stack has [index, limit] so index is at depth - 2
        if self.loop_stack.depth() < 2 {
            return Err(ForthError::StackUnderflow);
        }
        let index = self.loop_stack.get(self.loop_stack.depth() - 2)
            .ok_or(ForthError::StackUnderflow)?;
        self.data_stack.push(index);
        Ok(())
    }

    fn op_unloop(&mut self) -> Result<(), ForthError> {
        // UNLOOP ( -- )
        // Discard the loop control parameters from the loop stack
        // Loop stack has [index, limit], so pop both
        if self.loop_stack.depth() < 2 {
            return Err(ForthError::StackUnderflow);
        }
        self.loop_stack.pop()?; // Pop limit
        self.loop_stack.pop()?; // Pop index
        Ok(())
    }

    // ========================================================================
    // STACK INSPECTION
    // ========================================================================

    fn op_depth(&mut self) -> Result<(), ForthError> {
        // DEPTH ( -- n )
        // Push the number of items on the data stack
        let depth = self.data_stack.depth() as i64;
        self.data_stack.push(depth);
        Ok(())
    }

}

