// primitives.rs - Complete Forth Primitive Operations
// This is the foundation - everything else builds on these ~25 operations

use std::io::{self, Read, Write};

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

    /// Iterate over stack values from bottom to top
    pub fn iter(&self) -> impl Iterator<Item = &i64> {
        self.data.iter()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Primitive {
    // Memory operations
    Fetch,      // @ ( addr -- n )
    Store,      // ! ( n addr -- )
    CFetch,     // C@ ( addr -- c )
    CStore,     // C! ( c addr -- )

    // Stack manipulation
    Dup,        // DUP ( n -- n n )
    Drop,       // DROP ( n -- )
    Swap,       // SWAP ( a b -- b a )
    Over,       // OVER ( a b -- a b a )
    Rot,        // ROT ( a b c -- b c a )

    // Return stack
    ToR,        // >R ( n -- ) (R: -- n)
    FromR,      // R> ( -- n ) (R: n -- )
    RFetch,     // R@ ( -- n ) (R: n -- n)

    // Arithmetic
    Add,        // + ( a b -- c )
    Sub,        // - ( a b -- c )
    Mul,        // * ( a b -- c )
    Div,        // / ( a b -- c )
    Mod,        // MOD ( a b -- c )

    // Comparison
    Equals,     // = ( a b -- flag )
    Less,       // < ( a b -- flag )
    Greater,    // > ( a b -- flag )

    // Logical
    And,        // AND ( a b -- c )
    Or,         // OR ( a b -- c )
    Xor,        // XOR ( a b -- c )
    Invert,     // INVERT ( n -- ~n )

    // I/O
    Emit,       // EMIT ( c -- )
    Key,        // KEY ( -- c )
    Dot,        // . ( n -- ) Print number and space
    Cr,         // CR ( -- ) Print newline
    Type,       // TYPE ( addr len -- ) Print string from memory

    // Loop
    I,          // I ( -- n ) Get current loop index

    // Stack inspection
    Depth,      // DEPTH ( -- n ) Get number of items on data stack
}

impl Primitive {
    pub fn name(&self) -> &'static str {
        match self {
            Primitive::Fetch => "@",
            Primitive::Store => "!",
            Primitive::CFetch => "C@",
            Primitive::CStore => "C!",
            Primitive::Dup => "DUP",
            Primitive::Drop => "DROP",
            Primitive::Swap => "SWAP",
            Primitive::Over => "OVER",
            Primitive::Rot => "ROT",
            Primitive::ToR => ">R",
            Primitive::FromR => "R>",
            Primitive::RFetch => "R@",
            Primitive::Add => "+",
            Primitive::Sub => "-",
            Primitive::Mul => "*",
            Primitive::Div => "/",
            Primitive::Mod => "MOD",
            Primitive::Equals => "=",
            Primitive::Less => "<",
            Primitive::Greater => ">",
            Primitive::And => "AND",
            Primitive::Or => "OR",
            Primitive::Xor => "XOR",
            Primitive::Invert => "INVERT",
            Primitive::Emit => "EMIT",
            Primitive::Key => "KEY",
            Primitive::Dot => ".",
            Primitive::Cr => "CR",
            Primitive::Type => "TYPE",
            Primitive::I => "I",
            Primitive::Depth => "DEPTH",
        }
    }
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
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        VM {
            data_stack: Stack::new(),
            return_stack: ReturnStack::new(),
            memory: vec![0; 65536], // 64KB memory
            loop_stack: Stack::new(),
            here: 0x4000, // Start string allocation at 16KB
        }
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

    pub fn execute_primitive(&mut self, prim: Primitive) -> Result<(), ForthError> {
        match prim {
            // Memory operations
            Primitive::Fetch => self.op_fetch(),
            Primitive::Store => self.op_store(),
            Primitive::CFetch => self.op_c_fetch(),
            Primitive::CStore => self.op_c_store(),

            // Stack manipulation
            Primitive::Dup => self.op_dup(),
            Primitive::Drop => self.op_drop(),
            Primitive::Swap => self.op_swap(),
            Primitive::Over => self.op_over(),
            Primitive::Rot => self.op_rot(),

            // Return stack
            Primitive::ToR => self.op_to_r(),
            Primitive::FromR => self.op_from_r(),
            Primitive::RFetch => self.op_r_fetch(),

            // Arithmetic
            Primitive::Add => self.op_add(),
            Primitive::Sub => self.op_sub(),
            Primitive::Mul => self.op_mul(),
            Primitive::Div => self.op_div(),
            Primitive::Mod => self.op_mod(),

            // Comparison
            Primitive::Equals => self.op_equals(),
            Primitive::Less => self.op_less(),
            Primitive::Greater => self.op_greater(),

            // Logical
            Primitive::And => self.op_and(),
            Primitive::Or => self.op_or(),
            Primitive::Xor => self.op_xor(),
            Primitive::Invert => self.op_invert(),

            // I/O
            Primitive::Emit => self.op_emit(),
            Primitive::Key => self.op_key(),
            Primitive::Dot => self.op_dot(),
            Primitive::Cr => self.op_cr(),
            Primitive::Type => self.op_type(),

            // Loop counter
            Primitive::I => self.op_i(),

            // Stack inspection
            Primitive::Depth => self.op_depth(),
        }
    }

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

    fn op_cr(&mut self) -> Result<(), ForthError> {
        // CR ( -- )
        // Print a newline
        println!();
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
        // The loop index is stored on top of the loop stack
        if self.loop_stack.depth() == 0 {
            return Err(ForthError::StackUnderflow);
        }
        let index = self.loop_stack.get(self.loop_stack.depth() - 1)
            .ok_or(ForthError::StackUnderflow)?;
        self.data_stack.push(index);
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

