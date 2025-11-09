// primitives.rs - Complete Forth Primitive Operations
// This is the foundation - everything else builds on these ~25 operations

use std::io::{self, Read, Write};

// ============================================================================
// CONSTANTS
// ============================================================================

/// Memory address for the BASE variable (stores numeric conversion radix)
pub const BASE_ADDR: usize = 0x100;

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
    Cr => "CR": "CR ( -- ) Print newline" => op_cr,
    Type => "TYPE": "TYPE ( addr len -- ) Print string from memory" => op_type,

    // Loop
    I => "I": "I ( -- n ) Get current loop index" => op_i,

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
        };

        // Initialize BASE to 10 (decimal)
        let base_bytes = 10i64.to_le_bytes();
        vm.memory[BASE_ADDR..BASE_ADDR + 8].copy_from_slice(&base_bytes);

        vm
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

