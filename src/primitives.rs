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

/// Memory address for the LATEST variable (pointer to most recent dictionary entry)
pub const LATEST_ADDR: usize = 0x110;

/// Memory address for the DP (Dictionary Pointer) variable
pub const DP_ADDR: usize = 0x118;

/// Memory address for the STATE variable (0 = interpret, 1 = compile)
pub const STATE_ADDR: usize = 0x120;

/// Dictionary starts here (after system variables)
pub const DICTIONARY_START: usize = 0x800;

// ============================================================================
// DICTIONARY STRUCTURE
// ============================================================================
// Each dictionary entry in memory:
// | LINK (8 bytes) | FLAGS (8 bytes) | NAME-LEN (1 byte) | NAME (n bytes) | CODE-ADDR (8 bytes) | CODE... |
//
// LINK:      Pointer to previous word (0 if no previous)
// FLAGS:     Bit 0 = IMMEDIATE, Bit 1 = HIDDEN
// NAME-LEN:  Length of name
// NAME:      Name characters
// CODE-ADDR: Bytecode address where this word's code starts

pub const F_IMMEDIATE: i64 = 1;
pub const F_HIDDEN: i64 = 2;

// ============================================================================
// BYTECODE OPCODES (for memory-based bytecode)
// ============================================================================
// Each instruction stored as: [OPCODE (1 byte)] [DATA (variable bytes)]

pub const OP_PRIMITIVE: u8 = 0x01;      // + prim_id (1 byte)
pub const OP_LITERAL: u8 = 0x02;        // + value (8 bytes)
pub const OP_JUMP: u8 = 0x03;           // + addr (8 bytes)
pub const OP_BRANCH0: u8 = 0x04;        // + addr (8 bytes) - JumpIfZero
pub const OP_BRANCHNZ: u8 = 0x05;       // + addr (8 bytes) - JumpIfNotZero
pub const OP_CALL: u8 = 0x06;           // + addr (8 bytes)
pub const OP_RETURN: u8 = 0x07;         // no data
pub const OP_PUSHVAR: u8 = 0x08;        // + offset (8 bytes)
pub const OP_PUSHCONST: u8 = 0x09;      // + value (8 bytes)
pub const OP_DO_SETUP: u8 = 0x0A;       // no data
pub const OP_QDO_SETUP: u8 = 0x0B;      // + addr (8 bytes)
pub const OP_LOOP_CHECK: u8 = 0x0C;     // + addr (8 bytes)
pub const OP_PLOOP_CHECK: u8 = 0x0D;    // + addr (8 bytes)
pub const OP_LOOP_END: u8 = 0x0E;       // no data

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

            /// Get the numeric ID of this primitive (for bytecode serialization)
            /// IDs are assigned based on declaration order
            pub fn id(&self) -> u8 {
                // Get position in all() array
                for (idx, (_name, prim)) in Self::all().iter().enumerate() {
                    if prim == self {
                        return idx as u8;
                    }
                }
                0 // Should never happen
            }

            /// Get a primitive by its numeric ID
            pub fn from_id(id: u8) -> Option<Primitive> {
                Self::all().get(id as usize).map(|(_name, prim)| *prim)
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
    Comma => ",": ", ( n -- ) Compile cell into dictionary at HERE" => op_comma,
    CComma => "C,": "C, ( c -- ) Compile byte into dictionary at HERE" => op_c_comma,
    Allot => "ALLOT": "ALLOT ( n -- ) Allocate n bytes in dictionary" => op_allot,
    Aligned => "ALIGNED": "ALIGNED ( addr -- addr' ) Align address to cell boundary" => op_aligned,
    StateAddr => "STATE": "STATE ( -- a-addr ) Address of compilation state variable" => op_state_addr,
    LatestAddr => "LATEST": "LATEST ( -- a-addr ) Address of latest word pointer" => op_latest_addr,
    DpAddr => "DP": "DP ( -- a-addr ) Address of dictionary pointer" => op_dp_addr,
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

    // Dictionary and execution
    Create => "CREATE": "CREATE ( c-addr u -- ) Create dictionary header" => op_create,
    Find => "FIND": "FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 ) Find word in dictionary" => op_find,
    Execute => "EXECUTE": "EXECUTE ( xt -- ) Execute execution token" => op_execute,

    // Bytecode compilation primitives
    CompilePrimitive => "COMPILE-PRIM": "COMPILE-PRIM ( prim-id -- ) Compile primitive bytecode" => op_compile_primitive,
    CompileLiteral => "COMPILE-LIT": "COMPILE-LIT ( n -- ) Compile literal bytecode" => op_compile_literal,
    CompileCall => "COMPILE-CALL": "COMPILE-CALL ( addr -- ) Compile call bytecode" => op_compile_call,
    CompileReturn => "COMPILE-RETURN": "COMPILE-RETURN ( -- ) Compile return bytecode" => op_compile_return,
    CompileBranch0 => "COMPILE-BRANCH0": "COMPILE-BRANCH0 ( addr -- ) Compile branch-if-zero bytecode" => op_compile_branch0,
    CompileBranchNZ => "COMPILE-BRANCHNZ": "COMPILE-BRANCHNZ ( addr -- ) Compile branch-if-not-zero bytecode" => op_compile_branchnz,
    CompileJump => "COMPILE-JUMP": "COMPILE-JUMP ( addr -- ) Compile unconditional jump bytecode" => op_compile_jump,
    CompileDoSetup => "COMPILE-DO-SETUP": "COMPILE-DO-SETUP ( -- ) Compile DO setup bytecode" => op_compile_do_setup,
    CompileLoopCheck => "COMPILE-LOOP-CHECK": "COMPILE-LOOP-CHECK ( addr -- ) Compile LOOP check bytecode" => op_compile_loop_check,
    CompileLoopEnd => "COMPILE-LOOP-END": "COMPILE-LOOP-END ( -- ) Compile LOOP end bytecode" => op_compile_loop_end,

    // Stack inspection
    Depth => "DEPTH": "DEPTH ( -- n ) Get number of items on data stack" => op_depth,

    // Dictionary manipulation
    Immediate => "IMMEDIATE": "IMMEDIATE ( -- ) Mark last word as immediate" => op_immediate,
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
    pub pending_execute: Option<usize>, // Address to execute (set by EXECUTE primitive)
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
            pending_execute: None,
        };

        // Initialize BASE to 10 (decimal)
        let base_bytes = 10i64.to_le_bytes();
        vm.memory[BASE_ADDR..BASE_ADDR + 8].copy_from_slice(&base_bytes);

        // Initialize >IN to 0
        let to_in_bytes = 0i64.to_le_bytes();
        vm.memory[TO_IN_ADDR..TO_IN_ADDR + 8].copy_from_slice(&to_in_bytes);

        // Initialize LATEST to 0 (no words yet)
        let latest_bytes = 0i64.to_le_bytes();
        vm.memory[LATEST_ADDR..LATEST_ADDR + 8].copy_from_slice(&latest_bytes);

        // Initialize DP (Dictionary Pointer) to start of dictionary
        let dp_bytes = (DICTIONARY_START as i64).to_le_bytes();
        vm.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&dp_bytes);

        // Initialize STATE to 0 (interpret mode)
        let state_bytes = 0i64.to_le_bytes();
        vm.memory[STATE_ADDR..STATE_ADDR + 8].copy_from_slice(&state_bytes);

        // Pre-populate dictionary with all primitives
        vm.populate_primitives();

        vm
    }

    /// Populate the memory-based dictionary with all primitive operations
    fn populate_primitives(&mut self) {
        for (name, prim) in Primitive::all() {
            let name_bytes = name.as_bytes();

            // Get current DP
            let mut dp_bytes = [0u8; 8];
            dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
            let mut dp = i64::from_le_bytes(dp_bytes) as usize;
            let entry_start = dp;

            // Write LINK field (previous LATEST value)
            let mut latest_bytes = [0u8; 8];
            latest_bytes.copy_from_slice(&self.memory[LATEST_ADDR..LATEST_ADDR + 8]);
            self.memory[dp..dp + 8].copy_from_slice(&latest_bytes);
            dp += 8;

            // Write FLAGS field (0 for primitives)
            let flags_bytes = 0i64.to_le_bytes();
            self.memory[dp..dp + 8].copy_from_slice(&flags_bytes);
            dp += 8;

            // Write NAME-LEN field
            self.memory[dp] = name_bytes.len() as u8;
            dp += 1;

            // Write NAME field
            self.memory[dp..dp + name_bytes.len()].copy_from_slice(name_bytes);
            dp += name_bytes.len();

            // Align to 8-byte boundary before CODE-ADDR
            dp = (dp + 7) & !7;

            // Write CODE-ADDR field (points to bytecode)
            let code_addr = dp + 8;
            let code_addr_bytes = (code_addr as i64).to_le_bytes();
            self.memory[dp..dp + 8].copy_from_slice(&code_addr_bytes);
            dp += 8;

            // Write bytecode: OP_PRIMITIVE + prim_id + OP_RETURN
            self.memory[dp] = OP_PRIMITIVE;
            dp += 1;
            self.memory[dp] = prim.id();
            dp += 1;
            self.memory[dp] = OP_RETURN;
            dp += 1;

            // Update DP
            let new_dp_bytes = (dp as i64).to_le_bytes();
            self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp_bytes);

            // Update LATEST to point to this new word
            let new_latest_bytes = (entry_start as i64).to_le_bytes();
            self.memory[LATEST_ADDR..LATEST_ADDR + 8].copy_from_slice(&new_latest_bytes);
        }
    }

    /// Set the input buffer for parsing (used by WORD, etc.)
    pub fn set_input(&mut self, input: &str) {
        let bytes = input.as_bytes();
        let len = bytes.len().min(INPUT_BUFFER_SIZE);

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

    fn op_immediate(&mut self) -> Result<(), ForthError> {
        // IMMEDIATE ( -- )
        // Mark the most recently defined word as IMMEDIATE
        // Get LATEST
        let mut latest_bytes = [0u8; 8];
        latest_bytes.copy_from_slice(&self.memory[LATEST_ADDR..LATEST_ADDR + 8]);
        let latest = i64::from_le_bytes(latest_bytes) as usize;

        if latest == 0 {
            return Err(ForthError::InvalidMemoryAddress);
        }

        // FLAGS field is at offset 8 from word start (after LINK)
        let flags_addr = latest + 8;

        // Read current flags
        let mut flags_bytes = [0u8; 8];
        flags_bytes.copy_from_slice(&self.memory[flags_addr..flags_addr + 8]);
        let mut flags = i64::from_le_bytes(flags_bytes);

        // Set IMMEDIATE bit (bit 0)
        flags |= F_IMMEDIATE;

        // Write back
        self.memory[flags_addr..flags_addr + 8].copy_from_slice(&flags.to_le_bytes());

        Ok(())
    }

    fn op_comma(&mut self) -> Result<(), ForthError> {
        // , ( n -- )
        // Compile a cell (8 bytes) into dictionary at HERE, then advance HERE
        let value = self.data_stack.pop()?;
        let here = self.here;

        if here + 8 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        // Store as little-endian bytes
        self.memory[here..here + 8].copy_from_slice(&value.to_le_bytes());
        self.here += 8;
        Ok(())
    }

    fn op_c_comma(&mut self) -> Result<(), ForthError> {
        // C, ( c -- )
        // Compile a byte into dictionary at HERE, then advance HERE
        let value = self.data_stack.pop()?;
        let here = self.here;

        if here >= self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        self.memory[here] = value as u8;
        self.here += 1;
        Ok(())
    }

    fn op_allot(&mut self) -> Result<(), ForthError> {
        // ALLOT ( n -- )
        // Allocate n bytes in dictionary by advancing DP
        let n = self.data_stack.pop()? as usize;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Advance DP by n bytes
        let new_dp = dp + n;
        if new_dp > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }

        let new_dp_bytes = (new_dp as i64).to_le_bytes();
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp_bytes);

        Ok(())
    }

    fn op_aligned(&mut self) -> Result<(), ForthError> {
        // ALIGNED ( addr -- addr' )
        // Align address to 8-byte boundary
        let addr = self.data_stack.pop()? as usize;
        let aligned = (addr + 7) & !7;
        self.data_stack.push(aligned as i64);
        Ok(())
    }

    fn op_state_addr(&mut self) -> Result<(), ForthError> {
        // STATE ( -- a-addr )
        // Push address of STATE variable
        self.data_stack.push(STATE_ADDR as i64);
        Ok(())
    }

    fn op_latest_addr(&mut self) -> Result<(), ForthError> {
        // LATEST ( -- a-addr )
        // Push address of LATEST variable
        self.data_stack.push(LATEST_ADDR as i64);
        Ok(())
    }

    fn op_dp_addr(&mut self) -> Result<(), ForthError> {
        // DP ( -- a-addr )
        // Push address of DP (dictionary pointer) variable
        self.data_stack.push(DP_ADDR as i64);
        Ok(())
    }

    fn op_create(&mut self) -> Result<(), ForthError> {
        // CREATE ( c-addr u -- )
        // Create a dictionary entry header in memory
        let len = self.data_stack.pop()? as usize;
        let name_addr = self.data_stack.pop()? as usize;

        // Get current DP (Dictionary Pointer)
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let mut dp = i64::from_le_bytes(dp_bytes) as usize;
        let entry_start = dp; // Save start of entry for LATEST

        // Get current LATEST
        let mut latest_bytes = [0u8; 8];
        latest_bytes.copy_from_slice(&self.memory[LATEST_ADDR..LATEST_ADDR + 8]);
        let latest = i64::from_le_bytes(latest_bytes) as usize;

        // Write LINK field (8 bytes) - points to previous word
        let link_bytes = (latest as i64).to_le_bytes();
        self.memory[dp..dp + 8].copy_from_slice(&link_bytes);
        dp += 8;

        // Write FLAGS field (8 bytes) - initially 0
        let flags_bytes = 0i64.to_le_bytes();
        self.memory[dp..dp + 8].copy_from_slice(&flags_bytes);
        dp += 8;

        // Write NAME-LEN field (1 byte)
        self.memory[dp] = len as u8;
        dp += 1;

        // Write NAME field (len bytes)
        if name_addr + len > self.memory.len() || dp + len > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory.copy_within(name_addr..name_addr + len, dp);
        dp += len;

        // Align to 8-byte boundary
        dp = (dp + 7) & !7;

        // Write CODE-ADDR field (8 bytes) - points to where bytecode will start
        let code_addr = (dp + 8) as i64;
        let code_addr_bytes = code_addr.to_le_bytes();
        self.memory[dp..dp + 8].copy_from_slice(&code_addr_bytes);
        dp += 8;

        // Update LATEST to point to this new word (start of entry)
        let new_latest_bytes = (entry_start as i64).to_le_bytes();
        self.memory[LATEST_ADDR..LATEST_ADDR + 8].copy_from_slice(&new_latest_bytes);

        // Update DP to current position
        let dp_bytes = (dp as i64).to_le_bytes();
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&dp_bytes);

        Ok(())
    }

    fn op_find(&mut self) -> Result<(), ForthError> {
        // FIND ( c-addr -- c-addr 0 | xt 1 | xt -1 )
        // Search dictionary for word (counted string at c-addr)
        // Returns: c-addr 0 (not found) | xt 1 (found, non-immediate) | xt -1 (found, immediate)

        let name_addr = self.data_stack.peek()? as usize; // Keep on stack in case not found

        // Get counted string (length byte + characters)
        if name_addr >= self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        let search_len = self.memory[name_addr] as usize;
        let search_name_addr = name_addr + 1;

        // Get LATEST (start of dictionary chain)
        let mut latest_bytes = [0u8; 8];
        latest_bytes.copy_from_slice(&self.memory[LATEST_ADDR..LATEST_ADDR + 8]);
        let mut current = i64::from_le_bytes(latest_bytes) as usize;

        // Walk the dictionary chain
        while current != 0 {
            // Read LINK field (offset 0)
            let mut link_bytes = [0u8; 8];
            if current + 8 > self.memory.len() {
                break;
            }
            link_bytes.copy_from_slice(&self.memory[current..current + 8]);
            let link = i64::from_le_bytes(link_bytes) as usize;

            // Read FLAGS field (offset 8)
            let mut flags_bytes = [0u8; 8];
            flags_bytes.copy_from_slice(&self.memory[current + 8..current + 16]);
            let flags = i64::from_le_bytes(flags_bytes);

            // Read NAME-LEN field (offset 16)
            let name_len = self.memory[current + 16] as usize;

            // Compare names (case-insensitive)
            if name_len == search_len {
                let dict_name_addr = current + 17;
                let mut match_found = true;

                for i in 0..name_len {
                    let c1 = self.memory[search_name_addr + i].to_ascii_uppercase();
                    let c2 = self.memory[dict_name_addr + i].to_ascii_uppercase();
                    if c1 != c2 {
                        match_found = false;
                        break;
                    }
                }

                if match_found {
                    // Found it! Get CODE-ADDR
                    let code_addr_offset = current + 17 + name_len;
                    let aligned_offset = (code_addr_offset + 7) & !7;

                    let mut code_addr_bytes = [0u8; 8];
                    code_addr_bytes.copy_from_slice(&self.memory[aligned_offset..aligned_offset + 8]);
                    let code_addr = i64::from_le_bytes(code_addr_bytes);

                    // Pop the c-addr
                    self.data_stack.pop()?;

                    // Push xt (code address)
                    self.data_stack.push(code_addr);

                    // Push flag: -1 if immediate, 1 if not
                    let flag = if (flags & F_IMMEDIATE) != 0 { -1 } else { 1 };
                    self.data_stack.push(flag);

                    return Ok(());
                }
            }

            // Move to next word in chain
            current = link;
        }

        // Not found - leave c-addr on stack and push 0
        self.data_stack.push(0);
        Ok(())
    }

    fn op_execute(&mut self) -> Result<(), ForthError> {
        // EXECUTE ( xt -- )
        // Execute the word at execution token xt
        // Sets pending_execute which the interpreter will pick up

        let xt = self.data_stack.pop()? as usize;
        self.pending_execute = Some(xt);

        Ok(())
    }

    fn op_compile_primitive(&mut self) -> Result<(), ForthError> {
        // COMPILE-PRIM ( prim-id -- )
        // Compile a primitive instruction: OP_PRIMITIVE (1 byte) + prim_id (1 byte)
        let prim_id = self.data_stack.pop()? as u8;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and prim_id
        if dp + 2 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_PRIMITIVE;
        self.memory[dp + 1] = prim_id;

        // Update DP
        let new_dp = (dp + 2) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_literal(&mut self) -> Result<(), ForthError> {
        // COMPILE-LIT ( n -- )
        // Compile a literal instruction: OP_LITERAL (1 byte) + value (8 bytes)
        let value = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and value
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_LITERAL;
        self.memory[dp + 1..dp + 9].copy_from_slice(&value.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_call(&mut self) -> Result<(), ForthError> {
        // COMPILE-CALL ( addr -- )
        // Compile a call instruction: OP_CALL (1 byte) + addr (8 bytes)
        let addr = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and address
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_CALL;
        self.memory[dp + 1..dp + 9].copy_from_slice(&addr.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_return(&mut self) -> Result<(), ForthError> {
        // COMPILE-RETURN ( -- )
        // Compile a return instruction: OP_RETURN (1 byte)

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode
        if dp + 1 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_RETURN;

        // Update DP
        let new_dp = (dp + 1) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_branch0(&mut self) -> Result<(), ForthError> {
        // COMPILE-BRANCH0 ( addr -- )
        // Compile a branch-if-zero instruction: OP_BRANCH0 (1 byte) + addr (8 bytes)
        let addr = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and address
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_BRANCH0;
        self.memory[dp + 1..dp + 9].copy_from_slice(&addr.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_branchnz(&mut self) -> Result<(), ForthError> {
        // COMPILE-BRANCHNZ ( addr -- )
        // Compile a branch-if-not-zero instruction: OP_BRANCHNZ (1 byte) + addr (8 bytes)
        let addr = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and address
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_BRANCHNZ;
        self.memory[dp + 1..dp + 9].copy_from_slice(&addr.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_jump(&mut self) -> Result<(), ForthError> {
        // COMPILE-JUMP ( addr -- )
        // Compile an unconditional jump instruction: OP_JUMP (1 byte) + addr (8 bytes)
        let addr = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and address
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_JUMP;
        self.memory[dp + 1..dp + 9].copy_from_slice(&addr.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_do_setup(&mut self) -> Result<(), ForthError> {
        // COMPILE-DO-SETUP ( -- )
        // Compile DO setup instruction: OP_DO_SETUP (1 byte)

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode
        if dp + 1 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_DO_SETUP;

        // Update DP
        let new_dp = (dp + 1) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_loop_check(&mut self) -> Result<(), ForthError> {
        // COMPILE-LOOP-CHECK ( addr -- )
        // Compile LOOP check instruction: OP_LOOP_CHECK (1 byte) + addr (8 bytes)
        let addr = self.data_stack.pop()?;

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode and address
        if dp + 9 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_LOOP_CHECK;
        self.memory[dp + 1..dp + 9].copy_from_slice(&addr.to_le_bytes());

        // Update DP
        let new_dp = (dp + 9) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }

    fn op_compile_loop_end(&mut self) -> Result<(), ForthError> {
        // COMPILE-LOOP-END ( -- )
        // Compile LOOP end instruction: OP_LOOP_END (1 byte)

        // Get current DP
        let mut dp_bytes = [0u8; 8];
        dp_bytes.copy_from_slice(&self.memory[DP_ADDR..DP_ADDR + 8]);
        let dp = i64::from_le_bytes(dp_bytes) as usize;

        // Write opcode
        if dp + 1 > self.memory.len() {
            return Err(ForthError::InvalidMemoryAddress);
        }
        self.memory[dp] = OP_LOOP_END;

        // Update DP
        let new_dp = (dp + 1) as i64;
        self.memory[DP_ADDR..DP_ADDR + 8].copy_from_slice(&new_dp.to_le_bytes());

        Ok(())
    }
}

