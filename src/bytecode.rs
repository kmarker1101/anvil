// bytecode.rs - Forth bytecode instruction set

use crate::primitives::Primitive;

/// Bytecode instruction for the Forth interpreter
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Execute a primitive operation
    Primitive(Primitive),

    /// Push a literal value onto the data stack
    PushLiteral(i64),

    /// Unconditional jump to address
    Jump(usize),

    /// Jump if top of stack is zero (for IF...THEN)
    JumpIfZero(usize),

    /// Jump if top of stack is not zero (for WHILE)
    JumpIfNotZero(usize),

    /// Call a user-defined word at bytecode address
    Call(usize),

    /// Return from current word
    Return,

    /// Push a variable's memory offset onto the stack
    PushVariable(usize),

    /// Push a constant value onto the stack
    PushConstant(i64),

    /// Setup DO loop: pops limit and start from data stack, pushes to loop stack
    DoSetup,

    /// Setup ?DO loop: pops limit and start, jumps to end if equal, else pushes to loop stack
    QuestionDoSetup(usize),

    /// Check loop condition and jump to address if done, otherwise increment
    LoopCheck(usize),

    /// Check +LOOP condition with increment on TOS, jump if done
    PlusLoopCheck(usize),

    /// Clean up loop stack at loop end
    LoopEnd,
}

/// A compiled word consists of bytecode instructions
pub type Bytecode = Vec<Instruction>;

/// Address of a bytecode instruction
pub type BytecodeAddress = usize;

/// Placeholder address for forward references (to be backpatched)
pub const PLACEHOLDER_ADDR: usize = usize::MAX;
