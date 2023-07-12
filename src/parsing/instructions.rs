use crate::utils::Span;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Instruction {
    /// Pushing a value to the stack
    LoadConst(Value),
    /// Reading a variable/function
    LoadName(String),
    /// Get the named field of the top stack value (`person.name`)
    LoadAttr(String),
    /// Handles `a[b]`. `b` is the top stack value, `a` the one before
    BinarySubscript,
    /// Write the raw string given
    WriteText(String),
    /// Writes the value on the top of the stack
    WriteTop,
    /// Set the last value on the stack in the current context
    Set(String),
    /// Set the last value on the stack in the global context. Same as Set outside of loops.
    SetGlobal(String),
    /// Include the given template
    Include(String),

    /// Create a map for the kwargs of a function or for inline maps.
    /// Inner field is the number of values
    BuildMap(usize),
    /// Create a list. Inner field is the number of values
    BuildList(usize),
    /// Call the named Tera function
    CallFunction(String),
    /// Render the macro at the given index in the macro_calls vec
    RenderMacro(usize),
    /// Apply the given filter
    ApplyFilter(String),
    /// Run the given test
    RunTest(String),
    /// Render the given block
    RenderBlock(String),

    /// Jump to the instruction at the given idx
    Jump(usize),
    /// Jump to the instruction at the given idx and pops the top value of the stack if the value is falsy
    PopJumpIfFalse(usize),
    /// Jump is TOS is falsy or pop it. Used with and/or
    JumpIfFalseOrPop(usize),
    /// Jump is TOS is truthy or pop it. Used with and/or
    JumpIfTrueOrPop(usize),

    /// Start capturing the output in another buffer than the template output
    /// Used for filter section
    Capture,
    /// We are done capturing
    EndCapture,

    /// Start an iteration. `true` if it's iterating on (key, value)
    StartIterate(bool),
    /// Start to iterate on the value at the top of the stack. The integer is the ip to jump to
    /// when the for loop is over
    Iterate(usize),
    /// Store a value for key/value in a for loop
    StoreLocal(String),
    /// Store whether the loop did not iterate, used in for / else situations
    StoreDidNotIterate,
    /// At the end of a loop we want to remove it
    PopLoop,

    // math
    Mul,
    Div,
    FloorDiv,
    Mod,
    Plus,
    Minus,
    Power,

    // logic
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Equal,
    NotEqual,

    StrConcat,
    In,

    // unary
    Not,
    Negative,
}

#[derive(Clone, PartialEq, Default)]
pub struct Chunk {
    instructions: Vec<Instruction>,
    /// instruction idx -> Span
    spans: HashMap<u32, Span>,
    /// The template name so we can point to the right place for error messages
    pub name: String,
}

impl Chunk {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            instructions: Vec::with_capacity(256),
            spans: HashMap::with_capacity(256),
            name: name.to_owned(),
        }
    }

    pub(crate) fn add(&mut self, instr: Instruction) -> u32 {
        let idx = self.instructions.len();
        self.instructions.push(instr);
        idx as u32
    }

    pub(crate) fn add_instruction_with_span(&mut self, instr: Instruction, span: Span) {
        let idx = self.add(instr);
        self.spans.insert(idx, span);
    }

    pub(crate) fn get(&self, idx: usize) -> Option<&Instruction> {
        self.instructions.get(idx)
    }

    pub(crate) fn get_mut(&mut self, idx: usize) -> Option<&mut Instruction> {
        self.instructions.get_mut(idx)
    }

    pub(crate) fn len(&self) -> usize {
        self.instructions.len()
    }

    pub(crate) fn is_calling_function(&self, fn_name: &str) -> bool {
        self.instructions.iter().any(|i| match i {
            Instruction::CallFunction(s) => s == fn_name,
            _ => false,
        })
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== {} ===", self.name)?;

        for (offset, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{offset:>04} {instr:?}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_size() {
        assert_eq!(std::mem::size_of::<Instruction>(), 32);
    }
}
