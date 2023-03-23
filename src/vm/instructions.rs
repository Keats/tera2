use crate::utils::Span;
use crate::value::Value;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

#[derive(Debug, Clone)]
pub enum Instruction {
    /// Pushing a value to the stack
    LoadConst(Value),
    /// Reading a variable/function
    LoadVar(String),
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

#[derive(Clone)]
pub struct Chunk {
    instructions: Vec<Instruction>,
    /// instruction idx -> Span
    spans: HashMap<u32, Span>,
    // What should it be there? Template name?
    name: String,
}

impl Chunk {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            instructions: Vec::with_capacity(256),
            spans: HashMap::with_capacity(256),
            name: name.to_owned(),
        }
    }

    pub(crate) fn add_instruction(&mut self, instr: Instruction) -> u32 {
        let idx = self.instructions.len();
        self.instructions.push(instr);
        idx as u32
    }

    pub(crate) fn add_instruction_with_span(&mut self, instr: Instruction, span: Span) {
        let idx = self.add_instruction(instr);
        self.spans.insert(idx, span);
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== {} ===", self.name)?;

        // let mut offset = 0;
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

    // #[test]
    // fn test_debug() {
    //     let mut chunk = Chunk::new("hello");
    //     chunk.add_instruction(Instruction::LoadConst(Value::U64(1)));
    //     chunk.add_instruction(Instruction::LoadVar("ho".to_string()));
    //     println!("{:?}", chunk);
    //     assert!(false);
    // }
}
