use crate::utils::Span;
use crate::value::Value;
use std::fmt;
use std::fmt::Formatter;
use std::ops::RangeInclusive;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Instruction {
    /// Pushing a value to the stack
    LoadConst(Value),
    /// Reading a variable/function
    LoadName(String),
    /// Get the named field of the top stack value (`person.name`)
    LoadAttr(String),
    /// Safely get the named field of the top stack value (`person.name`)
    LoadAttrOpt(String),
    /// Handles `a[b]`. `b` is the top stack value, `a` the one before
    BinarySubscript,
    /// Safely handles `a[b]`. `b` is the top stack value, `a` the one before
    BinarySubscriptOpt,
    /// Handles `a[1:2]`, `a[::-1]`, `a[:2]` etc
    Slice,
    /// Safely handles `a[1:2]`, `a[::-1]`, `a[:2]` etc
    SliceOpt,
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
    /// Render the given inline component
    RenderInlineComponent(String),
    /// Render the given component with body
    RenderBodyComponent(String),
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
    /// Jump if TOS is falsy or pop it. Used with and/or
    JumpIfFalseOrPop(usize),
    /// Jump if TOS is truthy or pop it. Used with and/or
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
    /// Skips the rest of the loop and goes straight to PopLoop
    /// TODO: Can we skip it?
    Break,
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

    // We create some optimized instructions to avoid moving things too much on the stack
    // in the VM

    // LoadName + LoadAttr* (single push for entire path)
    //path[0] is the variable name, path[1..] are attribute names
    LoadPath(Vec<String>),
    // LoadName + LoadAttr* + WriteTop
    WritePath(Vec<String>),
}

#[derive(Clone, PartialEq, Default)]
pub(crate) struct Chunk {
    /// Instructions with their associated spans.
    /// Most instructions have 0 or 1 span, but fused instructions (LoadPath, WritePath)
    /// have multiple spans - one per path element for accurate error reporting.
    instructions: Vec<(Instruction, Vec<Span>)>,
    /// The template name so we can point to the right place for error messages
    pub name: String,
}

impl Chunk {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            instructions: Vec::with_capacity(256),
            name: name.to_owned(),
        }
    }

    pub(crate) fn add(&mut self, instr: Instruction, span: Option<Span>) -> u32 {
        let idx = self.instructions.len();
        let spans = span.into_iter().collect();
        self.instructions.push((instr, spans));
        idx as u32
    }

    pub(crate) fn get(&self, idx: usize) -> Option<&(Instruction, Vec<Span>)> {
        self.instructions.get(idx)
    }

    pub(crate) fn get_mut(&mut self, idx: usize) -> Option<&mut (Instruction, Vec<Span>)> {
        self.instructions.get_mut(idx)
    }

    pub(crate) fn len(&self) -> usize {
        self.instructions.len()
    }

    pub(crate) fn is_calling_function(&self, fn_name: &str) -> bool {
        self.instructions.iter().any(|(i, _)| match i {
            Instruction::CallFunction(s) => s == fn_name,
            _ => false,
        })
    }

    pub(crate) fn get_span(&self, idx: u32) -> Option<&Span> {
        self.instructions
            .get(idx as usize)
            .and_then(|(_, spans)| spans.first())
    }

    /// Get a specific span from an instruction's span list.
    /// Used by fused instructions (LoadPath, WritePath) where each path element has its own span.
    pub(crate) fn get_span_at(&self, idx: u32, span_idx: usize) -> Option<&Span> {
        self.instructions
            .get(idx as usize)
            .and_then(|(_, spans)| spans.get(span_idx))
    }

    /// Expand a range of span indices into a single Span.
    /// Takes the start position from the first span and end position from the last span.
    pub(crate) fn expand_span(&self, range: &RangeInclusive<u32>) -> Option<Span> {
        let start = *range.start();
        let end = *range.end();
        let start_span = self.get_span(start)?;

        // Fast path: single instruction, no expansion needed
        if start == end {
            return Some(start_span.clone());
        }

        let end_span = self.get_span(end)?;
        let mut expanded = start_span.clone();
        expanded.expand(end_span);
        Some(expanded)
    }

    /// Optimize bytecode by combining common instruction patterns to avoid pushing/popping
    /// so much on the stack in the VM when we can
    pub(crate) fn optimize(&mut self) {
        let mut optimized = Vec::with_capacity(self.instructions.len());
        // Map from old instruction index to new instruction index
        // +1 to handle jumps that target one-past-the-end (i.e., chunk.len())
        let mut index_map: Vec<usize> = vec![0; self.instructions.len() + 1];
        let mut i = 0;

        while i < self.instructions.len() {
            // Record the mapping for this instruction
            index_map[i] = optimized.len();

            // Try to collect a path: LoadName followed by any number of LoadAttr
            if let (Instruction::LoadName(name), spans) = &self.instructions[i] {
                let mut path = vec![name.clone()];
                // Collect spans from all combined instructions for accurate error reporting
                let mut collected_spans: Vec<Span> = spans.clone();
                let mut j = i + 1;

                // Collect consecutive LoadAttr instructions
                while j < self.instructions.len() {
                    if let (Instruction::LoadAttr(attr), attr_spans) = &self.instructions[j] {
                        // Map the consumed LoadAttr to the same position as the first instruction
                        index_map[j] = optimized.len();
                        path.push(attr.clone());
                        collected_spans.extend_from_slice(attr_spans);
                        j += 1;
                    } else {
                        break;
                    }
                }

                // Check if followed by WriteTop
                let has_write = j < self.instructions.len()
                    && matches!(&self.instructions[j].0, Instruction::WriteTop);

                if has_write {
                    // Map the consumed WriteTop
                    index_map[j] = optimized.len();
                    // Fuse entire path + WriteTop into WritePath
                    // Don't include WriteTop's span - we only need spans for each path element
                    optimized.push((Instruction::WritePath(path), collected_spans));
                    i = j + 1; // Skip past WriteTop
                    continue;
                } else if path.len() > 1 {
                    // Fuse LoadName + LoadAttr* into LoadPath (no WriteTop)
                    optimized.push((Instruction::LoadPath(path), collected_spans));
                    i = j;
                    continue;
                }
                // Single LoadName with no attrs AND no WriteTop - keep original LoadName
                // and fall through to add it
            }

            // No pattern matched, keep original
            optimized.push(self.instructions[i].clone());
            i += 1;
        }

        // Map the one-past-the-end index (for jumps that target chunk.len())
        index_map[self.instructions.len()] = optimized.len();

        // Now fix up all jump targets
        for (instr, _) in &mut optimized {
            match instr {
                Instruction::Jump(target)
                | Instruction::PopJumpIfFalse(target)
                | Instruction::JumpIfFalseOrPop(target)
                | Instruction::JumpIfTrueOrPop(target)
                | Instruction::Iterate(target) => {
                    *target = index_map[*target];
                }
                _ => {}
            }
        }

        self.instructions = optimized;
    }
}

impl fmt::Debug for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== {} ===", self.name)?;

        for (offset, (instr, _)) in self.instructions.iter().enumerate() {
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
