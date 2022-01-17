// use crate::parser::ast::{
//     Block, Expression, FilterSection, ForLoop, FunctionCall, If, Include, MacroCall,
//     MacroDefinition, Node, Set, SpannedExpression, Test,
// };
// use crate::parser::errors::{ParsingError, ParsingResult, SpannedParsingError};
// use crate::parser::lexer::{Keyword, Operator, PeekableLexer, Symbol, Token};
// use std::collections::HashMap;
// use std::ops::Range;

// pub mod ast;
// mod errors;
// mod lexer;

mod parser;

mod ast2;
mod lexer2;

#[cfg(test)]
mod tests;
//
// // From https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
//
// fn prefix_binding_power(op: Operator) -> ParsingResult<((), u8)> {
//     use Operator::*;
//
//     match op {
//         Not => Ok(((), 3)),
//         Add | Sub => Ok(((), 7)),
//         _ => Err(SpannedParsingError::new(
//             ParsingError::UnexpectedOperator(op, vec![Not, Add, Sub]),
//             0..0,
//         )),
//     }
// }
//
// // Some of those could be postfix but if it works like that...
// fn infix_binding_power(op: Operator) -> (u8, u8) {
//     use Operator::*;
//
//     match op {
//         And | Or => (1, 2),
//         In | Is => (3, 4),
//         Pipe => (5, 6),
//         Equal | NotEqual | LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual => (7, 8),
//         Add | Sub => (11, 12),
//         Mul | Div | Mod | StrConcat => (13, 14),
//         _ => unreachable!("bad op: {:?}", op),
//     }
// }
//
// /// Strings are delimited by double quotes, single quotes and backticks
// /// We need to remove those before putting them in the AST
// fn replace_string_markers(input: &str) -> String {
//     match input.chars().next().unwrap() {
//         '"' => input.replace('"', ""),
//         '\'' => input.replace('\'', ""),
//         '`' => input.replace('`', ""),
//         _ => unreachable!("How did you even get there"),
//     }
// }
//
// #[derive(Clone, Debug, PartialEq)]
// enum ParsingContext {
//     // Those ones happen in an expression only
//     Paren,
//     Array,
//     TestArgs,
//     Kwargs,
//     Set,
//     // Those ones exist to provide better error messages
//     ForLoop,
//     Block,
//     MacroDefinition,
//     If,
//     FilterSection,
// }
//
// fn eof_error(last_idx: usize) -> SpannedParsingError {
//     SpannedParsingError::new(ParsingError::UnexpectedEof, last_idx..last_idx)
// }
//
// pub struct Parser<'a> {
//     source: &'a str,
//     lexer: PeekableLexer<'a>,
//     pub nodes: Vec<Node>,
//     contexts: Vec<ParsingContext>,
//     // filled when we encounter a {% extends %}, we don't need to keep the extends node in the AST
//     pub parent: Option<String>,
//     // if we have a parent template, we only care about the blocks, whatever is in between is
//     // disregarded and we will only look at the fields below
//     pub blocks: HashMap<String, Vec<Node>>,
//     pub macros: HashMap<String, MacroDefinition>,
//     // (file, namespace)
//     pub macro_imports: Vec<(String, String)>,
//     // The size in bytes of the text, not including any tags
//     pub size_hint: usize,
//     // WS management
//     // The equivalent of `trim_blocks` and `lstrip_blocks` set to True in Jinja2
//     smart_whitespace: bool,
//     trim_start_next: bool,
//     trim_end_previous: bool,
// }
//
// impl<'a> Parser<'a> {
//     pub fn new(source: &'a str) -> Self {
//         let lexer = PeekableLexer::new(source);
//
//         Self {
//             source,
//             lexer,
//             nodes: Vec::new(),
//             contexts: Vec::new(),
//             trim_start_next: false,
//             trim_end_previous: false,
//             parent: None,
//             blocks: HashMap::new(),
//             macros: HashMap::new(),
//             macro_imports: Vec::new(),
//             size_hint: 0,
//             smart_whitespace: false,
//         }
//     }
//
//     pub fn enable_smart_whitespace(&mut self) {
//         self.smart_whitespace = true;
//     }
//
//     fn parse_until<F: Fn(&Token) -> bool>(&mut self, end_check_fn: F) -> ParsingResult<Vec<Node>> {
//         let mut nodes = Vec::new();
//
//         while let Some(token) = self.lexer.next() {
//             match token {
//                 Token::VariableStart(ws) => {
//                     self.trim_end_previous = ws;
//                     if let Some(n) = self.parse_text() {
//                         nodes.push(n);
//                     }
//
//                     // It can either be an expression or super()
//                     if let Token::Keyword(Keyword::Super) = self.peek_or_error()? {
//                         let mut in_block = false;
//                         for ctx in &self.contexts {
//                             if ctx == &ParsingContext::Block {
//                                 in_block = true;
//                                 break;
//                             }
//                         }
//
//                         if in_block {
//                             nodes.push(Node::Super);
//                         } else {
//                             // TODO: explain super() can only be used in blocks
//                             return Err(SpannedParsingError::new(
//                                 ParsingError::UnexpectedToken(
//                                     Token::Keyword(Keyword::Super),
//                                     vec![],
//                                 ),
//                                 self.lexer.span(),
//                             ));
//                         }
//                         self.next_or_error()?;
//                     } else {
//                         let expr = self.parse_expression(0)?;
//                         nodes.push(Node::VariableBlock(expr));
//                     }
//
//                     match self
//                         .expect_one_of(vec![Token::VariableEnd(true), Token::VariableEnd(false)])?
//                     {
//                         Token::VariableEnd(b) => self.trim_start_next = b,
//                         _ => unreachable!(),
//                     }
//                 }
//                 Token::TagStart(ws) => {
//                     self.trim_end_previous = ws;
//                     if let Some(n) = self.parse_text() {
//                         nodes.push(n);
//                     }
//                     let next_token = self.peek_or_error()?;
//                     if end_check_fn(&next_token) {
//                         return Ok(nodes);
//                     }
//                     if let Some(node) = self.parse_tag()? {
//                         nodes.push(node);
//                     }
//                 }
//                 Token::Comment => {
//                     let comment = self.lexer.slice().to_owned();
//                     self.trim_end_previous = comment.starts_with("{#-");
//                     if let Some(n) = self.parse_text() {
//                         nodes.push(n);
//                     }
//                     self.trim_start_next = comment.ends_with("-#}");
//                 }
//                 Token::Error => {
//                     return Err(SpannedParsingError::new(
//                         ParsingError::UnexpectedToken(Token::Error, vec![]),
//                         self.lexer.span(),
//                     ));
//                 }
//                 t => {
//                     unreachable!("Not implemented yet {:?}; {:?}", t, self.lexer.span())
//                 }
//             }
//         }
//
//         if let Some(n) = self.parse_text() {
//             nodes.push(n);
//         }
//
//         Ok(nodes)
//     }
//
//     pub fn parse(&mut self) -> ParsingResult<()> {
//         // TODO: clean up parse_ident somehow
//         // TODO: revamp errors + ensure super etc are only used in the correct place
//         // TODO: revamp all expect fns
//
//         self.nodes = self.parse_until(|_| false)?;
//         Ok(())
//     }
//
//     /// Appends the text up until the new tag/expr with whitespace management taken into account
//     fn parse_text(&mut self) -> Option<Node> {
//         let mut previous = self.lexer.slice_before();
//         if self.trim_end_previous {
//             previous = previous.trim_end();
//         }
//         if self.trim_start_next {
//             previous = previous.trim_start();
//         }
//         self.trim_start_next = false;
//         self.trim_end_previous = false;
//
//         if !previous.is_empty() {
//             self.size_hint += previous.len();
//             let node = Node::Text(previous.to_string());
//             Some(node)
//         } else {
//             None
//         }
//     }
//
//     fn parse_raw(&mut self) -> ParsingResult<Node> {
//         self.expect_tag_end()?;
//
//         let start = self.lexer.span().end;
//         let mut end;
//         let trim_end_previous;
//         // We go through the lexer until we find an {% endraw %}
//         loop {
//             if let Token::TagStart(ws) = self.next_or_error()? {
//                 end = self.lexer.span().start;
//                 if let Token::Keyword(Keyword::EndRaw) = self.peek_or_error()? {
//                     trim_end_previous = ws;
//                     self.lexer.next();
//                     break;
//                 }
//             }
//         }
//         let mut slice = self.lexer.slice_at(start..end);
//         if self.trim_start_next {
//             slice = slice.trim_start();
//         }
//         if trim_end_previous {
//             slice = slice.trim_end();
//         }
//         let body = slice.to_owned();
//         self.expect_tag_end()?;
//
//         Ok(Node::Raw(body))
//     }
//
//     /// This doesn't return a Node as we just keep track of the string value on the parser directly.
//     /// We don't need it in the AST
//     fn parse_extends(&mut self) -> ParsingResult<()> {
//         if !self.contexts.is_empty() {
//             return Err(SpannedParsingError::new(
//                 ParsingError::TagNotAllowed("extends".to_string()),
//                 self.lexer.span(),
//             ));
//         }
//
//         if let Some(ref existing) = self.parent {
//             return Err(SpannedParsingError::new(
//                 ParsingError::DuplicateExtend(existing.clone()),
//                 self.lexer.span(),
//             ));
//         }
//         self.lexer.expect(Token::String)?;
//         let val = replace_string_markers(self.lexer.slice());
//         self.parent = Some(val);
//         self.expect_tag_end()?;
//
//         Ok(())
//     }
//
//     fn parse_include(&mut self) -> ParsingResult<Node> {
//         // Files can be a single string or an array of strings
//         let files =
//             match self.expect_one_of(vec![Token::String, Token::Symbol(Symbol::LeftBracket)])? {
//                 Token::String => {
//                     let val = replace_string_markers(self.lexer.slice());
//                     vec![val]
//                 }
//                 Token::Symbol(Symbol::LeftBracket) => {
//                     let start = self.lexer.span().start;
//                     let vals = self.parse_array()?;
//                     let end = self.lexer.span().end;
//                     let mut files = Vec::with_capacity(vals.len());
//
//                     for v in vals {
//                         match v.node {
//                             Expression::Str(s) => files.push(s),
//                             _ => {
//                                 return Err(SpannedParsingError::new(
//                                     ParsingError::InvalidInclude,
//                                     start..end,
//                                 ));
//                             }
//                         }
//                     }
//                     files
//                 }
//                 _ => unreachable!(),
//             };
//
//         let ignore_missing = match self.peek_or_error()? {
//             Token::Keyword(Keyword::IgnoreMissing) => {
//                 self.lexer.next();
//                 true
//             }
//             Token::TagEnd(_) => false,
//             t => {
//                 return Err(SpannedParsingError::new(
//                     ParsingError::UnexpectedToken(
//                         t,
//                         vec![
//                             Token::Keyword(Keyword::IgnoreMissing),
//                             Token::TagEnd(true),
//                             Token::TagEnd(false),
//                         ],
//                     ),
//                     self.lexer.span(),
//                 ))
//             }
//         };
//
//         self.expect_tag_end()?;
//         Ok(Node::Include(Include {
//             files,
//             ignore_missing,
//         }))
//     }
//
//     fn parse_set(&mut self, global: bool) -> ParsingResult<Node> {
//         self.lexer.expect(Token::Ident)?;
//         let key = self.lexer.slice().to_owned();
//         self.lexer.expect(Token::Symbol(Symbol::Assign))?;
//         let value = self.parse_expression(0)?;
//         self.expect_tag_end()?;
//         Ok(Node::Set(Set { key, value, global }))
//     }
//
//     fn parse_import(&mut self) -> ParsingResult<()> {
//         self.lexer.expect(Token::String)?;
//         let filename = replace_string_markers(self.lexer.slice());
//         self.lexer.expect(Token::Keyword(Keyword::As))?;
//         self.lexer.expect(Token::Ident)?;
//         let namespace = self.lexer.slice().to_owned();
//         for (existing_filename, existing_namespace) in &self.macro_imports {
//             if existing_namespace == &namespace {
//                 return Err(SpannedParsingError::new(
//                     ParsingError::ConflictingMacroImport(format!(
//                         "namespace {} is already imported for the file '{}'",
//                         namespace, existing_filename
//                     )),
//                     self.lexer.span(),
//                 ));
//             }
//         }
//         self.expect_tag_end()?;
//         self.macro_imports.push((filename, namespace));
//
//         Ok(())
//     }
//
//     fn parse_for_loop(&mut self) -> ParsingResult<Node> {
//         self.lexer.expect(Token::Ident)?;
//         let var1 = self.lexer.slice().to_owned();
//         let mut var2 = String::new();
//         if self.peek_or_error()? == Token::Symbol(Symbol::Comma) {
//             self.lexer.next();
//             self.lexer.expect(Token::Ident)?;
//             var2 = self.lexer.slice().to_owned();
//         }
//         self.lexer.expect(Token::Op(Operator::In))?;
//         let start = self.lexer.span().end;
//         let container = self.parse_expression(0)?;
//         let end = self.lexer.span().start;
//         if !container.can_be_iterated_on() {
//             return Err(SpannedParsingError::new(
//                 ParsingError::CannotIterateOn,
//                 start..end,
//             ));
//         }
//         let key = if var2.is_empty() {
//             None
//         } else {
//             Some(var1.clone())
//         };
//         let value = if key.is_none() { var1 } else { var2 };
//         self.expect_tag_end()?;
//         self.contexts.push(ParsingContext::ForLoop);
//         let body = self.parse_until(|tok| {
//             matches!(
//                 tok,
//                 Token::Keyword(Keyword::EndFor) | Token::Keyword(Keyword::Else)
//             )
//         })?;
//         let current_tok = self.lexer.current();
//         self.next_or_error()?;
//         let otherwise = if current_tok == Token::Keyword(Keyword::Else) {
//             self.expect_tag_end()?;
//             let toks = self.parse_until(|tok| *tok == Token::Keyword(Keyword::EndFor))?;
//             self.lexer.expect(Token::Keyword(Keyword::EndFor))?;
//             toks
//         } else {
//             Vec::new()
//         };
//
//         self.expect_tag_end()?;
//         self.contexts.pop();
//
//         Ok(Node::ForLoop(ForLoop {
//             key,
//             value,
//             container,
//             body,
//             otherwise,
//         }))
//     }
//
//     fn parse_filter_section(&mut self) -> ParsingResult<Node> {
//         self.contexts.push(ParsingContext::FilterSection);
//         self.lexer.expect(Token::Ident)?;
//         let name = self.lexer.slice().to_owned();
//         let kwargs = match self.peek_or_error()? {
//             Token::Symbol(Symbol::LeftParen) => self.parse_kwargs()?,
//             _ => HashMap::new(),
//         };
//         self.expect_tag_end()?;
//         let body = self.parse_until(|tok| *tok == Token::Keyword(Keyword::EndFilter))?;
//         self.lexer.expect(Token::Keyword(Keyword::EndFilter))?;
//         self.expect_tag_end()?;
//         self.contexts.pop();
//
//         Ok(Node::FilterSection(FilterSection { name, kwargs, body }))
//     }
//
//     fn parse_if(&mut self) -> ParsingResult<Node> {
//         let mut conditions = Vec::new();
//         let mut otherwise = Vec::new();
//         let mut has_seen_else = false;
//         self.contexts.push(ParsingContext::If);
//
//         loop {
//             let condition = if has_seen_else {
//                 None
//             } else {
//                 Some(self.parse_expression(0)?)
//             };
//             self.expect_tag_end()?;
//
//             let body = self.parse_until(|tok| {
//                 if has_seen_else {
//                     *tok == Token::Keyword(Keyword::EndIf)
//                 } else {
//                     matches!(
//                         tok,
//                         Token::Keyword(Keyword::Elif)
//                             | Token::Keyword(Keyword::Else)
//                             | Token::Keyword(Keyword::EndIf)
//                     )
//                 }
//             })?;
//
//             match self.lexer.current() {
//                 Token::Keyword(Keyword::Elif) => {
//                     conditions.push((condition.unwrap(), body));
//                     self.lexer.expect(Token::Keyword(Keyword::Elif))?;
//                 }
//                 Token::Keyword(Keyword::Else) => {
//                     conditions.push((condition.unwrap(), body));
//                     has_seen_else = true;
//                     self.lexer.expect(Token::Keyword(Keyword::Else))?;
//                 }
//                 Token::Keyword(Keyword::EndIf) => {
//                     if has_seen_else {
//                         otherwise = body;
//                     } else {
//                         conditions.push((condition.unwrap(), body));
//                     }
//                     self.lexer.expect(Token::Keyword(Keyword::EndIf))?;
//                     self.expect_tag_end()?;
//                     break;
//                 }
//                 _ => {
//                     return Err(eof_error(self.lexer.last_idx()));
//                 }
//             }
//         }
//
//         self.contexts.pop();
//
//         Ok(Node::If(If {
//             conditions,
//             otherwise,
//         }))
//     }
//
//     fn parse_block(&mut self) -> ParsingResult<Option<Node>> {
//         self.contexts.push(ParsingContext::Block);
//         self.lexer.expect(Token::Ident)?;
//         let name = self.lexer.slice().to_owned();
//         self.expect_tag_end()?;
//         let body = self.parse_until(|tok| *tok == Token::Keyword(Keyword::EndBlock))?;
//         self.lexer.expect(Token::Keyword(Keyword::EndBlock))?;
//         let mut closing_name = String::new();
//         if let Token::Ident = self.peek_or_error()? {
//             self.lexer.next();
//             closing_name = self.lexer.slice().to_owned();
//         }
//         if !closing_name.is_empty() && name != closing_name {
//             return Err(SpannedParsingError::new(
//                 ParsingError::MismatchedBlock(name),
//                 self.lexer.span(),
//             ));
//         }
//         self.expect_tag_end()?;
//         self.contexts.pop();
//
//         self.blocks.insert(name.clone(), body.clone());
//         // Child templates do not get AST really
//         if self.parent.is_some() {
//             Ok(None)
//         } else {
//             Ok(Some(Node::Block(Block { name, body })))
//         }
//     }
//
//     fn parse_macro(&mut self) -> ParsingResult<()> {
//         self.lexer.expect(Token::Ident)?;
//         let name = self.lexer.slice().to_owned();
//         self.lexer.expect(Token::Symbol(Symbol::LeftParen))?;
//         let mut kwargs = HashMap::new();
//
//         // Not using `parse_kwargs` since it only allows literals as arguments
//         loop {
//             if let Token::Symbol(Symbol::RightParen) = self.peek_or_error()? {
//                 break;
//             }
//
//             self.lexer.expect(Token::Ident)?;
//             let arg_name = self.lexer.slice().to_owned();
//             kwargs.insert(arg_name.clone(), None);
//
//             match self.peek_or_error()? {
//                 Token::Symbol(Symbol::Assign) => {
//                     self.lexer.next();
//                     let val = match self.next_or_error()? {
//                         Token::Bool(b) => Expression::Bool(b),
//                         Token::String => {
//                             Expression::Str(replace_string_markers(self.lexer.slice()))
//                         }
//                         Token::Integer(i) => Expression::Integer(i),
//                         Token::Float(f) => Expression::Float(f),
//                         t => {
//                             return Err(SpannedParsingError::new(
//                                 ParsingError::UnexpectedToken(
//                                     t,
//                                     vec![
//                                         Token::Bool(true),
//                                         Token::String,
//                                         Token::Integer(0),
//                                         Token::Float(0.0),
//                                     ],
//                                 ),
//                                 self.lexer.span(),
//                             ));
//                         }
//                     };
//                     kwargs.insert(
//                         arg_name,
//                         Some(SpannedExpression::new(val, self.lexer.span())),
//                     );
//                     if let Token::Symbol(Symbol::Comma) = self.peek_or_error()? {
//                         self.lexer.next();
//                         continue;
//                     }
//                 }
//                 Token::Symbol(Symbol::Comma) => {
//                     self.lexer.next();
//                     continue;
//                 }
//                 _ => continue,
//             }
//         }
//
//         self.lexer.expect(Token::Symbol(Symbol::RightParen))?;
//         self.expect_tag_end()?;
//
//         let body = self.parse_until(|tok| *tok == Token::Keyword(Keyword::EndMacro))?;
//         self.lexer.expect(Token::Keyword(Keyword::EndMacro))?;
//
//         let mut closing_name = String::new();
//         if let Token::Ident = self.peek_or_error()? {
//             self.lexer.next();
//             closing_name = self.lexer.slice().to_owned();
//         }
//         if !closing_name.is_empty() && name != closing_name {
//             return Err(SpannedParsingError::new(
//                 ParsingError::MismatchedBlock(name),
//                 self.lexer.span(),
//             ));
//         }
//         self.expect_tag_end()?;
//         self.macros
//             .insert(name.clone(), MacroDefinition { name, kwargs, body });
//
//         Ok(())
//     }
//
//     fn parse_tag(&mut self) -> ParsingResult<Option<Node>> {
//         match self.next_or_error()? {
//             Token::Keyword(k) => match k {
//                 Keyword::Set => Ok(Some(self.parse_set(false)?)),
//                 Keyword::SetGlobal => Ok(Some(self.parse_set(true)?)),
//                 Keyword::For => Ok(Some(self.parse_for_loop()?)),
//                 Keyword::If => Ok(Some(self.parse_if()?)),
//                 Keyword::Extends => {
//                     self.parse_extends()?;
//                     Ok(None)
//                 }
//                 Keyword::Macro => {
//                     self.parse_macro()?;
//                     Ok(None)
//                 }
//                 Keyword::Block => Ok(self.parse_block()?),
//                 Keyword::Include => Ok(Some(self.parse_include()?)),
//                 Keyword::Import => {
//                     self.parse_import()?;
//                     Ok(None)
//                 }
//                 Keyword::Filter => Ok(Some(self.parse_filter_section()?)),
//                 Keyword::Raw => Ok(Some(self.parse_raw()?)),
//                 _ => Err(SpannedParsingError::new(
//                     ParsingError::UnexpectedToken(Token::Keyword(k), vec![]),
//                     self.lexer.span(),
//                 )),
//             },
//             t => Err(SpannedParsingError::new(
//                 ParsingError::UnexpectedToken(t, vec![]),
//                 self.lexer.span(),
//             )),
//         }
//     }
//
//     fn parse_kwargs(&mut self) -> ParsingResult<HashMap<String, SpannedExpression>> {
//         let mut kwargs = HashMap::new();
//         self.contexts.push(ParsingContext::Kwargs);
//
//         self.lexer.expect(Token::Symbol(Symbol::LeftParen))?;
//
//         loop {
//             let name = match self.next_or_error()? {
//                 Token::Ident => self.lexer.slice().to_owned(),
//                 Token::Symbol(Symbol::RightParen) => break,
//                 t => {
//                     return Err(SpannedParsingError::new(
//                         ParsingError::UnexpectedToken(
//                             t,
//                             vec![Token::Ident, Token::Symbol(Symbol::RightParen)],
//                         ),
//                         self.lexer.span(),
//                     ));
//                 }
//             };
//
//             self.lexer.expect(Token::Symbol(Symbol::Assign))?;
//             let value = self.parse_expression(0)?;
//             kwargs.insert(name, value);
//
//             match self.next_or_error()? {
//                 Token::Symbol(Symbol::RightParen) => break,
//                 _ => continue,
//             }
//         }
//
//         self.contexts.pop();
//         Ok(kwargs)
//     }
//
//     /// An ident can have multiple forms:
//     /// `hey`
//     /// `hey.ho`
//     /// `hey['ho']`
//     /// `hey[ho]`
//     /// `hey[ho].name`
//     /// `hey[1]`
//     fn parse_ident(&mut self) -> ParsingResult<(String, Range<usize>)> {
//         // We are already at the ident token when we start
//         let start = self.lexer.span().start;
//         let mut end;
//         let mut base_ident = self.lexer.slice().to_owned();
//         let mut after_dot = false;
//         let mut in_brackets = Vec::new();
//
//         loop {
//             end = self.lexer.span().end;
//             let token = self.peek_or_error()?;
//
//             // After a dot, only an ident or an integer is allowed
//             if after_dot {
//                 after_dot = false;
//                 self.lexer.next();
//
//                 match token {
//                     Token::Ident => {
//                         base_ident.push_str(self.lexer.slice());
//                     }
//                     Token::Integer(i) => {
//                         base_ident.push_str(&i.to_string());
//                     }
//                     t => {
//                         return Err(SpannedParsingError::new(
//                             ParsingError::UnexpectedToken(t, vec![Token::Ident, Token::Integer(0)]),
//                             self.lexer.span(),
//                         ));
//                     }
//                 }
//                 continue;
//             }
//
//             // After a left brackets: ident or integer or string
//             if !in_brackets.is_empty() {
//                 self.lexer.next();
//
//                 match token {
//                     Token::Ident => {
//                         base_ident.push_str(self.lexer.slice());
//                     }
//                     Token::Integer(i) => {
//                         base_ident.push_str(&i.to_string());
//                     }
//                     Token::String => {
//                         base_ident.push_str(self.lexer.slice());
//                     }
//                     Token::Symbol(Symbol::LeftBracket) => {
//                         in_brackets.push(true);
//                         base_ident.push_str(self.lexer.slice());
//                     }
//                     _ => {
//                         // Need to disallow a[], base_ident is never an empty string
//                         if token == Token::Symbol(Symbol::RightBracket)
//                             && !base_ident.ends_with('[')
//                         {
//                             in_brackets.pop();
//                             base_ident.push_str(self.lexer.slice());
//                             continue;
//                         }
//
//                         return Err(SpannedParsingError::new(
//                             ParsingError::UnexpectedToken(
//                                 token,
//                                 vec![
//                                     Token::Ident,
//                                     Token::Integer(0),
//                                     Token::String,
//                                     Token::Symbol(Symbol::RightBracket),
//                                 ],
//                             ),
//                             self.lexer.span(),
//                         ));
//                     }
//                 }
//                 continue;
//             }
//
//             // After an ident, only dot, left bracket
//             // In array it can be followed by a `,` and in functions by `=` or `,`
//             let mut allow_comma = false;
//             let mut allow_assign = false;
//             let mut allow_right_parent = false;
//             if let Some(c) = self.contexts.last() {
//                 allow_comma = *c == ParsingContext::Array
//                     || *c == ParsingContext::TestArgs
//                     || *c == ParsingContext::Kwargs;
//                 allow_assign = *c == ParsingContext::Kwargs || *c == ParsingContext::Set;
//                 allow_right_parent = *c == ParsingContext::Paren;
//             }
//
//             match token {
//                 Token::Symbol(Symbol::Dot) => {
//                     after_dot = true;
//                     self.lexer.next();
//                     base_ident.push_str(self.lexer.slice());
//                 }
//                 Token::Symbol(Symbol::LeftBracket) => {
//                     in_brackets.push(true);
//                     self.lexer.next();
//                     base_ident.push_str(self.lexer.slice());
//                 }
//                 Token::Op(_)
//                 | Token::VariableEnd(_)
//                 | Token::TagEnd(_)
//                 | Token::Symbol(Symbol::LeftParen)
//                 | Token::Symbol(Symbol::DoubleColumn) => break,
//                 _ => {
//                     if token == Token::Symbol(Symbol::Comma) && allow_comma {
//                         break;
//                     }
//                     if token == Token::Symbol(Symbol::Assign) && allow_assign {
//                         break;
//                     }
//                     if token == Token::Symbol(Symbol::RightParen) && allow_right_parent {
//                         break;
//                     }
//
//                     self.lexer.next();
//                     return Err(SpannedParsingError::new(
//                         ParsingError::UnexpectedToken(
//                             token,
//                             vec![
//                                 Token::Symbol(Symbol::Dot),
//                                 Token::Symbol(Symbol::LeftBracket),
//                             ],
//                         ),
//                         self.lexer.span(),
//                     ));
//                 }
//             }
//         }
//
//         Ok((base_ident, start..end))
//     }
//
//     fn parse_array(&mut self) -> ParsingResult<Vec<SpannedExpression>> {
//         let mut vals = Vec::new();
//         self.contexts.push(ParsingContext::Array);
//
//         loop {
//             match self.lexer.peek() {
//                 Some(Token::Symbol(Symbol::Comma)) => {
//                     self.lexer.next();
//                 }
//                 Some(Token::Symbol(Symbol::RightBracket)) => {
//                     self.lexer.next();
//                     break;
//                 }
//                 _ => vals.push(self.parse_expression(0)?),
//             };
//         }
//
//         self.contexts.pop();
//         Ok(vals)
//     }
//
//     fn parse_test(&mut self) -> ParsingResult<SpannedExpression> {
//         self.lexer.expect(Token::Ident)?;
//         let start = self.lexer.span().start;
//         let name = self.lexer.slice().to_owned();
//         let mut args = vec![];
//         self.contexts.push(ParsingContext::TestArgs);
//
//         // Do we have arguments?
//         if let Some(Token::Symbol(Symbol::LeftParen)) = self.lexer.peek() {
//             self.lexer.next();
//
//             loop {
//                 let expr = self.parse_expression(0)?;
//                 args.push(expr);
//
//                 match self.next_or_error()? {
//                     Token::Symbol(Symbol::Comma) => {
//                         if let Some(Token::Symbol(Symbol::RightParen)) = self.lexer.peek() {
//                             // it was a trailing comma
//                             break;
//                         }
//                     }
//                     Token::Symbol(Symbol::RightParen) => {
//                         break;
//                     }
//                     t => {
//                         return Err(SpannedParsingError::new(
//                             ParsingError::UnexpectedToken(
//                                 t,
//                                 vec![
//                                     Token::Symbol(Symbol::Comma),
//                                     Token::Symbol(Symbol::RightParen),
//                                 ],
//                             ),
//                             self.lexer.span(),
//                         ));
//                     }
//                 }
//             }
//         }
//
//         self.contexts.pop();
//         Ok(SpannedExpression::new(
//             Expression::Test(Test { name, args }),
//             start..self.lexer.span().start,
//         ))
//     }
//
//     fn parse_expression(&mut self, min_bp: u8) -> ParsingResult<SpannedExpression> {
//         let mut lhs = match self.next_or_error()? {
//             Token::Integer(i) => SpannedExpression::new(Expression::Integer(i), self.lexer.span()),
//             Token::Float(i) => SpannedExpression::new(Expression::Float(i), self.lexer.span()),
//             Token::Bool(i) => SpannedExpression::new(Expression::Bool(i), self.lexer.span()),
//             Token::Ident => {
//                 // Need to parse it first in case it's actually an ident since we will move
//                 // past it otherwise
//                 let (ident, span) = self.parse_ident()?;
//                 match self.lexer.peek() {
//                     // a function
//                     Some(Token::Symbol(Symbol::LeftParen)) => {
//                         let kwargs = self.parse_kwargs()?;
//                         SpannedExpression::new(
//                             Expression::FunctionCall(FunctionCall {
//                                 name: ident,
//                                 kwargs,
//                             }),
//                             span.start..self.lexer.span().start,
//                         )
//                     }
//                     // a macro call
//                     Some(Token::Symbol(Symbol::DoubleColumn)) => {
//                         self.lexer.next();
//                         // Should be followed by macro name
//                         self.lexer.expect(Token::Ident)?;
//                         let macro_name = self.lexer.slice().to_owned();
//                         // and left paren
//                         self.peek_and_expect(Token::Symbol(Symbol::LeftParen))?;
//                         let kwargs = self.parse_kwargs()?;
//                         SpannedExpression::new(
//                             Expression::MacroCall(MacroCall {
//                                 namespace: ident,
//                                 name: macro_name,
//                                 kwargs,
//                             }),
//                             span.start..self.lexer.span().start,
//                         )
//                     }
//                     _ => SpannedExpression::new(Expression::Ident(ident), span),
//                 }
//             }
//             Token::String => SpannedExpression::new(
//                 Expression::Str(replace_string_markers(self.lexer.slice())),
//                 self.lexer.span(),
//             ),
//             Token::Symbol(Symbol::LeftBracket) => {
//                 let start = self.lexer.span().start;
//                 let array = self.parse_array()?;
//                 SpannedExpression::new(Expression::Array(array), start..self.lexer.span().end)
//             }
//             Token::Symbol(Symbol::LeftParen) => {
//                 self.contexts.push(ParsingContext::Paren);
//                 let lhs = self.parse_expression(0)?;
//                 self.lexer.expect(Token::Symbol(Symbol::RightParen))?;
//                 self.contexts.pop();
//                 lhs
//             }
//             Token::Op(op) => {
//                 let start = self.lexer.span().start;
//                 let (_, r_bp) = prefix_binding_power(op).map_err(|mut e| {
//                     e.range = self.lexer.span();
//                     e
//                 })?;
//
//                 let rhs = self.parse_expression(r_bp)?;
//                 let end = rhs.range.end;
//                 SpannedExpression::new(Expression::Expr(op, vec![rhs]), start..end)
//             }
//             t => {
//                 return Err(SpannedParsingError::new(
//                     ParsingError::UnexpectedToken(t, vec![]),
//                     self.lexer.span(),
//                 ))
//             }
//         };
//
//         let mut negated = false;
//         loop {
//             let op = match self.lexer.peek() {
//                 Some(Token::Op(op)) => op,
//                 Some(t @ Token::Symbol(_)) => {
//                     if let Some(c) = self.contexts.last() {
//                         match c {
//                             ParsingContext::Array => {
//                                 let tokens = vec![
//                                     Token::Symbol(Symbol::Comma),
//                                     Token::Symbol(Symbol::RightBracket),
//                                 ];
//                                 if !tokens.contains(&t) {
//                                     self.lexer.next();
//                                     return Err(SpannedParsingError::new(
//                                         ParsingError::UnexpectedToken(t, tokens),
//                                         self.lexer.span(),
//                                     ));
//                                 }
//                                 break;
//                             }
//                             ParsingContext::TestArgs => {
//                                 let tokens = vec![
//                                     Token::Symbol(Symbol::Comma),
//                                     Token::Symbol(Symbol::RightParen),
//                                 ];
//                                 if !tokens.contains(&t) {
//                                     self.lexer.next();
//                                     return Err(SpannedParsingError::new(
//                                         ParsingError::UnexpectedToken(t, tokens),
//                                         self.lexer.span(),
//                                     ));
//                                 }
//                                 break;
//                             }
//                             ParsingContext::Kwargs => {
//                                 let tokens = vec![
//                                     Token::Symbol(Symbol::Comma),
//                                     Token::Symbol(Symbol::RightParen),
//                                 ];
//                                 if !tokens.contains(&t) {
//                                     self.lexer.next();
//                                     return Err(SpannedParsingError::new(
//                                         ParsingError::UnexpectedToken(t, tokens),
//                                         self.lexer.span(),
//                                     ));
//                                 }
//                                 break;
//                             }
//                             _ => break,
//                         }
//                     } else {
//                         self.lexer.next();
//                         return Err(SpannedParsingError::new(
//                             ParsingError::UnexpectedToken(t, vec![]),
//                             self.lexer.span(),
//                         ));
//                     }
//                 }
//                 Some(_) => break,
//                 None => {
//                     break;
//                 }
//             };
//
//             // Special case for `not in` which is 2 operators in a row
//             if op == Operator::Not {
//                 self.lexer.next();
//                 self.peek_and_expect(Token::Op(Operator::In))?;
//                 negated = true;
//                 continue;
//             }
//
//             let (l_bp, r_bp) = infix_binding_power(op);
//             if l_bp < min_bp {
//                 break;
//             }
//             // Advance past the op
//             self.lexer.next();
//
//             let mut rhs = if op == Operator::Is {
//                 // Special-case `is not`
//                 if let Some(Token::Op(Operator::Not)) = self.lexer.peek() {
//                     negated = true;
//                     self.lexer.next();
//                 }
//                 self.parse_test()?
//             } else {
//                 match self.lexer.peek() {
//                     Some(t @ Token::Op(_)) => {
//                         // Only `is`, `and` and `or` can have an operator after and it should always be `not`
//                         if t == Token::Op(Operator::Not)
//                             && (op == Operator::And || op == Operator::Or)
//                         {
//                             self.parse_expression(r_bp)?
//                         } else {
//                             self.lexer.next();
//                             return Err(SpannedParsingError::new(
//                                 ParsingError::UnexpectedToken(t, vec![]),
//                                 self.lexer.span(),
//                             ));
//                         }
//                     }
//                     _ => self.parse_expression(r_bp)?,
//                 }
//             };
//
//             // We can have filters that look like ident, without parentheses so we need to convert
//             // them to a function
//             if op == Operator::Pipe {
//                 rhs = match rhs.node {
//                     Expression::Ident(s) => SpannedExpression::new(
//                         Expression::FunctionCall(FunctionCall {
//                             name: s,
//                             kwargs: HashMap::new(),
//                         }),
//                         rhs.range,
//                     ),
//                     _ => rhs,
//                 };
//             }
//             let start = lhs.range.start;
//             let end = rhs.range.end;
//             lhs = SpannedExpression::new(Expression::Expr(op, vec![lhs, rhs]), start..end);
//
//             if negated {
//                 let span = lhs.range.clone();
//                 lhs = SpannedExpression::new(Expression::Expr(Operator::Not, vec![lhs]), span);
//                 negated = false;
//             }
//             continue;
//         }
//
//         Ok(lhs)
//     }
//
//     fn peek_or_error(&mut self) -> ParsingResult<Token> {
//         match self.lexer.peek() {
//             Some(Token::Error) => Err(SpannedParsingError::new(
//                 ParsingError::UnexpectedToken(Token::Error, vec![]),
//                 self.lexer.span(),
//             )),
//             Some(t) => Ok(t),
//             None => {
//                 self.lexer.next();
//                 Err(eof_error(self.lexer.last_idx()))
//             }
//         }
//     }
//
//     fn next_or_error(&mut self) -> ParsingResult<Token> {
//         match self.lexer.next() {
//             Some(Token::Error) => Err(SpannedParsingError::new(
//                 ParsingError::UnexpectedToken(Token::Error, vec![]),
//                 self.lexer.span(),
//             )),
//             Some(t) => Ok(t),
//             None => Err(eof_error(self.lexer.last_idx())),
//         }
//     }
//
//     fn peek_and_expect(&mut self, token: Token) -> ParsingResult<()> {
//         match self.lexer.peek() {
//             Some(t) => {
//                 if t != token {
//                     Err(SpannedParsingError::new(
//                         ParsingError::UnexpectedToken(t, vec![token]),
//                         self.lexer.span(),
//                     ))
//                 } else {
//                     Ok(())
//                 }
//             }
//             None => Err(eof_error(self.lexer.last_idx())),
//         }
//     }
//
//     fn expect_one_of(&mut self, tokens: Vec<Token>) -> ParsingResult<Token> {
//         match self.lexer.next() {
//             Some(t) => {
//                 if !tokens.contains(&t) {
//                     Err(SpannedParsingError::new(
//                         ParsingError::UnexpectedToken(t, tokens),
//                         self.lexer.span(),
//                     ))
//                 } else {
//                     Ok(t)
//                 }
//             }
//             None => Err(eof_error(self.lexer.last_idx())),
//         }
//     }
//
//     fn expect_tag_end(&mut self) -> ParsingResult<()> {
//         if let Token::TagEnd(b) =
//             self.expect_one_of(vec![Token::TagEnd(false), Token::TagEnd(true)])?
//         {
//             self.trim_start_next = b;
//         }
//         Ok(())
//     }
// }
