// use std::collections::BTreeMap;
// use valuable::Valuable;
//
// // #[derive(Debug, Clone, PartialEq)]
// // pub struct Context {
// //     data: BTreeMap<String, Valuable>,
// // }
//
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use std::borrow::Borrow;
//     use valuable::Listable;
//
//     #[test]
//     fn can_use_valuable() {
//         use valuable::{NamedValues, Valuable, Value, Visit};
//
//         fn parse_index(s: &str) -> Option<usize> {
//             if s.starts_with('+') || (s.starts_with('0') && s.len() != 1) {
//                 return None;
//             }
//             s.parse().ok()
//         }
//
//         struct ObjectAccess<'a> {
//             parts: Vec<&'a str>,
//             res: Option<&'a dyn Valuable>,
//         }
//
//         impl<'a> ObjectAccess<'a> {
//             pub fn new(pointer: &'a str) -> Self {
//                 // very stupid for now
//                 let parts: Vec<_> = pointer.split('.').rev().collect();
//                 Self { parts, res: None }
//             }
//         }
//
//         impl Visit for ObjectAccess<'_> {
//             fn visit_value(&mut self, value: Value<'_>) {
//                 match value {
//                     Value::Structable(v) => {
//                         let def = v.definition();
//                         v.visit(self);
//                     }
//                     Value::Enumerable(v) => {
//                         let def = v.definition();
//                         let variant = v.variant();
//                         // Print the enum name
//                         println!("{}::{}:", def.name(), variant.name());
//                         // Visit fields
//                         v.visit(self);
//                     }
//                     Value::Listable(v) => {
//                         println!("Listable");
//                         // Visit fields
//                         v.visit(self);
//                     }
//                     Value::Mappable(v) => {
//                         println!("Mappable TODO");
//                         // Visit fields
//                         v.visit(self);
//                     }
//                     // Primitive or unknown type, just render Debug
//                     v => {
//                         if self.parts.is_empty() {
//                             self.res = Some(value);
//                             return;
//                         }
//                     }
//                 }
//             }
//
//             fn visit_named_fields(&mut self, named_values: &NamedValues<'_>) {
//                 let current = self.parts.last();
//                 if current.is_none() {
//                     return;
//                 }
//
//                 for (field, value) in named_values {
//                     if field.name() == *current.unwrap() {
//                         self.parts.pop();
//                         return value.visit(self);
//                     }
//                 }
//             }
//
//             fn visit_unnamed_fields(&mut self, values: &[Value<'_>]) {
//                 println!("Unnamed field");
//                 for value in values {
//                     println!("{:?}", value);
//                 }
//             }
//
//             fn visit_entry(&mut self, key: Value<'_>, value: Value<'_>) {
//                 println!("{:?} => {:?}", key, value);
//             }
//         }
//
//         #[derive(Valuable)]
//         struct Person {
//             name: String,
//             age: u32,
//             addresses: Vec<Address>,
//             years: Vec<usize>,
//         }
//
//         #[derive(Valuable)]
//         struct Address {
//             street: String,
//             city: String,
//             zip: String,
//         }
//
//         let person = Person {
//             name: "Angela Ashton".to_string(),
//             age: 31,
//             years: vec![2000, 2010, 2020],
//             addresses: vec![
//                 Address {
//                     street: "123 1st Ave".to_string(),
//                     city: "Townsville".to_string(),
//                     zip: "12345".to_string(),
//                 },
//                 Address {
//                     street: "555 Main St.".to_string(),
//                     city: "New Old Town".to_string(),
//                     zip: "55555".to_string(),
//                 },
//             ],
//         };
//
//         let mut object_access = ObjectAccess::new("name");
//         valuable::visit(&person, &mut object_access);
//         assert_eq!(
//             object_access.res.unwrap().as_value().as_str(),
//             Some(person.name.as_str())
//         );
//
//         let mut object_access = ObjectAccess::new("years.1");
//         valuable::visit(&person, &mut object_access);
//         assert_eq!(object_access.res.unwrap().as_value().as_i64(), Some(2010));
//
//         let mut object_access = ObjectAccess::new("addresses.0.city");
//         valuable::visit(&person, &mut object_access);
//         assert_eq!(
//             object_access.res.unwrap().as_value().as_str(),
//             Some(person.addresses[0].city.as_str())
//         );
//     }
// }
