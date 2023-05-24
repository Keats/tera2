use crate::template::Template;

struct VirtualMachine<'t> {
    template: &'t Template,
    ip: usize,
}

impl<'t> VirtualMachine<'t> {
    pub fn new(template: &'t Template) -> Self {
        Self { template, ip: 0 }
    }

    fn interpret(&self) {}
}
