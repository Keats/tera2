use std::error::Error;
use tera::{Context, Kwargs, State, Tera, Value};

pub fn do_nothing_filter(value: Value, _: Kwargs, _: &State) -> Value {
    value
}

// TODO: move examples folder to root of the repo to be able to select features
fn main() {
    let mut tera = Tera::new();
    tera.register_filter("do_nothing", do_nothing_filter);
    tera.load_from_glob("examples/basic/templates/**/*")
        .unwrap();

    let mut context = Context::new();
    context.insert("username", &"Bob");
    context.insert("numbers", &vec![1, 2, 3]);
    context.insert("show_all", &false);
    context.insert("bio", &"<script>alert('pwnd');</script>");

    // A one off template
    Tera::one_off("hello", &Context::new(), true).unwrap();

    match tera.render("users/profile.html", &context) {
        Ok(s) => println!("{:?}", s),
        Err(e) => {
            println!("Error: {}", e);
            let mut cause = e.source();
            while let Some(e) = cause {
                println!("Reason: {}", e);
                cause = e.source();
            }
        }
    };
}
