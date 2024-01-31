use crate::Tera;

/// This will take our custom insta format to create multiple templates
/// The format is:
/// $$ filename
/// body
/// $$ other filename
/// other body
pub fn split_multi_templates(body: &str) -> Vec<(String, String)> {
    let parts: Vec<_> = body.split("$$ ").skip(1).collect();
    let mut tpls = Vec::with_capacity(parts.len());
    for part in parts {
        let mut chars = part.chars();
        let filename: String = chars.by_ref().take_while(|&c| c != '\n').collect();
        let content = chars.collect::<String>().trim().to_string();
        tpls.push((filename, content));
    }
    return tpls
}


/// Splits the body into multiple templates ans
/// returns the tera instance as well as the last template name.
/// To be used when the templates are valid only
pub fn create_multi_templates_tera(body: &str) -> (Tera, String) {
    let tpls = split_multi_templates(body);
    let last_filename = tpls.last().unwrap().0.clone();
    let mut tera = Tera::default();
    tera.add_raw_templates(tpls).unwrap();

    (tera, last_filename)
}