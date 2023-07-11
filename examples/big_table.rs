use tera::{Context, Tera};

static BIG_TABLE_TEMPLATE: &str = "
<table>
{% for row in table %}
<tr>{% for col in row %}<td>{{ col }}</td>{% endfor %}</tr>
{% endfor %}
</table>
";

fn main() {
    let length = 100;
    let mut table = Vec::with_capacity(length);
    for _ in 0..length {
        let mut inner = Vec::with_capacity(length);
        for i in 0..length {
            inner.push(i);
        }
        table.push(inner);
    }

    let mut tera = Tera::default();
    tera.add_raw_templates(vec![("big-table.html", BIG_TABLE_TEMPLATE)])
        .unwrap();
    let mut ctx = Context::new();
    ctx.insert("table", &table);
    let res = tera.render("big-table.html", &ctx);
    println!("{:?}", res.is_ok());
}
