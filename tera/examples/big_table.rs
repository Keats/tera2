use tera::{Context, Tera, TeraResult};

static BIG_TABLE_TEMPLATE: &str = "
<table>
{% for row in table %}
<tr>{% for col in row %}<td>{{ col }}</td>{% endfor %}</tr>
{% endfor %}
</table>
";

fn setup() -> (Tera, Context) {
    let length = 100;
    let mut table = Vec::with_capacity(length);
    for _ in 0..length {
        let mut inner = Vec::with_capacity(length);
        for i in 0..length {
            inner.push(i);
        }
        table.push(inner);
    }

    let mut ctx = Context::new();
    ctx.insert("table", &table);
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![("big-table.html", BIG_TABLE_TEMPLATE)])
        .unwrap();
    let mut ctx = Context::new();
    ctx.insert("table", &table);
    (tera, ctx)
}

fn render(tera: Tera, ctx: Context) -> TeraResult<String> {
    tera.render("big-table.html", &ctx)
}

fn main() {
    let (tera, ctx) = setup();
    let res = render(tera, ctx);
    println!("{:?}", res.is_ok());
}
