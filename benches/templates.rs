use criterion::{black_box, criterion_group, criterion_main, Criterion};

use serde_derive::Serialize;

use tera::{Context, Tera};

#[derive(Serialize)]
struct Team {
    name: String,
    score: u8,
}

static BIG_TABLE_TEMPLATE: &str = "
<table>
{% for row in table %}
<tr>{% for col in row %}<td>{{ col }}</td>{% endfor %}</tr>
{% endfor %}
</table>
";

static TEAMS_TEMPLATE: &str = r#"
<html>
  <head>
    <title>{{ year }}</title>
  </head>
  <body>
    <h1>CSL {{ year }}</h1>
    <ul>
    {% for team in teams %}
      <li class="{% if loop.index0 == 0 %}champion{% endif %}">
      <b>{{ team.name }}</b>: {{ team.score }}
      </li>
    {% endfor %}
    </ul>
  </body>
</html>
"#;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("big-table", |b| {
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

        b.iter(|| {
            let res = tera.render("big-table.html", &ctx);
            black_box(res).unwrap();
        })
    });

    c.bench_function("teams", |b| {
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![("teams.html", TEAMS_TEMPLATE)])
            .unwrap();
        let mut ctx = Context::new();
        ctx.insert("year", &2015);
        ctx.insert(
            "teams",
            &vec![
                Team {
                    name: "Jiangsu".into(),
                    score: 43,
                },
                Team {
                    name: "Beijing".into(),
                    score: 27,
                },
                Team {
                    name: "Guangzhou".into(),
                    score: 22,
                },
                Team {
                    name: "Shandong".into(),
                    score: 12,
                },
            ],
        );

        b.iter(|| {
            let res = tera.render("teams.html", &ctx);
            black_box(res).unwrap();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
