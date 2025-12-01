use serde::Serialize;
use std::hint::black_box;
use tera::{Context, Tera};

#[derive(Serialize)]
struct Team {
    name: String,
    score: u8,
}

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

fn main() {
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

    for _ in 0..100_000 {
        let res = tera.render("teams.html", &ctx);
        black_box(res).unwrap();
    }
}
