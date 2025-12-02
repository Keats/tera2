use serde::Serialize;
use std::hint::black_box;
use tera::{Context, Tera};

#[derive(Serialize)]
struct Team {
    name: String,
    score: u16,
    city: String,
    founded: u16,
    wins: u16,
    losses: u16,
}

// Bigger template with more loop variable accesses and attribute lookups
static TEAMS_TEMPLATE: &str = r#"
<html>
  <head>
    <title>{{ title }} - {{ year }}</title>
  </head>
  <body>
    <h1>{{ league_name }} {{ year }}</h1>
    <p>Total teams: {{ teams | length }}</p>
    <table>
      <thead>
        <tr><th>#</th><th>Team</th><th>City</th><th>Founded</th><th>Score</th><th>W</th><th>L</th></tr>
      </thead>
      <tbody>
      {% for team in teams %}
        <tr class="{% if loop.first %}champion{% elif loop.last %}relegated{% else %}normal{% endif %} {% if loop.index0 % 2 == 0 %}even{% else %}odd{% endif %}">
          <td>{{ loop.index }}</td>
          <td><b>{{ team.name }}</b></td>
          <td>{{ team.city }}</td>
          <td>{{ team.founded }}</td>
          <td>{{ team.score }}</td>
          <td>{{ team.wins }}</td>
          <td>{{ team.losses }}</td>
        </tr>
      {% endfor %}
      </tbody>
    </table>
    <footer>
      <p>Season {{ year }} - {{ league_name }}</p>
    </footer>
  </body>
</html>
"#;

fn main() {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![("teams.html", TEAMS_TEMPLATE)])
        .unwrap();

    // Generate 50 teams for more realistic workload
    let teams: Vec<Team> = (0..50)
        .map(|i| Team {
            name: format!("Team {}", i),
            score: (100 - i * 2) as u16,
            city: format!("City {}", i % 10),
            founded: 1900 + i as u16,
            wins: (30 - i / 2) as u16,
            losses: (i / 2) as u16,
        })
        .collect();

    let mut ctx = Context::new();
    ctx.insert("year", &2015);
    ctx.insert("title", "Football League Standings");
    ctx.insert("league_name", "Premier League");
    ctx.insert("teams", &teams);

    for _ in 0..100_000 {
        let res = tera.render("teams.html", &ctx);
        black_box(res).unwrap();
    }
}
