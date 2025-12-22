# Dashboard UI Kit Example

This example demonstrates all major Tera2 component features in a realistic dashboard scenario.

## Files

- `base.html` - Base layout template with blocks
- `components.html` - Reusable UI component definitions
- `dashboard.html` - Main dashboard using all features

## Features Demonstrated

### 1. Template Inheritance (Blocks)

`base.html` defines the page structure with customizable blocks:

```jinja2
{% block title %}Dashboard{% endblock %}
{% block header %}...{% endblock %}
{% block sidebar %}{% endblock %}
{% block content %}{% endblock %}
{% block footer %}...{% endblock %}
```

`dashboard.html` extends and overrides these blocks:

```jinja2
{% extends "base.html" %}
{% block content %}
  ...dashboard content...
{% endblock %}
```

### 2. Hierarchical Namespacing

Components use dot notation for logical grouping:

```jinja2
{% component ui.icon(name) %}...{% endcomponent ui.icon %}
{% component ui.button(label, variant) %}...{% endcomponent ui.button %}
{% component ui.nav.item(label, href, children) %}...{% endcomponent ui.nav.item %}
{% component layout.section(title) %}...{% endcomponent layout.section %}
```

### 3. Typed Parameters with Defaults

```jinja2
{% component ui.button(label: string, variant: string = "primary", size: string = "md", ...attrs) %}
```

- `label: string` - Required string parameter
- `variant: string = "primary"` - Optional with default
- `...attrs` - Rest parameter collects extra kwargs

### 4. Rest Parameters (`...rest`)

Open components accept additional attributes:

```jinja2
{% component ui.button(label, variant, ...attrs) %}
<button class="btn {% if attrs.class %}{{ attrs.class }}{% endif %}"
        {% if attrs.disabled %}disabled{% endif %}>
```

Usage:

```jinja2
{{ <ui.button label="Save" class="custom" disabled={true} /> }}
```

### 5. Spread Operator

Pass a map's contents as component arguments:

```jinja2
{% set btn_defaults = {"type": "button", "class": "shadow-sm"} %}
{{ <ui.button label="Save" {...btn_defaults} /> }}
```

Order matters - later values override earlier:

```jinja2
{# explicit wins (after spread) #}
{{ <ui.button label="Test" {...defaults} variant="danger" /> }}

{# spread wins (after explicit) #}
{{ <ui.button label="Test" variant="primary" {...overrides} /> }}
```

### 6. Body Slots

Components can wrap content using `{{ body }}`:

```jinja2
{% component ui.card(title) %}
<div class="card">
    <h3>{{ title }}</h3>
    <div class="card-body">{{ body }}</div>
</div>
{% endcomponent ui.card %}
```

Usage with block syntax:

```jinja2
{% <ui.card title="My Card"> %}
    <p>This content goes into {{ body }}</p>
{% </ui.card> %}
```

### 7. Recursive Components

`ui.nav.item` calls itself for nested navigation:

```jinja2
{% component ui.nav.item(label, href, children: array = []) %}
<li>
    <a href="{{ href }}">{{ label }}</a>
    {% if children | length > 0 %}
    <ul>
        {% for child in children %}
        {{ <ui.nav.item
            label={child.label}
            href={child.href}
            children={child.children | default(value=[])}
        /> }}
        {% endfor %}
    </ul>
    {% endif %}
</li>
{% endcomponent ui.nav.item %}
```

### 8. Expression Arguments

Pass computed values using `{expression}` syntax:

```jinja2
{{ <ui.stat label="Total Orders" value={orders | length} /> }}
{{ <ui.stat label="Revenue" value={"$" ~ total_revenue} /> }}
{{ <ui.badge label={status.label ~ " (" ~ status.count ~ ")"} /> }}
```

### 9. Shorthand Attributes

When variable name matches parameter name:

```jinja2
{% set label = "Quick Save" %}
{% set variant = "success" %}
{{ <ui.button label variant /> }}
{# Equivalent to: <ui.button label={label} variant={variant} /> #}
```

### 10. Nested Components

Components can use other components internally:

```jinja2
{% component ui.button(label, icon, ...) %}
<button>
    {% if icon %}{{ <ui.icon name={icon} /> }}{% endif %}
    {{ label }}
</button>
{% endcomponent ui.button %}
```

In templates, you can nest both inline and block components:

```jinja2
{% <ui.card title="Outer"> %}
    <p>Some content</p>
    {% <ui.card title="Inner"> %}
        {{ <ui.button label="Deep" /> }}
    {% </ui.card> %}
{% </ui.card> %}
```

## Running the Example

```bash
cargo run --example dashboard
```

Or in your own code:

```rust
use tera::{Tera, Context};

let mut tera = Tera::default();
tera.add_raw_template("base.html", include_str!("base.html"))?;
tera.add_raw_template("components.html", include_str!("components.html"))?;
tera.add_raw_template("dashboard.html", include_str!("dashboard.html"))?;

let mut context = Context::new();
context.insert("site_name", "My Dashboard");
context.insert("orders", &vec![/* order data */]);
context.insert("products", &vec![/* product data */]);
// ... more data

let html = tera.render("dashboard.html", &context)?;
```

## Component Reference

| Component | Parameters | Features |
|-----------|-----------|----------|
| `ui.icon` | `name: string` | Simple leaf |
| `ui.badge` | `label: string, variant = "default"` | Defaults |
| `ui.button` | `label, variant = "primary", size = "md", ...attrs` | Rest params |
| `ui.card` | `title = "", ...attrs` | Body slot |
| `ui.alert` | `variant = "info", dismissible = false` | Body slot, bool |
| `ui.stat` | `label, value, icon = "", trend = ""` | Nested component |
| `ui.nav.item` | `label, href = "#", icon = "", children = [], active = false` | Recursive |
| `ui.table` | `headers: array, rows: array, ...attrs` | Typed arrays |
| `layout.section` | `title = "", subtitle = "", ...attrs` | Body slot |
