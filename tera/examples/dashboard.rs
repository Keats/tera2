//! Dashboard Example - Demonstrates Tera2 component features
//!
//! Run with: `cargo run --example dashboard`

use serde::Serialize;
use tera::{Context, Tera};

#[derive(Serialize)]
struct Order {
    id: u32,
    customer: String,
    total: f64,
    status: String,
}

#[derive(Serialize)]
struct Product {
    name: String,
    category: String,
    price: f64,
    stock: u32,
}

#[derive(Serialize)]
struct Customer {
    name: String,
    email: String,
}

#[derive(Serialize)]
struct Notification {
    title: String,
    message: String,
    #[serde(rename = "type")]
    notification_type: String,
    dismissible: bool,
}

#[derive(Serialize, Clone)]
struct NavItem {
    label: String,
    href: String,
    #[serde(default)]
    icon: String,
    #[serde(default)]
    active: bool,
    #[serde(default)]
    children: Vec<NavItem>,
}

fn main() {
    let mut tera = Tera::default();

    // Load dashboard templates
    if let Err(err) = tera.add_raw_templates(vec![
        ("base.html", include_str!("dashboard/base.html")),
        ("components.html", include_str!("dashboard/components.html")),
        ("dashboard.html", include_str!("dashboard/dashboard.html")),
    ]) {
        panic!("{}", err);
    }

    // Create sample data
    let orders = vec![
        Order {
            id: 1001,
            customer: "Alice Smith".to_string(),
            total: 150.00,
            status: "completed".to_string(),
        },
        Order {
            id: 1002,
            customer: "Bob Jones".to_string(),
            total: 89.99,
            status: "pending".to_string(),
        },
        Order {
            id: 1003,
            customer: "Carol White".to_string(),
            total: 245.50,
            status: "processing".to_string(),
        },
    ];

    let products = vec![
        Product {
            name: "Laptop Pro".to_string(),
            category: "Electronics".to_string(),
            price: 999.99,
            stock: 15,
        },
        Product {
            name: "Wireless Mouse".to_string(),
            category: "Electronics".to_string(),
            price: 29.99,
            stock: 150,
        },
        Product {
            name: "Cotton T-Shirt".to_string(),
            category: "Clothing".to_string(),
            price: 19.99,
            stock: 200,
        },
    ];

    let customers = vec![
        Customer {
            name: "Alice Smith".to_string(),
            email: "alice@example.com".to_string(),
        },
        Customer {
            name: "Bob Jones".to_string(),
            email: "bob@example.com".to_string(),
        },
    ];

    let notifications = vec![
        Notification {
            title: "New Order".to_string(),
            message: "You have 3 new orders waiting for review.".to_string(),
            notification_type: "info".to_string(),
            dismissible: true,
        },
        Notification {
            title: "Low Stock".to_string(),
            message: "5 products are running low on stock.".to_string(),
            notification_type: "warning".to_string(),
            dismissible: true,
        },
    ];

    // Product rows for table (as arrays of strings)
    let product_rows: Vec<Vec<String>> = products
        .iter()
        .map(|p| {
            vec![
                p.name.clone(),
                p.category.clone(),
                format!("${:.2}", p.price),
                p.stock.to_string(),
                if p.stock > 50 {
                    "In Stock".to_string()
                } else {
                    "Low Stock".to_string()
                },
            ]
        })
        .collect();

    // Navigation items (demonstrates recursive component with nested data)
    let nav_items = vec![
        NavItem {
            label: "Dashboard".to_string(),
            href: "/dashboard".to_string(),
            icon: "home".to_string(),
            active: true,
            children: vec![],
        },
        NavItem {
            label: "Products".to_string(),
            href: "/products".to_string(),
            icon: "box".to_string(),
            active: false,
            children: vec![
                NavItem {
                    label: "All Products".to_string(),
                    href: "/products".to_string(),
                    icon: "".to_string(),
                    active: false,
                    children: vec![],
                },
                NavItem {
                    label: "Categories".to_string(),
                    href: "/products/categories".to_string(),
                    icon: "".to_string(),
                    active: false,
                    children: vec![
                        NavItem {
                            label: "Electronics".to_string(),
                            href: "/products/categories/electronics".to_string(),
                            icon: "".to_string(),
                            active: false,
                            children: vec![],
                        },
                        NavItem {
                            label: "Clothing".to_string(),
                            href: "/products/categories/clothing".to_string(),
                            icon: "".to_string(),
                            active: false,
                            children: vec![],
                        },
                    ],
                },
            ],
        },
        NavItem {
            label: "Orders".to_string(),
            href: "/orders".to_string(),
            icon: "shopping-cart".to_string(),
            active: false,
            children: vec![
                NavItem {
                    label: "Pending".to_string(),
                    href: "/orders?status=pending".to_string(),
                    icon: "".to_string(),
                    active: false,
                    children: vec![],
                },
                NavItem {
                    label: "Completed".to_string(),
                    href: "/orders?status=completed".to_string(),
                    icon: "".to_string(),
                    active: false,
                    children: vec![],
                },
            ],
        },
        NavItem {
            label: "Customers".to_string(),
            href: "/customers".to_string(),
            icon: "users".to_string(),
            active: false,
            children: vec![],
        },
        NavItem {
            label: "Settings".to_string(),
            href: "/settings".to_string(),
            icon: "cog".to_string(),
            active: false,
            children: vec![],
        },
    ];

    let mut context = Context::new();
    context.insert("site_name", "Acme Dashboard");
    context.insert("current_year", "2025");
    context.insert("orders", &orders);
    context.insert("products", &products);
    context.insert("customers", &customers);
    context.insert("notifications", &notifications);
    context.insert("product_rows", &product_rows);
    context.insert("nav_items", &nav_items);
    context.insert("total_revenue", &12500);
    context.insert("selected_count", &0);
    context.insert("pending_count", &5);
    context.insert("processing_count", &3);
    context.insert("shipped_count", &12);
    context.insert("delivered_count", &45);
    context.insert("cancelled_count", &2);

    let result = tera
        .render("dashboard.html", &context)
        .expect("Failed to render");

    println!("{}", result);
}
