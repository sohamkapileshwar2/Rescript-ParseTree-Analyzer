mod parsetree;
use parsetree::purescript::purescript_function;
use parsetree::rescript::rescript_function;
use parsetree::purescript::types::Module;
use std::fs::File;
use std::io::BufReader;
use serde_json;



fn main() {
    println!("Hello, world!");
    let mut s= String::from("Hello");

    s.push_str(", Again");
    test(&mut s);
    println!("{s}");

    println!("Testing Modules");

    let file = File::open("./purescript_pt.json").expect("Failed to open file");

    let reader = BufReader::new(file);

    let data: Module<Vec<i64>> = serde_json::from_reader(reader).expect("Failed to deserialize JSON");

    purescript_function();
    rescript_function();
}

fn test(some_string: &mut String) {
    some_string.push_str(" World");
    println!("{some_string}");
}
