mod parsetree;
use parsetree::purescript::purescript_function;
use parsetree::rescript::rescript_function;


fn main() {
    println!("Hello, world!");
    let mut s= String::from("Hello");

    s.push_str(", Again");
    test(&mut s);
    println!("{s}");

    println!("Testing Modules");
    purescript_function();
    rescript_function();
}

fn test(some_string: &mut String) {
    some_string.push_str(" World");
    println!("{some_string}");
}
