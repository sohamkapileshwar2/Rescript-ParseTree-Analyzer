mod parsetree;
use parsetree::purescript::purescript_function;


fn main() {
    println!("Hello, world!");
    let mut s= String::from("Hello");

    s.push_str(", Again");
    test(&mut s);
    println!("{s}");

    println!("Testing Purescript Module");
    purescript_function();
}

fn test(some_string: &mut String) {
    some_string.push_str(" World");
    println!("{some_string}");
}
