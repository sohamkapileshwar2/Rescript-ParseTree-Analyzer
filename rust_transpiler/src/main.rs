fn main() {
    println!("Hello, world!");
    let mut s= String::from("Hello");
    let y = s;

    s.push_str(", Again");
    let bytes = s.as_bytes()
    println!("{bytes}");
    test(&mut s);
    println!("{s}");
}

fn test(some_string: &mut String) {
    some_string.push_str(" World");
    println!("{some_string}");
}
