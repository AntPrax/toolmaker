mod parser;

use std::fs::File;
use std::io::Read;
use crate::parser::parser::parse_eprime_model;

fn main() {
    let mut file = File::open("models/golomb.eprime").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let res = parse_eprime_model(&contents);
    match res {
        Ok(_decls) => println!("File parsed successfully!"),
        Err(errors) => {
            for error in errors {
                println!("{}", error);
            }
        }
    }
    
}