mod parser;
mod generate;
mod domain;
mod val;
mod ast;

use std::fs::File;
use std::io::Read;
use crate::parser::parse_eprime_model;
use crate::generate::generate;
use crate::Assignments;
use val::*;

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
    
    let mut result = Assignments::new();
    result.insert("x".to_string(), Val::Matrix(vec![Val::Boolean(true), Val::Boolean(false)]));
    let param_file = File::create("./out.param").unwrap();
    generate(&result, param_file );
}