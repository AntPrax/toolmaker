use std::io::Write;
use crate::Assignments;

pub fn generate(assignments: &Assignments, mut file: std::fs::File) {
    for (k, v) in assignments {
        writeln!(file, "letting {} be {}", k, v).unwrap();
    }
}