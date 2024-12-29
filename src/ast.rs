use crate::TypeError;
use crate::Val;
use std::collections::HashMap;

pub struct Model {
    pub finds: Vec<(String, DomainExpr)>,
    pub givens: Vec<(String, DomainExpr)>,
    pub domains: HashMap<String, DomainExpr>,
    pub constraints: Vec<Constraint>,
    pub constants: HashMap<String, ValExpr>,
}

#[derive(Clone, Debug)]
pub enum DomainExpr {
    Alias(String),
    Matrix(Vec<DomainExpr>, Box<DomainExpr>),
    Integer,
    Boolean,
}

#[derive(Clone, Debug)]
pub enum ValExpr {
    Plus(Box<ValExpr>, Box<ValExpr>),
    Minus(Box<ValExpr>, Box<ValExpr>),
    Times(Box<ValExpr>, Box<ValExpr>),
    And(Box<ValExpr>, Box<ValExpr>),
    Or(Box<ValExpr>, Box<ValExpr>),
    Alias(String),
    Matrix(Vec<ValExpr>),
    Integer(i64),
    Boolean(bool),
}

impl ValExpr {
    pub fn eval(expr: ValExpr, vals: &mut HashMap<String, Val>) -> Result<Val, TypeError> {
        // use ValExpr::*;
        // match expr {
        //     Plus(a, b) => eval_int(a) + eval_int(b)
        // }
        todo!();
    }

    // pub fn eval_int(expr: ValExpr) -> Result<i64, TypeError> {

    // }
}

#[derive(Clone, Debug)]
pub enum Statement {
    LettingDomain(String, DomainExpr),
    LettingVal(String, ValExpr),
    Given(String, DomainExpr),
    Find(String, DomainExpr),
}

#[derive(Clone, Debug)]
pub enum Constraint {}
