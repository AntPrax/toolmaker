use std::collections::BTreeMap;
use std::collections::HashMap;
use std::fmt;
use thiserror::Error;

#[derive(Clone, Debug)]
pub enum Domain {
    Matrix(Vec<Domain>, Box<Domain>),
    Integer,
    Boolean,
}

#[derive(Clone, Debug)]
pub enum Val {
    Matrix(Vec<Val>),
    Integer(i64),
    Boolean(bool),
}

#[derive(Error, Debug)]
#[error("could not convert {val} to {typ}")]
pub struct TypeError {
    val: Val,
    typ: &'static str,
}

impl TypeError {
    pub fn new(val: Val, typ: &'static str) -> TypeError {
        TypeError { val, typ }
    }
}

impl FromVal for i64 {
    fn from_val(e: Val) -> Result<Self, TypeError> {
        match e {
            Val::Integer(i) => Ok(i),
            _ => Err(TypeError::new(e, std::any::type_name::<Self>())),
        }
    }
}
impl FromVal for bool {
    fn from_val(e: Val) -> Result<Self, TypeError> {
        match e {
            Val::Boolean(b) => Ok(b),
            _ => Err(TypeError::new(e, std::any::type_name::<Self>())),
        }
    }
}
impl From<i64> for Val {
    fn from(i: i64) -> Self {
        Self::Integer(i)
    }
}
impl From<bool> for Val {
    fn from(b: bool) -> Self {
        Self::Boolean(b)
    }
}
impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Val::*;
        match self {
            Integer(i) => write!(f, "{}", i),
            Boolean(b) => write!(f, "{}", b),
            Matrix(vals) => {
                write!(f, "[")?;
                let mut vals = vals.iter().peekable();
                while let Some(val) = vals.next() {
                    if vals.peek().is_some() {
                        write!(f, "{}, ", val)?;
                    } else {
                        write!(f, "{}", val)?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

pub trait FromVal: Sized {
    fn from_val(_: Val) -> Result<Self, TypeError>;
}

impl<T: FromVal> FromVal for Vec<T> {
    fn from_val(e: Val) -> Result<Self, TypeError> {
        match e {
            Val::Matrix(vals) => vals
                .into_iter()
                .map(|e| T::from_val(e))
                .collect::<Result<_, _>>(),
            _ => Err(TypeError::new(e, std::any::type_name::<Self>())),
        }
    }
}

pub enum ValOrDomain {
    Val(Val),
    Domain(Domain)
}

pub type IdentTable = HashMap<String, ValOrDomain>;
pub type Assignments = BTreeMap<String, Val>;