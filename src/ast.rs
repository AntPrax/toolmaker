#[derive(Clone, Debug)]
pub enum AliasedDomain {
    Alias(String),
    Matrix(Vec<AliasedDomain>, Box<AliasedDomain>),
    Integer,
    Boolean,
}

#[derive(Clone, Debug)]
pub enum AliasedVal {
    Alias(String),
    Matrix(Vec<AliasedVal>),
    Integer(i64),
    Boolean(bool),
}

#[derive(Clone, Debug)]
pub struct AliasedDecl {
    pub name: String,
    pub dom: AliasedDomain,
}

#[derive(Clone, Debug)]
pub enum Statement {
    LettingDomain(AliasedDecl),
    LettingVal(String, AliasedVal),
    Given(AliasedDecl),
    Find(AliasedDecl)
}


#[derive(Clone, Debug)]
pub enum Constraint {}
