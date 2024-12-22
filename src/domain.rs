#[derive(Clone, Debug)]
pub enum Domain {
    Matrix(Vec<Domain>, Box<Domain>),
    Integer,
    Boolean,
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub name: String,
    pub dom: Domain
}