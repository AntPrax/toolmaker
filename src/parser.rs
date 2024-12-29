use crate::ast::*;
use crate::val::*;
use chumsky::prelude::*;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Write;
type Span = std::ops::Range<usize>;

pub struct EvaluatedModel {
    pub finds: Vec<(String, Domain)>,
    pub constraints: Vec<Constraint>,
}

impl DomainExpr {
    fn eval(self, domains: &HashMap<String, Domain>, span: Span) -> Result<Domain, Simple<Token>> {
        use crate::ast::DomainExpr::*;
        match self {
            Integer => Ok(Domain::Integer),
            Boolean => Ok(Domain::Boolean),
            Alias(name) => domains
                .get(&name)
                .cloned()
                .ok_or_else(|| Simple::custom(span, Token::Ident(name))),
            Matrix(indices, valdom) => {
                let is = indices
                    .into_iter()
                    .map(|d| d.eval(domains, span.clone()))
                    .collect::<Result<Vec<_>, Simple<Token>>>()?;
                let vd = valdom.eval(domains, span)?;
                Ok(Domain::Matrix(is, Box::new(vd)))
            }
        }
    }
}

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
enum Token {
    Letting,
    Be,
    Domain,
    Matrix,
    Indexed,
    By,
    Of,
    Given,
    Find,
    Such,
    That,
    Maximising,
    Minimising,
    Equals,
    Colon,
    Semicolon,
    Comma,
    Int,
    Bool,
    LBracket,
    RBracket,
    LParen,
    RParen,
    EssencePrime,
    VersionNum,
    BoolLit(bool),
    IntLit(i64),
    Ident(String),
    Plus,
    Minus,
    Times,
    ToThePower,
    LessThan,
    GreaterThan,
    And,
    Or,
    Xor,
    Not,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Token::*;

        match self {
            Letting => f.write_str("letting"),
            Be => f.write_str("be"),
            Domain => f.write_str("domain"),
            Matrix => f.write_str("matrix"),
            Indexed => f.write_str("indexed"),
            By => f.write_str("by"),
            Of => f.write_str("of"),
            Given => f.write_str("given"),
            Find => f.write_str("find"),
            Such => f.write_str("such"),
            That => f.write_str("that"),
            Maximising => f.write_str("maximising"),
            Minimising => f.write_str("minimising"),
            Equals => f.write_str("="),
            Colon => f.write_str(":"),
            Semicolon => f.write_str(";"),
            Comma => f.write_str(","),
            Int => f.write_str("int"),
            Bool => f.write_str("bool"),
            LBracket => f.write_str("["),
            RBracket => f.write_str("]"),
            LParen => f.write_str("("),
            RParen => f.write_str(")"),
            EssencePrime => f.write_str("ESSENCE'"),
            VersionNum => f.write_str("1.0"),
            BoolLit(b) => write!(f, "{}", b),
            IntLit(i) => write!(f, "{}", i),
            Ident(s) => write!(f, "{}", s),
            And => f.write_str("/\\"),
            Or => f.write_str("\\/"),
            Plus => f.write_char('+'),
            Minus => f.write_char('-'),
            Times => f.write_char('*'),
            ToThePower => f.write_str("**"),
            LessThan => f.write_char('<'),
            GreaterThan => f.write_char('-'),
            Xor => f.write_str("XOR"),
            Not => f.write_char('!'),
        }
    }
}

fn parse_comments() -> impl Parser<char, (), Error = Simple<char>> {
    just('$')
        .ignore_then(take_until(text::newline()))
        .ignore_then(text::whitespace())
        .repeated()
        .ignored()
        .boxed()
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    use text::keyword;
    use Token::*;

    let int_lit = just('-')
        .or_not()
        .then(text::int(10).from_str::<i64>())
        .try_map(|(minus, i), span| {
            let mut i: i64 = i.map_err(|e| Simple::custom(span, e.to_string()))?;
            if minus.is_some() {
                i = -i;
            }
            Ok(IntLit(i))
        });

    let token1 = choice((
        keyword("letting").to(Letting),
        keyword("be").to(Be),
        keyword("domain").to(Domain),
        keyword("matrix").to(Matrix),
        keyword("indexed").to(Indexed),
        keyword("by").to(By),
        keyword("given").to(Given),
        keyword("letting").to(Letting),
        keyword("find").to(Find),
        keyword("of").to(Of),
        keyword("int").to(Int),
        keyword("bool").to(Bool),
        keyword("false").to(BoolLit(false)),
        keyword("true").to(BoolLit(true)),
        keyword("such").to(Such),
        keyword("that").to(That),
        keyword("minimising").to(Minimising),
        keyword("maximising").to(Maximising),
    ))
    .boxed();

    let token2 = choice((
        just("1.0").to(VersionNum),
        int_lit,
        just('=').to(Equals),
        just(':').to(Colon),
        just(';').to(Semicolon),
        just(',').to(Comma),
        just('(').to(LParen),
        just(')').to(RParen),
        just('[').to(LBracket),
        just(']').to(RBracket),
        just("ESSENCE'").to(EssencePrime),
        just('+').to(Plus),
        just('-').to(Minus),
        just("/\\").to(And),
        just("\\/").to(Or),
        just("**").to(ToThePower),
        just('*').to(Times),
        text::ident().map(Token::Ident),
    ))
    .boxed();

    let token = token1.or(token2); // split up because choice is only implemented for tuples up to a fixed size

    parse_comments()
        .ignore_then(token)
        .padded()
        .repeated()
        .then_ignore(parse_comments())
        .boxed()
}

fn bin_op(
    expr: impl Parser<Token, ValExpr, Error = Simple<Token>> + Clone,
    token: Token,
    constructor: fn(Box<ValExpr>, Box<ValExpr>) -> ValExpr,
) -> impl Parser<Token, ValExpr, Error = Simple<Token>> {
    expr.clone()
        .then_ignore(just(token))
        .then(expr)
        .map(move |(a, b)| constructor(Box::new(a), Box::new(b)))
}

fn parse_val_expr() -> impl Parser<Token, ValExpr, Error = Simple<Token>> {
    use Token::*;

    recursive(|expr| {
        let matrix = expr
            .clone()
            .chain(just(Comma).ignore_then(expr.clone()).repeated())
            .or_not()
            .flatten()
            .then_ignore(just(Semicolon).then(parse_aliased_domain()).or_not())
            .delimited_by(just(LBracket), just(RBracket))
            .map(ValExpr::Matrix);

        let primitive = select! {
            Ident(s) => ValExpr::Alias(s),
            IntLit(x) => ValExpr::Integer(x),
            BoolLit(b) => ValExpr::Boolean(b),
        };

        let bracketed = expr.clone().delimited_by(just(LBracket), just(RBracket));

        let times = bin_op(expr.clone(), Times, ValExpr::Times);
        let plus = bin_op(expr.clone(), Plus, ValExpr::Plus);
        let minus = bin_op(expr.clone(), Minus, ValExpr::Minus);

        choice((
            primitive, matrix, bracketed, // power,
            times, plus, minus,
            // unary_minus
        ))
    })
    .boxed()
}

fn parse_val() -> impl Parser<Token, Val, Error = Simple<Token>> {
    use Token::*;

    recursive(|val| {
        let matrix = val
            .clone()
            .chain(just(Comma).ignore_then(val).repeated())
            .or_not()
            .flatten()
            .then_ignore(just(Semicolon).then(parse_aliased_domain()).or_not())
            .delimited_by(just(LBracket), just(RBracket))
            .map(Val::Matrix);

        let primitive = select! {
            IntLit(x) => Val::Integer(x),
            BoolLit(b) => Val::Boolean(b)
        };

        choice((primitive, matrix))
    })
    .boxed()
}

fn parse_aliased_domain() -> impl Parser<Token, DomainExpr, Error = Simple<Token>> {
    use Token::*;

    let expr = recursive(|e| {
        none_of([
            Letting, Be, Domain, Matrix, Given, Indexed, By, Find, Of, Int, Bool, LParen, RParen,
        ])
        .repeated()
        .at_least(1)
        .or(e.delimited_by(just(LParen), just(RParen)))
    })
    .boxed();

    recursive(|domain| {
        let int = just(Int)
            .ignore_then(
                expr.repeated()
                    .delimited_by(just(LParen), just(RParen))
                    .or_not(),
            )
            .to(DomainExpr::Integer);

        let bool = just(Bool).to(DomainExpr::Boolean);

        let matrix = just(Matrix)
            .then_ignore(just(Indexed))
            .then_ignore(just(By))
            .ignore_then(
                domain
                    .clone()
                    .repeated()
                    .at_least(1)
                    .delimited_by(just(LBracket), just(RBracket)),
            )
            .then_ignore(just(Of))
            .then(domain)
            .map(|(indexes, valtype)| DomainExpr::Matrix(indexes, Box::new(valtype)));

        let ident = select! {
            Ident(s) => DomainExpr::Alias(s)
        };

        choice((matrix, bool, int, ident))
    })
    .boxed()
}

fn letting_prefix_parser() -> impl Parser<Token, String, Error = Simple<Token>> {
    use Token::*;

    let ident = select! {
        Ident(s) => s
    };

    just(Letting)
        .ignore_then(ident)
        .then_ignore(just(Be).or(just(Equals)))
        .boxed()
}

fn statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> {
    use Token::*;

    let ident = select! {
        Ident(s) => s
    };

    let letting = letting_prefix_parser()
        .then_ignore(just(Domain))
        .then(parse_aliased_domain())
        .map(|(name, dom)| Statement::LettingDomain(name, dom))
        .or(letting_prefix_parser()
            .then(parse_val_expr())
            .map(|(name, val)| Statement::LettingVal(name, val)));

    let decl = |decl_keyword| {
        just(decl_keyword)
            .ignore_then(ident)
            .then_ignore(just(Colon))
            .then(parse_aliased_domain())
    };

    let given = decl(Given).map(|(i, d)| Statement::Given(i, d));
    let find = decl(Find).map(|(i, d)| Statement::Find(i, d));

    choice((letting, given, find)).boxed()
}

fn lang_decl_parser() -> impl Parser<Token, (), Error = Simple<Token>> {
    use Token::*;

    just(Ident("language".to_string()))
        .ignore_then(just(EssencePrime))
        .ignore_then(just(VersionNum))
        .ignored()
        .boxed()
}

fn model_parser() -> impl Parser<Token, Model, Error = Simple<Token>> {
    use Token::*;

    lang_decl_parser()
        .ignore_then(statement_parser().repeated())
        .then_ignore(one_of([Such, Maximising, Minimising]))
        .validate(|stmts, span, emit| {
            let mut domains = HashMap::new();
            let mut givens = Vec::new();
            let mut finds = Vec::new();
            let mut constants = HashMap::new();

            for stmt in stmts {
                match stmt {
                    Statement::Given(i, d) => givens.push((i, d)),
                    Statement::Find(i, d) => finds.push((i, d)),
                    Statement::LettingDomain(i, d) => {
                        domains.insert(i, d);
                    }
                    Statement::LettingVal(name, val) => {
                        constants.insert(name, val);
                    }
                }
            }

            Model {
                givens,
                finds,
                domains,
                constants,
                constraints: Vec::new(),
            }
        })
        .boxed()
}

fn error<I: fmt::Display + std::hash::Hash + Eq>(e: Simple<I>) -> String {
    let str = "Error parsing ESSENCE': \n\t";
    match e.reason() {
        chumsky::error::SimpleReason::Custom(s) => format!("{}{}", str, s),
        _ => format!("{}{}", str, e),
    }
}

fn parse_eprime<T, P>(string: &str, parser: P) -> Result<T, Vec<String>>
where
    P: Parser<Token, T, Error = Simple<Token>>,
{
    let (tokens, errors) = lexer().parse_recovery(string);

    let mut errors: Vec<_> = errors.into_iter().map(error).collect();

    if let Some(tokens) = tokens {
        let result: Result<_, Vec<_>> = parser
            .parse(tokens)
            .map_err(|e| e.into_iter().map(error).collect());
        if let Err(mut errors2) = result {
            errors.append(&mut errors2);
            Err(errors)
        } else {
            result
        }
    } else {
        Err(errors)
    }
}

pub fn parse_eprime_model(model: &str) -> Result<Model, Vec<String>> {
    parse_eprime(model, model_parser())
}

pub fn parse_eprime_params(params: &str) -> Result<Assignments, Vec<String>> {
    parse_eprime(params, param_parser())
}

fn duplicate_decl(span: std::ops::Range<usize>, name: String) -> Simple<Token> {
    Simple::<Token>::custom(span, format!("Duplicate declaration of {name}"))
}

pub fn param_parser() -> impl Parser<Token, Assignments, Error = Simple<Token>> {
    let letting = letting_prefix_parser().then(parse_val());

    lang_decl_parser()
        .ignore_then(letting.repeated())
        .then_ignore(end())
        .validate(|lettings, span, emit| {
            let mut map = Assignments::new();
            for (name, dom) in lettings {
                if map.insert(name.clone(), dom).is_some() {
                    emit(duplicate_decl(span.clone(), name));
                }
            }
            map
        })
        .boxed()
}
