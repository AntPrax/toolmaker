pub mod parser {
    use chumsky::prelude::*;
    use std::{collections::HashMap, fmt};
    use thiserror::Error;

    type Span = std::ops::Range<usize>;

    #[derive(Clone)]
    pub enum Domain {
        Matrix(Vec<Domain>, Box<Domain>),
        Integer,
        Boolean,
    }
    
    #[derive(Clone, Debug)]
    enum AliasedDomain {
        Alias(String),
        Matrix(Vec<AliasedDomain>, Box<AliasedDomain>),
        Integer,
        Boolean,
    }
    
    impl AliasedDomain {
        fn unalias(
            self,
            domains: &HashMap<String, Domain>,
            span: Span,
        ) -> Result<Domain, Simple<Token>> {
            use AliasedDomain::*;
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
                        .map(|d| d.unalias(domains, span.clone()))
                        .collect::<Result<Vec<_>, Simple<Token>>>()?;
                    let vd = valdom.unalias(domains, span)?;
                    Ok(Domain::Matrix(is, Box::new(vd)))
                }
            }
        }
    }
    
    pub struct Decl {
        pub name: String,
        pub dom: Domain,
    }
    
    pub struct Decls {
        pub finds: Vec<Decl>,
        pub givens: Vec<Decl>,
    }
    
    pub type EPrimeMap = HashMap<String, Val>;
    
    type MakeSol<S> = fn(EPrimeMap) -> S;

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
    
    #[derive(Clone, Debug)]
    pub enum Val {
        Matrix(Vec<Val>),
        Integer(i64),
        Boolean(bool),
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
        Other(char),
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
                Other(c) => write!(f, "{}", c),
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
            text::ident().map(Token::Ident),
            any().map(Other),
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
    
    fn parse_val() -> impl Parser<Token, Val, Error = Simple<Token>> {
        use Token::*;
    
        recursive(|val| {
            let matrix = val
                .clone()
                .chain(just(Comma).ignore_then(val).repeated())
                .or_not()
                .flatten()
                .then_ignore(just(Semicolon).then(parse_domain()).or_not())
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
    
    fn parse_domain() -> impl Parser<Token, AliasedDomain, Error = Simple<Token>> {
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
                .to(AliasedDomain::Integer);
    
            let bool = just(Bool).to(AliasedDomain::Boolean);
    
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
                .map(|(indexes, valtype)| AliasedDomain::Matrix(indexes, Box::new(valtype)));
    
            let ident = select! {
                Ident(s) => AliasedDomain::Alias(s)
            };
    
            choice((matrix, bool, int, ident))
        })
        .boxed()
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
    
    #[derive(Clone, Debug)]
    enum Statement {
        LettingDomain(String, AliasedDomain),
        LettingVal,
        Given(String, AliasedDomain),
        Find(String, AliasedDomain),
    }
    
    fn statement_parser() -> impl Parser<Token, Statement, Error = Simple<Token>> {
        use Token::*;
    
        let letting = letting_prefix_parser()
            .then_ignore(just(Domain))
            .then(parse_domain())
            .map(|(ident, dom)| Statement::LettingDomain(ident, dom))
            .or(letting_prefix_parser()
                .then(take_until(
                    choice((just(Letting), just(Given), just(Find))).ignored(),
                ))
                .to(Statement::LettingVal));
    
        let ident = select! {
            Ident(s) => s
        };
    
        let decl = |decl_keyword| {
            just(decl_keyword)
                .ignore_then(ident)
                .then_ignore(just(Colon))
                .then(parse_domain())
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
    
    fn model_parser() -> impl Parser<Token, Decls, Error = Simple<Token>> {
        use Token::*;
    
        lang_decl_parser()
            .ignore_then(statement_parser().repeated())
            .then_ignore(one_of([Such, Maximising, Minimising]))
            .validate(|stmts, span, emit| {
                let mut domains = HashMap::new();
                let mut givens = Vec::new();
                let mut finds = Vec::new();
    
                for stmt in stmts {
                    match stmt {
                        Statement::Given(name, d) => match d.unalias(&domains, span.clone()) {
                            Err(e) => emit(e),
                            Ok(dom) => givens.push(Decl { name, dom }),
                        },
                        Statement::Find(name, d) => match d.unalias(&domains, span.clone()) {
                            Err(e) => emit(e),
                            Ok(dom) => finds.push(Decl { name, dom }),
                        },
                        Statement::LettingDomain(name, d) => match d.unalias(&domains, span.clone()) {
                            Err(e) => emit(e),
                            Ok(dom) => {
                                if domains.insert(name.clone(), dom).is_some() {
                                    emit(duplicate_decl(span.clone(), name));
                                }
                            }
                        },
                        _ => {}
                    }
                }
    
                Decls { givens, finds }
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
    
    pub fn parse_eprime_model(eprime: &str) -> Result<Decls, Vec<String>> {
        let (tokens, errors) = lexer().parse_recovery(eprime);
    
        let mut errors: Vec<_> = errors.into_iter().map(error).collect();
    
        if let Some(tokens) = tokens {
            let result: Result<_, Vec<_>> = model_parser()
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
    
    fn duplicate_decl(span: std::ops::Range<usize>, name: String) -> Simple<Token> {
        Simple::<Token>::custom(span, format!("Duplicate declaration of {name}"))
    }
    
    fn parse_param() -> impl Parser<Token, EPrimeMap, Error = Simple<Token>> {
        let letting = letting_prefix_parser().then(parse_val());
    
        lang_decl_parser()
            .ignore_then(letting.repeated())
            .then_ignore(end())
            .validate(|lettings, span, emit| {
                let mut map = EPrimeMap::new();
                for (name, dom) in lettings {
                    if map.insert(name.clone(), dom).is_some() {
                        emit(duplicate_decl(span.clone(), name));
                    }
                }
                map
            })
            .boxed()
    }
}
