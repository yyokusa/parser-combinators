use std::fmt::Debug;
use std::cmp::PartialEq;
use std::cmp::Eq;

type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

/// implement this trait for any function that matches 
/// the signature of a parser
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// This function takes a string slice as input and returns
/// a tuple of the remaining string slice and the matched string slice.
fn match_literal<'a>(expected: &'static str) 
-> impl Parser<'a, ()>
{
    move |input: &'a str| match input.find(expected) {
        Some(0) => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// This function takes a string slice as input and returns
///  a tuple of the remaining string slice and the matched string slice. 
/// If the input string slice does not start with an alphabetic character
/// it returns an error.
#[allow(dead_code)]
fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

/// This function takes two parsers as input and returns a new parser
/// that runs the first parser and then the second on the remaining input.
/// If both parsers succeed, it returns a tuple of the remaining input
/// and the two parsed values. If either parser fails, it returns an error.
#[allow(dead_code)]
fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) 
-> impl Parser<'a, (R1, R2)> 
where 
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2.parse(next_input)
                .map(|(final_input, result2)| (final_input, (result1, result2)))
        })
    }
}

/// Functor pattern from Haskell/Category Theory
/// Option, Result, Iterator, Future in Rust are all functors
fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| 
        parser.parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
}

// Left and Right

fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

#[cfg(test)]
mod tests {
    use crate::Parser;
    use crate::left;
    use crate::match_literal;
    use crate::identifier;
    use crate::pair;
    use crate::right;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            Ok(("", ())),
            parse_joe.parse("Hello Joe!")
        );
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe.parse("Hello Joe! Hello Robert!")
        );
        assert_eq!(
            Err("Hello Mike!"),
            parse_joe.parse("Hello Mike!")
        );
    }

    #[test]
    fn identifier_parser() {
        assert_eq!(
            Ok(("", "i-am-an-identifier".to_string())),
            identifier("i-am-an-identifier")
        );
        assert_eq!(
            Ok((" entirely an identifier", "not".to_string())),
            identifier("not entirely an identifier")
        );
        assert_eq!(
            Err("!not at all an identifier"),
            identifier("!not at all an identifier")
        );
    }

    #[test]
    fn pair_combinator() {
        let tag_opener = pair(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", ((), "my-first-element".to_string()))),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn right_combinator() {
        let tag_opener = right(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", "my-first-element".to_string())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }

    #[test]
    fn left_combinator() {
        let tag_opener = left(match_literal("<"), identifier);
        assert_eq!(
            Ok(("/>", ())),
            tag_opener.parse("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener.parse("oops"));
        assert_eq!(Err("!oops"), tag_opener.parse("<!oops"));
    }
}






// Parser as a function type
// Fn(Input) -> Result<(Input, Output), Error>
// TODO: currently we return the bit of the string 
// that we couldn't parse as an error, will improve!!!
// TODO: cleaner to use ASCII and slice of bytes instead of str slices
// Fn(&str) -> Result<(&str, Element), &str>