use std::fmt::Debug;
use std::cmp::PartialEq;
use std::cmp::Eq;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

/// This function takes a string slice as input and returns
/// a tuple of the remaining string slice and the matched string slice.
pub fn match_literal(expected: &'static str) 
-> impl Fn(&str) -> Result<(&str, ()), &str>
{
    move |input| match input.find(expected) {
        Some(0) => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// This function takes a string slice as input and returns
///  a tuple of the remaining string slice and the matched string slice. 
/// If the input string slice does not start with an alphabetic character
/// it returns an error.
#[allow(dead_code)]
fn identifier(input: &str) -> Result<(&str, String), &str> {
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
fn pair<P1, P2, R1, R2>(parser1: P1, parser2: P2) 
-> impl Fn(&str) -> Result<(&str, (R1, R2)), &str> 
where 
    P1: Fn(&str) -> Result<(&str, R1), &str>,
    P2: Fn(&str) -> Result<(&str, R2), &str>,
{
    move |input| match parser1(input) {
        Ok((next_input, result1)) => match parser2(next_input) {
            Ok((final_input, result2)) => Ok((final_input, (result1, result2))),
            Err(err) => Err(err),
        },
        Err(err) => Err(err),
    }
}



#[cfg(test)]
mod tests {
    use crate::match_literal;
    use crate::identifier;
    use crate::pair;

    #[test]
    fn literal_parser() {
        let parse_joe = match_literal("Hello Joe!");
        assert_eq!(
            Ok(("", ())),
            parse_joe("Hello Joe!")
        );
        assert_eq!(
            Ok((" Hello Robert!", ())),
            parse_joe("Hello Joe! Hello Robert!")
        );
        assert_eq!(
            Err("Hello Mike!"),
            parse_joe("Hello Mike!")
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
            tag_opener("<my-first-element/>")
        );
        assert_eq!(Err("oops"), tag_opener("oops"));
        assert_eq!(Err("!oops"), tag_opener("<!oops"));
    }
}






// Parser as a function type
// Fn(Input) -> Result<(Input, Output), Error>
// TODO: currently we return the bit of the string 
// that we couldn't parse as an error, will improve!!!
// TODO: cleaner to use ASCII and slice of bytes instead of str slices
// Fn(&str) -> Result<(&str, Element), &str>