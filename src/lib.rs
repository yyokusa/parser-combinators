use std::fmt::Debug;
use std::cmp::PartialEq;
use std::cmp::Eq;

#[derive(Clone, Debug, PartialEq, Eq)]
struct Element {
    name: String,
    attributes: Vec<(String, String)>,
    children: Vec<Element>,
}

pub fn match_literal(expected: &'static str) 
-> impl Fn(&str) -> Result<(&str, ()), &str>
{
    move |input| match input.find(expected) {
        Some(0) => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

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



#[cfg(test)]
mod tests {
    use crate::match_literal;
    use crate::identifier;
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
}






// Parser as a function type
// Fn(Input) -> Result<(Input, Output), Error>
// TODO: currently we return the bit of the string 
// that we couldn't parse as an error, will improve!!!
// TODO: cleaner to use ASCII and slice of bytes instead of str slices
// Fn(&str) -> Result<(&str, Element), &str>