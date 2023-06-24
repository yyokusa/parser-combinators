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
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => {
            Ok((&input[expected.len()..], ()))
        }
        _ => Err(input),
    }
}



#[cfg(test)]
mod tests {
    use crate::match_literal;
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
}






// Parser as a function type
// Fn(Input) -> Result<(Input, Output), Error>
// TODO: currently we return the bit of the string 
// that we couldn't parse as an error, will improve!!!
// TODO: cleaner to use ASCII and slice of bytes instead of str slices
// Fn(&str) -> Result<(&str, Element), &str>