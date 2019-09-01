/// Helper typedef.
type ParseResult<'a, Output> = Result<(&'a str, Output), &'a str>;

/// Parser trait abstracts the signature of a parser combinator.
pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

/// Blanket implementation of Parser trait for all function that
/// resemble the type Fn(&str) -> ParseResult<Output>.
impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(&'a str) -> ParseResult<Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

/// Matches a single character.
pub fn is_char(ch: char, input: &str) -> ParseResult<()> {
    match input.chars().next() {
        Some(x) if x == ch => Ok((&input['a'.len_utf8()..], ())),
        _ => Err(input),
    }
}

/// Matches a string literal.
pub fn is_literal<'a>(expected: &'a str) -> impl Parser<'a, ()> {
    move |input: &'a str| match input.get(0..expected.len()) {
        Some(value) if expected == value => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

/// Matches an indentifier.
pub fn identifier(input: &str) -> ParseResult<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(v) if v.is_alphabetic() => matched.push(v),
        _ => return Err(input),
    }

    for v in chars {
        if v.is_alphanumeric() || v == '-' {
            matched.push(v);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}

/// pair is a combinator that combines two parsers P1 and P2 serially.
pub fn pair<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input| {
        parser1.parse(input).and_then(|(next_input, result1)| {
            parser2
                .parse(next_input)
                .map(|(remaining_input, result2)| (remaining_input, (result1, result2)))
        })
    }
}

/// Functor that that can be used to transform the parser's result.
pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

/// Helper functor that ignores the right part of the result. This is particularly
/// useful for stuff like `is_char` or `is_literal` combinators that return () in
/// case of success.
pub fn left<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R1>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(left, _right)| left)
}

/// Helper functor that ignores the left part of the result. This is particularly
/// useful for stuff like `is_char` or `is_literal` combinators that return () in
/// case of success.
pub fn right<'a, P1, P2, R1, R2>(parser1: P1, parser2: P2) -> impl Parser<'a, R2>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    map(pair(parser1, parser2), |(_left, right)| right)
}

/// Consumes one or more tokens.
pub fn one_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first)) = parser.parse(input) {
            input = next_input;
            result.push(first);
        } else {
            return Err(input);
        }

        while let Ok((next_input, item)) = parser.parse(input) {
            input = next_input;
            result.push(item);
        }
        Ok((input, result))
    }
}

/// Consumes zero or more.
pub fn zero_or_more<'a, P, A>(parser: P) -> impl Parser<'a, Vec<A>>
where
    P: Parser<'a, A>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, item)) = parser.parse(input) {
            input = next_input;
            result.push(item);
        }

        Ok((input, result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_char() {
        assert_eq!(is_char('a', "abc"), Ok(("bc", ())));
        assert_eq!(is_char('a', "xbc"), Err("xbc"));
    }

    #[test]
    fn test_is_literal() {
        let match_umer = is_literal("Howdy, Umer!");

        assert_eq!(Ok(("", ())), match_umer.parse("Howdy, Umer!"));
        assert_eq!(Ok((" hmm", ())), match_umer.parse("Howdy, Umer! hmm"));
        assert_eq!(Err("Howdy, Azad!"), match_umer.parse("Howdy, Azad!"));
    }

    #[test]
    fn test_identifier() {
        assert_eq!(identifier("some-thing"), Ok(("", "some-thing".to_string())));
        assert_eq!(identifier("a-b-c-1-d"), Ok(("", "a-b-c-1-d".to_string())));
        assert_eq!(
            identifier("some123-thing"),
            Ok(("", "some123-thing".to_string()))
        );
        assert_eq!(identifier("-thing"), Err("-thing"));
        assert_eq!(identifier("thing- abc"), Ok((" abc", "thing-".to_string())));
    }

    #[test]
    fn test_pair_combinator() {
        let tag_opener = right(is_literal("<"), identifier);

        assert_eq!(
            tag_opener.parse("<first-element/>"),
            Ok(("/>", "first-element".to_string()))
        );

        assert_eq!(tag_opener.parse("<!oops"), Err("!oops"));
    }

    #[test]
    fn test_one_or_more() {
        let parser = one_or_more(is_literal("nom"));
        assert_eq!(parser.parse("nomnom"), Ok(("", vec![(), ()])));
        assert_eq!(parser.parse("monmon"), Err("monmon"));
        assert_eq!(parser.parse(""), Err(""));
    }

    #[test]
    fn test_zero_or_more() {
        let parser = zero_or_more(is_literal("nom"));
        assert_eq!(parser.parse("nomnom"), Ok(("", vec![(), ()])));
        assert_eq!(parser.parse("monmon"), Ok(("monmon", vec![])));
        assert_eq!(parser.parse(""), Ok(("", vec![])));
    }
}
