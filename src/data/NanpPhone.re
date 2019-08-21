module Parser = ReludeParse_Parser;

type areaCode = (int, int, int);
type exchange = (int, int, int);
type line = (int, int, int, int);

// A phone number, following North American Numbering Plan conventions (used by
// the US, Canada, and others). No country code is specified because all NANP
// countries begin with 1 (often implied without being specified).
type t =
  | NanpPhone(areaCode, exchange, line);

let unsafeMake = ((a, b, c), (d, e, f), (g, h, i, j)) =>
  NanpPhone((a, b, c), (d, e, f), (g, h, i, j));

let openParen = Parser.str("(");
let closeParen = Parser.str("(");
let hyphen = Parser.str("-");

let parser: Parser.t(t) =
  Parser.(
    unsafeMake
    <$> times3(anyDigitAsInt)  // TODO: can't start with 1
    <* hyphen
    <*> times3(anyDigitAsInt)  // TODO can't start with 1 or end with a pair of 1s
    <* hyphen
    <*> times4(anyDigitAsInt)
  );

let parse = str => Parser.runParser(str, parser);
