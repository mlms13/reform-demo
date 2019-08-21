module Parser = ReludeParse_Parser;

// private helper types that have their own parsers
module AreaCode = {
  type t =
    | AreaCode(int, int, int);

  let parser =
    Parser.(
      (
        opt(str("(")),
        filter(v => v != 1, anyDigitAsInt),
        times2(anyDigitAsInt),
        opt(str(")")),
      )
      |> mapTuple4((_, a, (b, c), _) => AreaCode(a, b, c))
    );
};

type exchange = (int, int, int);
type line = (int, int, int, int);

// A phone number, following North American Numbering Plan conventions (used by
// the US, Canada, and others). No country code is specified because all NANP
// countries begin with 1 (often implied without being specified).
type t =
  | NanpPhone(AreaCode.t, exchange, line);

let unsafeMake = (areaCode, (d, e, f), (g, h, i, j)) =>
  NanpPhone(areaCode, (d, e, f), (g, h, i, j));

let toDigits = (NanpPhone(AreaCode(a, b, c), (d, e, f), (g, h, i, j))) => (
  a,
  b,
  c,
  d,
  e,
  f,
  g,
  h,
  i,
  j,
);

let sep = Parser.anyOfStr(["-", "."]);

let parser: Parser.t(t) =
  Parser.(
    unsafeMake
    <$> ws
    *> opt(str("+") *> str("1") <|> str("1"))
    *> ws
    *> AreaCode.parser
    <* opt(sep)
    <* ws
    <*> times3(anyDigitAsInt)  // TODO can't start with 1 or end with a pair of 1s
    <* opt(sep)
    <* ws
    <*> times4(anyDigitAsInt)
    <* ws
    <* eof
  );

let parse = str => Parser.runParser(str, parser);
