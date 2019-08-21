open Jest;
open Expect;
open Relude.Globals;

describe("NanpPhone", () => {
  let parse = str => NanpPhone.parse(str) |> Result.map(NanpPhone.toDigits);
  let tuple3334445555 = (3, 3, 3, 4, 4, 4, 5, 5, 5, 5);

  test("xxx-yyy-zzzz (separated by hyphens, succeeds)", () =>
    expect(parse("333-444-5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("(xxx) yyy-zzzz (area code parens, succeeds)", () =>
    expect(parse("(333) 444-5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("333.444.5555 (dots as separator, succeeds)", () =>
    expect(parse("333.444.5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("333 444 5555 (space as separator, succeeds)", () =>
    expect(parse("333 444 5555")) |> toEqual(Result.ok(tuple3334445555))
  );

  test("3334445555 (no separator, succeeds)", () =>
    expect(parse("3334445555")) |> toEqual(Result.ok(tuple3334445555))
  );

  // TODO: optional country code (maybe prefixed with +)

  test("empty string (fails)", () =>
    expect(NanpPhone.parse("") |> Result.isError) |> toEqual(true)
  );

  // TODO: invalid characters, e.g. exchange 911, area code starts with 1
  // TODO: country code present, not 1
  // TODO: too many characters

  test("too few characters (fails)", () =>
    expect(NanpPhone.parse("333444") |> Result.isError) |> toEqual(true)
  );
});
