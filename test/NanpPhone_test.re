open Jest;
open Expect;
open Relude.Globals;

describe("NanpPhone", () => {
  test("parses abc-def-ghij format", () =>
    expect(NanpPhone.parse("333-444-5555"))
    |> toEqual(
         Result.ok(
           NanpPhone.NanpPhone((3, 3, 3), (4, 4, 4), (5, 5, 5, 5)),
         ),
       )
  );

  test("parser fails on empty string", () =>
    expect(NanpPhone.parse("") |> Result.isError) |> toEqual(true)
  );
});
