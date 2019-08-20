open Relude.Globals;

module Person = [%lenses
  type t = {
    firstName: string,
    lastName: string,
  }
];

module OfferForm = [%lenses
  type t = {
    isAgent: bool,
    agent: option(Person.t),
    buyer: Person.t,
    amount: float,
  }
];

let offer: OfferForm.t = {
  isAgent: false,
  agent: None,
  buyer: {
    firstName: "Michael",
    lastName: "Martin",
  },
  amount: 500000.99,
};

type offerField =
  | Field(OfferForm.field('a), 'a): offerField;

let patches = [Field(IsAgent, true), Field(Amount, 3.14)];

let applyPatches =
  List.foldLeft((acc, Field(field, v)) => OfferForm.set(acc, field, v));

let patchToJSON =
  List.foldLeft(
    acc =>
      fun
      | Field(OfferForm.IsAgent, v) =>
        Array.append(("isAgent", Js.Json.boolean(v)), acc)
      | Field(OfferForm.Amount, v) =>
        Array.append(("amount", Js.Json.number(v)), acc)
      | _ => acc,
    [||],
  )
  >> Js.Dict.fromArray
  >> Js.Json.object_;

let updated = applyPatches(offer, patches);
let json = patchToJSON(patches);
