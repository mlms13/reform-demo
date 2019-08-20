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

let patches: list(offerField) = [
  Field(IsAgent, true),
  Field(Amount, 3.14),
];

let applyPatches = (offer, xs) =>
  Belt.List.reduce(xs, offer, (acc, curr) =>
    switch (curr) {
    | Field(field, v) => acc->OfferForm.set(field, v)
    }
  );

let patchToJSON = (xs: list(offerField)) =>
  Belt.List.reduce(xs, [||], (acc, curr) =>
    switch (curr) {
    | Field(OfferForm.IsAgent, v) =>
      Array.append(acc, [|("isAgent", Js.Json.boolean(v))|])
    | Field(OfferForm.Amount, v) =>
      Array.append(acc, [|("amount", Js.Json.number(v))|])
    | _ => acc
    }
  )
  |> Js.Dict.fromArray
  |> Js.Json.object_;

let updated = applyPatches(offer, patches);
