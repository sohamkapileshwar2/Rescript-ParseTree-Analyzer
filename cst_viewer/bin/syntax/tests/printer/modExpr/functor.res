module F = (A: X, B: Y) => A 
module F = (@attr1 A: X, @attr2 B: Y) => A 
include (X: Map, Y: Set) => Z

module F = @functorAttr (A: X, B: Y) => A 
include @functorAttr (X: Map, Y: Set) => Z
include @functorAttr (X: Map, Y: Set, Z: SuperLongModuleTypeName, Foo: Baaaaaaaaar) => Z
include @functorAttr (@functorAttr2 X: Map, @attr3 Y: Set) => Z
include @functorAttr (@functorAttr2 X: Map, @attr3 Y: Set) => Zzzzzzzzzzzzzzzzzzz

module Make = (A: X, B: Y) => {let a = A.a + B.b}
// constraint on return type
module Make = (A: X, B: Y) :Set => {let a = A.a + B.b}
include @functorAttr (X: Map, Y: Set, Z: SuperLongModuleTypeName, Foo: Baaaaaaaaar): Set => {let a = A.a + B.b}
// with attributes
module Make = @functorAttr (A: X, B: Y) :Set => {let a = A.a + B.b}
module Make = @functorAttr (A: X, @functorAttr2 B: Y) :Set => {let a = A.a + B.b}


module F = () => Map
module F = @functorAttr () => Map
include () => Map
include @functorAttr () => Map

module Make = (
  Cmp: {
    type t;
    let eq: (t, t) => bool;
  }) : {
 type key = Cmp.t;
 type coll;
 let empty: coll;
 let add: (coll, key) => coll;
} => {
  open Cmp;
  type key = t;
  type coll = list<key>;
  let empty = list{};
  let add = (y: coll, e: key) =>
    if (List.exists(x => eq(x, e), y)) {
      y;
    } else {
      list{e, ...y};
    };
};

module Make = (A: X, B: Y) :((Set, Set) => Set) => {let a = A.a + B.b}
module Make = (SuperLongNaaaaaaaame: X, SuperLongNaaaaaaaame: Y, SuperLongNaaaaaaaame: Z) :((Seeeeeeeeeeeeeeeeeeeeeeeeeeeeet, Seeeeeeeeeeeeeeeeeeeeeeeeeeeeet, Seeeeeeeeeeeeeeeeeeeeeeeeeeeeet) => Set) => {let a = A.a + B.b}

module Make = (H: Hashtbl.HashedType): (S with type data = H.t) => {
  type rec weak_t<'a> = t<'a>
  let weak_create = create
  let emptybucket = weak_create(0)

  type rec data = H.t
}

module Gen1 = (P: Primitive, ()) => {
  type t = P.t
  type internal = P.t
  let inject = t => t
}

module DistinctString = (): StringBased  => {
  type t = string
  let inject = t => t
}
