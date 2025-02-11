import List "../src/List";

import Suite "mo:matchers/Suite";
import T "mo:matchers/Testable";
import M "mo:matchers/Matchers";

import Prim "mo:⛔";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Nat32 "mo:base/Nat32";
import Nat "mo:base/Nat";
import Order "mo:base/Order";

let { run; test; suite } = Suite;

func unwrap<T>(x : ?T) : T = switch (x) {
  case (?v) v;
  case (_) Prim.trap "internal error in unwrap()";
};

let n = 100;
var list = List.new<Nat>();

let sizes = Buffer.Buffer<Nat>(0);
for (i in Iter.range(0, n)) {
  sizes.add(List.size(list));
  List.add(list, i);
};
sizes.add(List.size(list));

class OrderTestable(initItem : Order.Order) : T.TestableItem<Order.Order> {
  public let item = initItem;
  public func display(order : Order.Order) : Text {
    switch (order) {
      case (#less) {
        "#less";
      };
      case (#greater) {
        "#greater";
      };
      case (#equal) {
        "#equal";
      };
    };
  };
  public let equals = Order.equal;
};

run(
  suite(
    "clone",
    [
      test(
        "clone",
        List.toArray(List.clone(list)),
        M.equals(T.array(T.natTestable, List.toArray(list))),
      ),
    ],
  )
);

run(
  suite(
    "add",
    [
      test(
        "sizes",
        Buffer.toArray(sizes),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n + 1)))),
      ),
      test(
        "elements",
        List.toArray(list),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
    ],
  )
);

assert List.indexOf(list, Nat.equal, n + 1) == null;
assert List.firstIndexWith(list, func(a : Nat) : Bool = a == n + 1) == null;
assert List.indexOf(list, Nat.equal, n) == ?n;
assert List.firstIndexWith(list, func(a : Nat) : Bool = a == n) == ?n;

assert List.lastIndexOf(list, Nat.equal, n + 1) == null;
assert List.lastIndexWith(list, func(a : Nat) : Bool = a == n + 1) == null;

assert List.lastIndexOf(list, Nat.equal, 0) == ?0;
assert List.lastIndexWith(list, func(a : Nat) : Bool = a == 0) == ?0;

assert List.forAll(list, func(x : Nat) : Bool = 0 <= x and x <= n);
assert List.forNone(list, func(x : Nat) : Bool = x == n + 1);
assert List.forSome(list, func(x : Nat) : Bool = x == n / 2);

run(
  suite(
    "iterator",
    [
      test(
        "elements",
        Iter.toArray(List.vals(list)),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
      test(
        "revElements",
        Iter.toArray(List.valsRev(list)),
        M.equals(T.array(T.intTestable, Iter.toArray(Iter.revRange(n, 0)))),
      ),
      test(
        "keys",
        Iter.toArray(List.keys(list)),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
      test(
        "items1",
        Iter.toArray(Iter.map<(Nat, Nat), Nat>(List.items(list), func((a, b)) { a })),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
      test(
        "items2",
        Iter.toArray(Iter.map<(Nat, Nat), Nat>(List.items(list), func((a, b)) { b })),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
      test(
        "itemsRev1",
        Iter.toArray(Iter.map<(Nat, Nat), Nat>(List.itemsRev(list), func((a, b)) { a })),
        M.equals(T.array(T.intTestable, Iter.toArray(Iter.revRange(n, 0)))),
      ),
      test(
        "itemsRev2",
        Iter.toArray(Iter.map<(Nat, Nat), Nat>(List.itemsRev(list), func((a, b)) { b })),
        M.equals(T.array(T.intTestable, Iter.toArray(Iter.revRange(n, 0)))),
      ),
    ],
  )
);

let for_add_many = List.init<Nat>(n, 0);
List.addMany(for_add_many, n, 0);

let for_add_iter = List.init<Nat>(n, 0);
List.addFromIter(for_add_iter, Array.init<Nat>(n, 0).vals());

run(
  suite(
    "init",
    [
      test(
        "init with toArray",
        List.toArray(List.init<Nat>(n, 0)),
        M.equals(T.array(T.natTestable, Array.tabulate<Nat>(n, func(_) = 0))),
      ),
      test(
        "init with vals",
        Iter.toArray(List.vals(List.init<Nat>(n, 0))),
        M.equals(T.array(T.natTestable, Array.tabulate<Nat>(n, func(_) = 0))),
      ),
      test(
        "add many with toArray",
        List.toArray(for_add_many),
        M.equals(T.array(T.natTestable, Array.tabulate<Nat>(2 * n, func(_) = 0))),
      ),
      test(
        "add many with vals",
        Iter.toArray(List.vals(for_add_many)),
        M.equals(T.array(T.natTestable, Array.tabulate<Nat>(2 * n, func(_) = 0))),
      ),
      test(
        "addFromIter",
        List.toArray(for_add_iter),
        M.equals(T.array(T.natTestable, Array.tabulate<Nat>(2 * n, func(_) = 0))),
      ),
    ],
  )
);

for (i in Iter.range(0, n)) {
  List.put(list, i, n - i : Nat);
};

run(
  suite(
    "put",
    [
      test(
        "size",
        List.size(list),
        M.equals(T.nat(n + 1)),
      ),
      test(
        "elements",
        List.toArray(list),
        M.equals(T.array(T.intTestable, Iter.toArray(Iter.revRange(n, 0)))),
      ),
    ],
  )
);

let removed = Buffer.Buffer<Nat>(0);
for (i in Iter.range(0, n)) {
  removed.add(unwrap(List.removeLast(list)));
};

run(
  suite(
    "removeLast",
    [
      test(
        "size",
        List.size(list),
        M.equals(T.nat(0)),
      ),
      test(
        "elements",
        Buffer.toArray(removed),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
    ],
  )
);

for (i in Iter.range(0, n)) {
  List.add(list, i);
};

run(
  suite(
    "addAfterRemove",
    [
      test(
        "elements",
        List.toArray(list),
        M.equals(T.array(T.natTestable, Iter.toArray(Iter.range(0, n)))),
      ),
    ],
  )
);

run(
  suite(
    "firstAndLast",
    [
      test(
        "first",
        [List.first(list)],
        M.equals(T.array(T.natTestable, [0])),
      ),
      test(
        "last of len N",
        [List.last(list)],
        M.equals(T.array(T.natTestable, [n])),
      ),
      test(
        "last of len 1",
        [List.last(List.init<Nat>(1, 1))],
        M.equals(T.array(T.natTestable, [1])),
      ),
    ],
  )
);

var sumN = 0;
List.iterate<Nat>(list, func(i) { sumN += i });
var sumRev = 0;
List.iterateRev<Nat>(list, func(i) { sumRev += i });
var sum1 = 0;
List.iterate<Nat>(List.init<Nat>(1, 1), func(i) { sum1 += i });
var sum0 = 0;
List.iterate<Nat>(List.new<Nat>(), func(i) { sum0 += i });

run(
  suite(
    "iterate",
    [
      test(
        "sumN",
        [sumN],
        M.equals(T.array(T.natTestable, [n * (n + 1) / 2])),
      ),
      test(
        "sumRev",
        [sumRev],
        M.equals(T.array(T.natTestable, [n * (n + 1) / 2])),
      ),
      test(
        "sum1",
        [sum1],
        M.equals(T.array(T.natTestable, [1])),
      ),
      test(
        "sum0",
        [sum0],
        M.equals(T.array(T.natTestable, [0])),
      ),
    ],
  )
);

/* --------------------------------------- */

var sumItems = 0;
List.iterateItems<Nat>(list, func(i, x) { sumItems += i + x });
var sumItemsRev = 0;
List.iterateItems<Nat>(list, func(i, x) { sumItemsRev += i + x });

run(
  suite(
    "iterateItems",
    [
      test(
        "sumItems",
        [sumItems],
        M.equals(T.array(T.natTestable, [n * (n + 1)])),
      ),
      test(
        "sumItemsRev",
        [sumItemsRev],
        M.equals(T.array(T.natTestable, [n * (n + 1)])),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5]);

run(
  suite(
    "contains",
    [
      test(
        "true",
        List.contains<Nat>(list, Nat.equal, 2),
        M.equals(T.bool(true)),
      ),
      test(
        "true",
        List.contains<Nat>(list, Nat.equal, 9),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.new<Nat>();

run(
  suite(
    "contains empty",
    [
      test(
        "true",
        List.contains<Nat>(list, Nat.equal, 2),
        M.equals(T.bool(false)),
      ),
      test(
        "true",
        List.contains<Nat>(list, Nat.equal, 9),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([2, 1, 10, 1, 0, 3]);

run(
  suite(
    "max",
    [
      test(
        "return value",
        List.max<Nat>(list, Nat.compare),
        M.equals(T.optional(T.natTestable, ?10)),
      )
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([2, 1, 10, 1, 0, 3, 0]);

run(
  suite(
    "min",
    [
      test(
        "return value",
        List.min<Nat>(list, Nat.compare),
        M.equals(T.optional(T.natTestable, ?0)),
      )
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5]);

var list2 = List.fromArray<Nat>([0, 1, 2]);

run(
  suite(
    "equal",
    [
      test(
        "empty lists",
        List.equal<Nat>(List.new<Nat>(), List.new<Nat>(), Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "non-empty lists",
        List.equal<Nat>(list, List.clone(list), Nat.equal),
        M.equals(T.bool(true)),
      ),
      test(
        "non-empty and empty lists",
        List.equal<Nat>(list, List.new<Nat>(), Nat.equal),
        M.equals(T.bool(false)),
      ),
      test(
        "non-empty lists mismatching lengths",
        List.equal<Nat>(list, list2, Nat.equal),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5]);
list2 := List.fromArray<Nat>([0, 1, 2]);

var list3 = List.fromArray<Nat>([2, 3, 4, 5]);

run(
  suite(
    "compare",
    [
      test(
        "empty lists",
        List.compare<Nat>(List.new<Nat>(), List.new<Nat>(), Nat.compare),
        M.equals(OrderTestable(#equal)),
      ),
      test(
        "non-empty lists equal",
        List.compare<Nat>(list, List.clone(list), Nat.compare),
        M.equals(OrderTestable(#equal)),
      ),
      test(
        "non-empty and empty lists",
        List.compare<Nat>(list, List.new<Nat>(), Nat.compare),
        M.equals(OrderTestable(#greater)),
      ),
      test(
        "non-empty lists mismatching lengths",
        List.compare<Nat>(list, list2, Nat.compare),
        M.equals(OrderTestable(#greater)),
      ),
      test(
        "non-empty lists lexicographic difference",
        List.compare<Nat>(list, list3, Nat.compare),
        M.equals(OrderTestable(#less)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5]);

run(
  suite(
    "toText",
    [
      test(
        "empty list",
        List.toText<Nat>(List.new<Nat>(), Nat.toText),
        M.equals(T.text("[]")),
      ),
      test(
        "singleton list",
        List.toText<Nat>(List.make<Nat>(3), Nat.toText),
        M.equals(T.text("[3]")),
      ),
      test(
        "non-empty list",
        List.toText<Nat>(list, Nat.toText),
        M.equals(T.text("[0, 1, 2, 3, 4, 5]")),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6, 7]);
list2 := List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6]);
list3 := List.new<Nat>();

var list4 = List.make<Nat>(3);

List.reverse<Nat>(list);
List.reverse<Nat>(list2);
List.reverse<Nat>(list3);
List.reverse<Nat>(list4);

run(
  suite(
    "reverse",
    [
      test(
        "even elements",
        List.toArray(list),
        M.equals(T.array(T.natTestable, [7, 6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "odd elements",
        List.toArray(list2),
        M.equals(T.array(T.natTestable, [6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "empty",
        List.toArray(list3),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "singleton",
        List.toArray(list4),
        M.equals(T.array(T.natTestable, [3])),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.reversed<Nat>(List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6, 7]));
list2 := List.reversed<Nat>(List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6]));
list3 := List.reversed<Nat>(List.new<Nat>());
list4 := List.reversed<Nat>(List.make<Nat>(3));

run(
  suite(
    "reversed",
    [
      test(
        "even elements",
        List.toArray(list),
        M.equals(T.array(T.natTestable, [7, 6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "odd elements",
        List.toArray(list2),
        M.equals(T.array(T.natTestable, [6, 5, 4, 3, 2, 1, 0])),
      ),
      test(
        "empty",
        List.toArray(list3),
        M.equals(T.array(T.natTestable, [] : [Nat])),
      ),
      test(
        "singleton",
        List.toArray(list4),
        M.equals(T.array(T.natTestable, [3])),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6]);

run(
  suite(
    "foldLeft",
    [
      test(
        "return value",
        List.foldLeft<Text, Nat>(list, "", func(acc, x) = acc # Nat.toText(x)),
        M.equals(T.text("0123456")),
      ),
      test(
        "return value empty",
        List.foldLeft<Text, Nat>(List.new<Nat>(), "", func(acc, x) = acc # Nat.toText(x)),
        M.equals(T.text("")),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6]);

run(
  suite(
    "foldRight",
    [
      test(
        "return value",
        List.foldRight<Nat, Text>(list, "", func(x, acc) = acc # Nat.toText(x)),
        M.equals(T.text("6543210")),
      ),
      test(
        "return value empty",
        List.foldRight<Nat, Text>(List.new<Nat>(), "", func(x, acc) = acc # Nat.toText(x)),
        M.equals(T.text("")),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.make<Nat>(2);

run(
  suite(
    "isEmpty",
    [
      test(
        "true",
        List.isEmpty(List.new<Nat>()),
        M.equals(T.bool(true)),
      ),
      test(
        "false",
        List.isEmpty(list),
        M.equals(T.bool(false)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([0, 1, 2, 3, 4, 5, 6]);

run(
  suite(
    "map",
    [
      test(
        "map",
        List.toArray(List.map<Nat, Text>(list, Nat.toText)),
        M.equals(T.array(T.textTestable, ["0", "1", "2", "3", "4", "5", "6"])),
      ),
      test(
        "empty",
        List.isEmpty(List.map<Nat, Text>(List.new<Nat>(), Nat.toText)),
        M.equals(T.bool(true)),
      ),
    ],
  )
);

/* --------------------------------------- */

list := List.fromArray<Nat>([8, 6, 9, 10, 0, 4, 2, 3, 7, 1, 5]);

run(
  suite(
    "sort",
    [
      test(
        "sort",
        List.sort<Nat>(list, Nat.compare) |> List.toArray(list),
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] |> M.equals(T.array(T.natTestable, _)),
      ),
    ],
  )
);

/* --------------------------------------- */

func locate_readable<X>(index : Nat) : (Nat, Nat) {
  // index is any Nat32 except for
  // blocks before super block s == 2 ** s
  let i = Nat32.fromNat(index);
  // element with index 0 located in data block with index 1
  if (i == 0) {
    return (1, 0);
  };
  let lz = Nat32.bitcountLeadingZero(i);
  // super block s = bit length - 1 = (32 - leading zeros) - 1
  // i in binary = zeroes; 1; bits blocks mask; bits element mask
  // bit lengths =     lz; 1;     floor(s / 2);       ceil(s / 2)
  let s = 31 - lz;
  // floor(s / 2)
  let down = s >> 1;
  // ceil(s / 2) = floor((s + 1) / 2)
  let up = (s + 1) >> 1;
  // element mask = ceil(s / 2) ones in binary
  let e_mask = 1 << up - 1;
  //block mask = floor(s / 2) ones in binary
  let b_mask = 1 << down - 1;
  // data blocks in even super blocks before current = 2 ** ceil(s / 2)
  // data blocks in odd super blocks before current = 2 ** floor(s / 2)
  // data blocks before the super block = element mask + block mask
  // elements before the super block = 2 ** s
  // first floor(s / 2) bits in index after the highest bit = index of data block in super block
  // the next ceil(s / 2) to the end of binary representation of index + 1 = index of element in data block
  (Nat32.toNat(e_mask + b_mask + 2 + (i >> up) & b_mask), Nat32.toNat(i & e_mask));
};

// this was optimized in terms of instructions
func locate_optimal<X>(index : Nat) : (Nat, Nat) {
  // super block s = bit length - 1 = (32 - leading zeros) - 1
  // blocks before super block s == 2 ** s
  let i = Nat32.fromNat(index);
  let lz = Nat32.bitcountLeadingZero(i);
  let lz2 = lz >> 1;
  // we split into cases to apply different optimizations in each one
  if (lz & 1 == 0) {
    // ceil(s / 2)  = 16 - lz2
    // floor(s / 2) = 15 - lz2
    // i in binary = zeroes; 1; bits blocks mask; bits element mask
    // bit lengths =     lz; 1;         15 - lz2;          16 - lz2
    // blocks before = 2 ** ceil(s / 2) + 2 ** floor(s / 2)

    // so in order to calculate index of the data block
    // we need to shift i by 16 - lz2 and set bit with number 16 - lz2, bit 15 - lz2 is already set

    // element mask = 2 ** (16 - lz2) = (1 << 16) >> lz2 = 0xFFFF >> lz2
    let mask = 0xFFFF >> lz2;
    (Nat32.toNat(((i << lz2) >> 16) ^ (0x10000 >> lz2)), Nat32.toNat(i & mask));
  } else {
    // s / 2 = ceil(s / 2) = floor(s / 2) = 15 - lz2
    // i in binary = zeroes; 1; bits blocks mask; bits element mask
    // bit lengths =     lz; 1;         15 - lz2;          15 - lz2
    // block mask = element mask = mask = 2 ** (s / 2) - 1 = 2 ** (15 - lz2) - 1 = (1 << 15) >> lz2 = 0x7FFF >> lz2
    // blocks before = 2 * 2 ** (s / 2)

    // so in order to calculate index of the data block
    // we need to shift i by 15 - lz2, set bit with number 16 - lz2 and unset bit 15 - lz2

    let mask = 0x7FFF >> lz2;
    (Nat32.toNat(((i << lz2) >> 15) ^ (0x18000 >> lz2)), Nat32.toNat(i & mask));
  };
};

let locate_n = 1_000;
var i = 0;
while (i < locate_n) {
  assert (locate_readable(i) == locate_optimal(i));
  assert (locate_readable(1_000_000 + i) == locate_optimal(1_000_000 + i));
  assert (locate_readable(1_000_000_000 + i) == locate_optimal(1_000_000_000 + i));
  assert (locate_readable(2_000_000_000 + i) == locate_optimal(2_000_000_000 + i));
  assert (locate_readable(2 ** 32 - 1 - i : Nat) == locate_optimal(2 ** 32 - 1 - i : Nat));
  i += 1;
};