// @testmode wasi

import Suite "mo:matchers/Suite";
import T "mo:matchers/Testable";
import M "mo:matchers/Matchers";
import Map "../src/Map";
import Iter "../src/Iter";
import Nat "../src/Nat";
import Runtime "../src/Runtime";
import Debug "../src/Debug";

let { run; test; suite } = Suite;

run(
  suite(
    "empty",
    [
      test(
        "dummy",
        0,
        M.equals(T.nat(0))
      )
    ]
  )
);

// TODO: Use PRNG in new base library
class Random(seed : Nat) {
  var number = seed;

  public func reset() {
    number := seed
  };

  public func next() : Nat {
    number := (123138118391 * number + 133489131) % 9999;
    number
  }
};

let randomSeed = 4711;
let numberOfEntries = 10_000;

run(
  suite(
    "large map",
    [
      test(
        "add",
        do {
          let map = Map.empty<Nat, Text>();
          for (index in Iter.natRange(0, numberOfEntries)) {
            Map.add(map, Nat.compare, index, debug_show (index));
            assert (Map.size(map) == index + 1);
            assert (Map.get(map, Nat.compare, index) == debug_show (index))
          };
          for (index in Iter.natRange(0, numberOfEntries)) {
            assert (Map.get(map, Nat.compare, index) == debug_show (index))
          };
          assert (Map.get(map, Nat.compare, numberOfEntries) == null);
          Map.assertValid(map, Nat.compare);
          Map.size(map)
        },
        M.equals(T.nat(numberOfEntries))
      ),
      test(
        "get",
        do {
          let map = Map.empty<Nat, Text>();
          let random = Random(randomSeed);
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            ignore Map.put(map, Nat.compare, key, debug_show (key))
          };
          random.reset();
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            assert (Map.get(map, Nat.compare, key) == debug_show (key))
          };
          true
        },
        M.equals(T.bool(true))
      ),
      test(
        "update",
        do {
          let map = Map.empty<Nat, Text>();
          let random = Random(randomSeed);
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            ignore Map.put(map, Nat.compare, key, debug_show (key))
          };
          random.reset();
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            assert (Map.containsKey(map, Nat.compare, key));
            let oldValue = Map.put(map, Nat.compare, key, debug_show (key) # "!");
            assert (oldValue != null)
          };
          random.reset();
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            assert (Map.containsKey(map, Nat.compare, key));
            assert (Map.get(map, Nat.compare, key) == debug_show (key) # "!")
          };
          Map.assertValid(map, Nat.compare);
          true
        },
        M.equals(T.bool(true))
      ),
      test(
        "delete",
        do {
          let map = Map.empty<Nat, Text>();
          let random = Random(randomSeed);
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            ignore Map.put(map, Nat.compare, key, debug_show (key))
          };
          random.reset();
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            assert (Map.containsKey(map, Nat.compare, key));
            assert (Map.get(map, Nat.compare, key) == debug_show (key))
          };
          random.reset();
          for (index in Iter.natRange(0, numberOfEntries)) {
            let key = random.next();
            if (Map.containsKey(map, Nat.compare, key)) {
              Map.delete(map, Nat.compare, key);
              assert (not Map.containsKey(map, Nat.compare, key))
            };
            assert (Map.get(map, Nat.compare, key) == null)
          };
          Map.assertValid(map, Nat.compare);
          Map.size(map)
        },
        M.equals(T.nat(0))
      ),
      test(
        "iterate",
        do {
          let map = Map.empty<Nat, Text>();
          for (index in Iter.natRange(0, numberOfEntries)) {
            Map.add(map, Nat.compare, index, debug_show (index))
          };
          var index = 0;
          for ((key, value) in Map.entries(map)) {
            assert (key == index);
            assert (value == debug_show (index));
            index += 1
          };
          index
        },
        M.equals(T.nat(numberOfEntries))
      ),
      test(
        "reverseIterate",
        do {
          let map = Map.empty<Nat, Text>();
          for (index in Iter.natRange(0, numberOfEntries)) {
            Map.add(map, Nat.compare, index, debug_show (index))
          };
          var index = numberOfEntries;
          for ((key, value) in Map.reverseEntries(map)) {
            index -= 1;
            assert (key == index);
            assert (value == debug_show (index))
          };
          index
        },
        M.equals(T.nat(0))
      )
    ]
  )
);
