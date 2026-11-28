import Bench "mo:bench";

import List "../src/List";
import Nat "../src/Nat";
import Runtime "../src/Runtime";

module {
  public func init() : Bench.Bench {
    let bench = Bench.Bench();

    bench.name("List traversals and builders");
    bench.description("Functions that visit every element of a List: iterators, folds, searches that find nothing, conversions and the functions that build a new List. Each cell makes one call that covers a List of the given size, except `add` and `removeLast`, which are called once per element. Rows marked `of a copy` include a `clone`: subtract the `clone` row to get the cost of the operation.");

    bench.rows([
      "values",
      "enumerate",
      "reverseValues",
      "reverseEnumerate",
      "forEach",
      "forEachEntry",
      "reverseForEach",
      "reverseForEachEntry",
      "foldLeft",
      "foldRight",
      "all",
      "any (not found)",
      "find (not found)",
      "findIndex (not found)",
      "findLastIndex (not found)",
      "max",
      "min",
      "isSorted",
      "equal",
      "compare",
      "toText",
      "map",
      "mapInPlace",
      "mapEntries",
      "mapResult",
      "filter (keep half)",
      "filterMap (keep half)",
      "flatMap (one each)",
      "clone",
      "toArray",
      "toVarArray",
      "toPure",
      "fromPure",
      "fromIter",
      "addAll (to empty)",
      "append (to empty)",
      "reverse",
      "flatten (rows of 100)",
      "join (rows of 100)",
      "fill (of a copy)",
      "retain (keep half, of a copy)",
      "deduplicate (pairs, of a copy)",
      "sortInPlace (sorted, of a copy)",
      "add",
      "removeLast (of a copy)"
    ]);
    bench.cols([
      "1_000",
      "100_000"
    ]);

    // inputs are built once, outside the measured runner
    func make(n : Nat) : List.List<Nat> = List.tabulate(n, func i = i);
    let small = make(1_000);
    let large = make(100_000);
    let smallCopy = make(1_000);
    let largeCopy = make(100_000);
    let smallPairs = List.tabulate<Nat>(1_000, func i = i / 2);
    let largePairs = List.tabulate<Nat>(100_000, func i = i / 2);
    let smallPure = small.toPure();
    let largePure = large.toPure();
    func rows(n : Nat) : List.List<List.List<Nat>> = List.tabulate(n / 100, func _ = make(100));
    let smallRows = rows(1_000);
    let largeRows = rows(100_000);

    bench.runner(
      func(row, col) {
        let (list, copy, pairs, pure, nested) = switch col {
          case "1_000" (small, smallCopy, smallPairs, smallPure, smallRows);
          case "100_000" (large, largeCopy, largePairs, largePure, largeRows);
          case _ Runtime.unreachable()
        };
        let n = list.size();
        switch row {
          case "values" for (_ in list.values()) {};
          case "enumerate" for (_ in list.enumerate()) {};
          case "reverseValues" for (_ in list.reverseValues()) {};
          case "reverseEnumerate" for (_ in list.reverseEnumerate()) {};
          case "forEach" list.forEach(func _ = ());
          case "forEachEntry" list.forEachEntry(func(_, _) = ());
          case "reverseForEach" list.reverseForEach(func _ = ());
          case "reverseForEachEntry" list.reverseForEachEntry(func(_, _) = ());
          case "foldLeft" ignore list.foldLeft(0, func(a, x) = a + x);
          case "foldRight" ignore list.foldRight(0, func(x, a) = a + x);
          case "all" ignore list.all(func x = x < n);
          case "any (not found)" ignore list.any(func x = x >= n);
          case "find (not found)" ignore list.find(func x = x >= n);
          case "findIndex (not found)" ignore list.findIndex(func x = x >= n);
          case "findLastIndex (not found)" ignore list.findLastIndex(func x = x >= n);
          case "max" ignore list.max(Nat.compare);
          case "min" ignore list.min(Nat.compare);
          case "isSorted" ignore list.isSorted(Nat.compare);
          case "equal" ignore list.equal(copy, Nat.equal);
          case "compare" ignore list.compare(copy, Nat.compare);
          case "toText" ignore list.toText(Nat.toText);
          case "map" ignore list.map(func x = x + 1);
          case "mapInPlace" copy.mapInPlace(func x = x);
          case "mapEntries" ignore list.mapEntries(func(x, i) = x + i);
          case "mapResult" ignore list.mapResult<Nat, Nat, ()>(func x = #ok x);
          case "filter (keep half)" ignore list.filter(func x = x % 2 == 0);
          case "filterMap (keep half)" ignore list.filterMap(func x = if (x % 2 == 0) ?x else null);
          case "flatMap (one each)" ignore list.flatMap(func x = Nat.range(x, x + 1));
          case "clone" ignore list.clone();
          case "toArray" ignore list.toArray();
          case "toVarArray" ignore list.toVarArray();
          case "toPure" ignore list.toPure();
          case "fromPure" ignore List.fromPure(pure);
          case "fromIter" ignore List.fromIter(Nat.range(0, n));
          case "addAll (to empty)" List.empty<Nat>().addAll(Nat.range(0, n));
          case "append (to empty)" List.empty<Nat>().append(list);
          case "reverse" ignore list.reverse();
          case "flatten (rows of 100)" ignore nested.flatten();
          case "join (rows of 100)" ignore nested.values().join();
          case "fill (of a copy)" list.clone().fill(0);
          case "retain (keep half, of a copy)" list.clone().retain(func x = x % 2 == 0);
          case "deduplicate (pairs, of a copy)" pairs.clone().deduplicate(Nat.equal);
          case "sortInPlace (sorted, of a copy)" list.clone().sortInPlace(Nat.compare);
          case "add" {
            let result = List.empty<Nat>();
            var i = 0;
            while (i < n) { result.add(i); i += 1 }
          };
          case "removeLast (of a copy)" {
            let c = list.clone();
            var i = 0;
            while (i < n) { ignore c.removeLast(); i += 1 }
          };
          case _ Runtime.unreachable()
        }
      }
    );

    bench
  }
}
