import Bench "mo:bench";

import List "../src/List";
import Nat "../src/Nat";
import Runtime "../src/Runtime";

module {
  public func init() : Bench.Bench {
    let bench = Bench.Bench();

    bench.name("List random access and positioned traversal");
    bench.description("Hot paths that locate an index in the List: `at`, `get`, `put`, `size`, and traversals that start at an index. Each point operation (`at`, `get`, `put`, `size`) is called once per element of a List of the given size; the other rows make one call that covers the whole List. The `loop` row is the bare loop for reference: subtract it to get the cost of the operations.");

    bench.rows([
      "loop",
      "at",
      "get",
      "get (out of range)",
      "put",
      "size",
      "range (iterate all)",
      "reader (read all)",
      "sliceToArray (all)",
      "sliceToVarArray (all)",
      "forEachInRange (all)",
      "nextIndexOf (not found)",
      "prevIndexOf (not found)",
      "reverseInPlace",
      "tabulate",
      "fromArray",
      "fromVarArray",
      "repeat",
      "addRepeat (to empty)",
      "truncate (to half, of a copy)"
    ]);
    bench.cols([
      "1_000",
      "100_000"
    ]);

    // inputs are built once, outside the measured runner
    let small = List.tabulate<Nat>(1_000, func i = i);
    let large = List.tabulate<Nat>(100_000, func i = i);
    let smallArray = small.toArray();
    let largeArray = large.toArray();
    let smallVarArray = small.toVarArray();
    let largeVarArray = large.toVarArray();

    bench.runner(
      func(row, col) {
        let list = switch col {
          case "1_000" small;
          case "100_000" large;
          case _ Runtime.unreachable()
        };
        let n = list.size();
        let (array, varArray) = if (n == 1_000) (smallArray, smallVarArray) else (largeArray, largeVarArray);
        switch row {
          case "loop" {
            var i = 0;
            while (i < n) { i += 1 }
          };
          case "at" {
            var i = 0;
            while (i < n) { ignore list.at(i); i += 1 }
          };
          case "get" {
            var i = 0;
            while (i < n) { ignore list.get(i); i += 1 }
          };
          case "get (out of range)" {
            var i = 0;
            while (i < n) { ignore list.get(n + i); i += 1 }
          };
          case "put" {
            var i = 0;
            while (i < n) { list.put(i, i); i += 1 }
          };
          case "size" {
            var i = 0;
            while (i < n) { ignore list.size(); i += 1 }
          };
          case "range (iterate all)" {
            for (_ in list.range(0, n)) {}
          };
          case "reader (read all)" {
            let next = list.reader(0);
            var i = 0;
            while (i < n) { ignore next(); i += 1 }
          };
          case "sliceToArray (all)" ignore list.sliceToArray(0, n);
          case "sliceToVarArray (all)" ignore list.sliceToVarArray(0, n);
          case "forEachInRange (all)" list.forEachInRange(func _ = (), 0, n);
          case "nextIndexOf (not found)" ignore list.nextIndexOf(Nat.equal, n, 0);
          case "prevIndexOf (not found)" ignore list.prevIndexOf(Nat.equal, n, n);
          case "reverseInPlace" list.reverseInPlace();
          case "tabulate" ignore List.tabulate(n, func i = i);
          case "fromArray" ignore List.fromArray(array);
          case "fromVarArray" ignore List.fromVarArray(varArray);
          case "repeat" ignore List.repeat(0, n);
          case "addRepeat (to empty)" List.empty<Nat>().addRepeat(0, n);
          case "truncate (to half, of a copy)" {
            // the copy is part of the cost; compare with the clone row in other benchmarks
            let copy = list.clone();
            copy.truncate(n / 2)
          };
          case _ Runtime.unreachable()
        }
      }
    );

    bench
  }
}
