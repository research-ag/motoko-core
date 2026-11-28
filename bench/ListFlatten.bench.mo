import Bench "mo:bench";

import List "../src/List";
import Runtime "../src/Runtime";

module {
  public func init() : Bench.Bench {
    let bench = Bench.Bench();

    bench.name("List.flatten vs. List.join");
    bench.description("Flattening a List of Lists: `flatten` reserves the whole result once, `join` reserves per sublist.");

    bench.rows([
      "flatten",
      "join"
    ]);
    // number of sublists x size of each sublist
    bench.cols([
      "10 x 10",
      "100 x 100",
      "1000 x 10",
      "10 x 1000"
    ]);

    // inputs are built once, outside the measured runner
    func make(outer : Nat, inner : Nat) : List.List<List.List<Nat>> = List.tabulate(outer, func _ = List.tabulate(inner, func j = j));
    let small = make(10, 10);
    let square = make(100, 100);
    let manyShort = make(1000, 10);
    let fewLong = make(10, 1000);

    bench.runner(
      func(row, col) {
        let lists = switch col {
          case "10 x 10" small;
          case "100 x 100" square;
          case "1000 x 10" manyShort;
          case "10 x 1000" fewLong;
          case _ Runtime.unreachable()
        };
        switch row {
          case "flatten" ignore lists.flatten();
          case "join" ignore lists.values().join();
          case _ Runtime.unreachable()
        }
      }
    );

    bench
  }
}
