import Array "../src/Array";
import Blob "../src/Blob";
import Base64 "../src/Base64";
import _Nat "../src/Nat";
import Bench "mo:bench-helper";

module {

  // Generate a Blob of zero bytes of given length
  func zeros(len : Nat) : Blob {
    Array.tabulate<Nat8>(len, func _ = 0)
    |> Blob.fromArray(_);
  };
  // Generate a Blob of mixed bytes of given length
  func mixed(len : Nat) : Blob {
    Array.tabulate<Nat8>(len, func i = (i % 256).toNat8())
    |> Blob.fromArray(_);
  };

  public func init() : Bench.V1 {
    let nRows = 5;

    let schema : Bench.Schema = {
      name = "Base64";
      description = "Compare zero bytes vs mixed bytes encoding to Base64";
      rows = Array.tabulate(nRows, func(i) = (10 ** i).toText());
      cols = ["zero bytes", "mixed bytes"];
    };

    let inputs : [[Blob]] = Array.tabulate(
      nRows,
      func ri = [zeros(10 ** ri), mixed(10 ** ri)],
    );

    let run : Bench.Runner = func(ri, ci) {
      ignore Base64.encode(inputs[ri][ci]);
    };

    Bench.V1(schema, run);
  };
};
