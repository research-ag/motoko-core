import Bench "mo:bench";

import Array "../src/Array";
import Nat "../src/Nat";
import Nat8 "../src/Nat8";
import Nat16 "../src/Nat16";
import Nat32 "../src/Nat32";
import Nat64 "../src/Nat64";
import Int "../src/Int";
import Int8 "../src/Int8";
import Int16 "../src/Int16";
import Int32 "../src/Int32";
import Int64 "../src/Int64";
import Runtime "../src/Runtime";

module {
  public func init() : Bench.Bench {
    let bench = Bench.Bench();

    bench.name("Int conversions");
    bench.description("Compares performance of different Nat conversions");

    bench.rows([
      "Nat8.toNat16",
      "Nat8.toNat32",
      "Nat8.toNat64",
      "Nat16.toNat32",
      "Nat16.toNat64",
      "Nat32.toNat64",
      "Int8.toInt16",
      "Int8.toInt32",
      "Int8.toInt64",
      "Int16.toInt32",
      "Int16.toInt64",
      "Int32.toInt64",
    ]);
    bench.cols([
      "1000",
    ]);

    let source8 = Array.tabulate<Nat8>(1000, func i = Nat8.fromIntWrap(i));
    let source16 = Array.tabulate<Nat16>(1000, func i = Nat16.fromIntWrap(i));
    let source32 = Array.tabulate<Nat32>(1000, func i = Nat32.fromIntWrap(i));
    let source8int = Array.tabulate<Int8>(1000, func i = Int8.fromIntWrap(i));
    let source16int = Array.tabulate<Int16>(1000, func i = Int16.fromIntWrap(i));
    let source32int = Array.tabulate<Int32>(1000, func i = Int32.fromIntWrap(i));
    let size = 1000;

    bench.runner(
      func(row, col) {
        switch row {
          case "Nat8.toNat16" {
            var i = 0;
            while (i < size) {
              ignore Nat8.toNat16(source8[i]);
              i += 1
            };
          };
          case "Nat8.toNat32" {
            var i = 0;
            while (i < size) {
              ignore Nat8.toNat32(source8[i]);
              i += 1
            };
          };
          case "Nat8.toNat64" {
            var i = 0;
            while (i < size) {
              ignore Nat8.toNat64(source8[i]);
              i += 1
            };
          };
          case "Nat16.toNat32" {
            var i = 0;
            while (i < size) {
              ignore Nat16.toNat32(source16[i]);
              i += 1
            };
          };
          case "Nat16.toNat64" {
            var i = 0;
            while (i < size) {
              ignore Nat16.toNat64(source16[i]);
              i += 1
            };
          };
          case "Nat32.toNat64" {
            var i = 0;
            while (i < size) {
              ignore Nat32.toNat64(source32[i]);
              i += 1
            };
          };
          case "Int8.toInt16" {
            var i = 0;
            while (i < size) {
              ignore Int8.toInt16(source8int[i]);
              i += 1
            };
          };
          case "Int8.toInt32" {
            var i = 0;
            while (i < size) {
              ignore Int8.toInt32(source8int[i]);
              i += 1
            };
          };
          case "Int8.toInt64" {
            var i = 0;
            while (i < size) {
              ignore Int8.toInt64(source8int[i]);
              i += 1
            };
          };
          case "Int16.toInt32" {
            var i = 0;
            while (i < size) {
              ignore Int16.toInt32(source16int[i]);
              i += 1
            };
          };
          case "Int16.toInt64" {
            var i = 0;
            while (i < size) {
              ignore Int16.toInt64(source16int[i]);
              i += 1
            };
          };
          case "Int32.toInt64" {
            var i = 0;
            while (i < size) {
              ignore Int32.toInt64(source32int[i]);
              i += 1
            };
          };
          case _ Runtime.unreachable()
        }
      }
    );

    bench
  }
}
