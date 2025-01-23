/// Signed integer numbers with infinite precision (also called big integers)

import Prim "mo:⛔";
import Hash "Hash";
import Iter "IterType";
import { todo } "Debug";
import Runtime "Runtime";

module {

  public type Int = Prim.Types.Int;

  public func abs(x : Int) : Nat {
    Prim.abs(x)
  };

  public func toText(x : Int) : Text {
    if (x == 0) {
      return "0"
    };

    let isNegative = x < 0;
    var int = if isNegative { -x } else { x };

    var text = "";
    let base = 10;

    while (int > 0) {
      let rem = int % base;
      text := (
        switch (rem) {
          case 0 { "0" };
          case 1 { "1" };
          case 2 { "2" };
          case 3 { "3" };
          case 4 { "4" };
          case 5 { "5" };
          case 6 { "6" };
          case 7 { "7" };
          case 8 { "8" };
          case 9 { "9" };
          case _ { Runtime.unreachable() }
        }
      ) # text;
      int := int / base
    };

    return if isNegative { "-" # text } else { text }
  };

  public func min(x : Int, y : Int) : Int {
    if (x < y) { x } else { y }
  };

  public func max(x : Int, y : Int) : Int {
    if (x < y) { y } else { x }
  };

  public func hash(i : Int) : Hash.Hash {
    todo() // New hash function?
  };

  public func equal(x : Int, y : Int) : Bool { x == y };

  public func notEqual(x : Int, y : Int) : Bool { x != y };

  public func less(x : Int, y : Int) : Bool { x < y };

  public func lessOrEqual(x : Int, y : Int) : Bool { x <= y };

  public func greater(x : Int, y : Int) : Bool { x > y };

  public func greaterOrEqual(x : Int, y : Int) : Bool { x >= y };

  public func compare(x : Int, y : Int) : { #less; #equal; #greater } {
    if (x < y) { #less } else if (x == y) { #equal } else { #greater }
  };

  public func neg(x : Int) : Int { -x };

  public func add(x : Int, y : Int) : Int { x + y };

  public func sub(x : Int, y : Int) : Int { x - y };

  public func mul(x : Int, y : Int) : Int { x * y };

  public func div(x : Int, y : Int) : Int { x / y };

  public func rem(x : Int, y : Int) : Int { x % y };

  public func pow(x : Int, y : Int) : Int { x ** y };

  public func range(fromInclusive : Int, toExclusive : Int) : Iter.Iter<Int> {
    todo()
  };

  public func rangeInclusive(from : Int, to : Int) : Iter.Iter<Int> {
    todo()
  };

}
