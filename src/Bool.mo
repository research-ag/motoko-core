/// Boolean type and operations.
///
/// While boolean operators `_ and _` and `_ or _` are short-circuiting,
/// avoiding computation of the right argument when possible, the functions
/// `logand(_, _)` and `logor(_, _)` are *strict* and will always evaluate *both*
/// of their arguments.

import Prim "mo:⛔";

module {

  /// Booleans with constants `true` and `false`.
  public type Bool = Prim.Types.Bool;

  /// Returns `"true"` or `"false"`.
  public func toText(b : Bool) : Text {
    if b { "true" } else { "false" }
  };

  /// Returns the order of `x` and `y`, where `false < true`.
  public func compare(b1 : Bool, b2 : Bool) : { #less; #equal; #greater } {
    if (b1 == b2) { #equal } else if (b1) { #greater } else { #less }
  };

}
