import Debug "../src/Debug";
import Nat "../src/Nat";
import Array "../src/Array";

Debug.print("Nat");

do {
  Debug.print("  add");

  assert Nat.add(1, Nat.add(2, 3)) == Nat.add(1, Nat.add(2, 3));
  assert Nat.add(0, 1) == 1;
  assert 1 == Nat.add(1, 0);
  assert Nat.add(0, 1) == Nat.add(1, 0);
  assert Nat.add(1, 2) == Nat.add(2, 1)
};

do {
  Debug.print("  shift");

  assert Nat.bitshiftLeft(1234567890, 3) == 1234567890 * 8;
  assert Nat.bitshiftRight(1234567892, 2) == 1234567892 / 4
};

do {
  Debug.print("  toText");

  assert Nat.toText(0) == "0";
  assert Nat.toText(1234) == "1234"
};

do {
  Debug.print("  range");

  assert Array.fromIter(Nat.range(0, 3)) == [0, 1, 2];
  assert Array.fromIter(Nat.range(1, 3)) == [1, 2];
  assert Array.fromIter(Nat.range(1, 2)) == [1];
  assert Array.fromIter(Nat.range(3, 0)) == [];
  assert Array.fromIter(Nat.range(1, 0)) == [];
  assert Array.fromIter(Nat.range(0, 0)) == [];
};

do {
  Debug.print("  rangeBy");

  assert Array.fromIter(Nat.rangeBy(0, 3, 1)) == [0, 1, 2];
  assert Array.fromIter(Nat.rangeBy(0, 3, 2)) == [0, 2];
  assert Array.fromIter(Nat.rangeBy(0, 3, 3)) == [0];
  assert Array.fromIter(Nat.rangeBy(1, 4, 2)) == [1, 3];
  assert Array.fromIter(Nat.rangeBy(1, 3, 2)) == [1];
  assert Array.fromIter(Nat.rangeBy(3, 0, -1)) == [3, 2, 1];
  assert Array.fromIter(Nat.rangeBy(3, 1, -1)) == [3, 2];
  assert Array.fromIter(Nat.rangeBy(3, 0, -2)) == [3, 1];
  assert Array.fromIter(Nat.rangeBy(3, 1, -2)) == [3];
  assert Array.fromIter(Nat.rangeBy(1, 3, -1)) == [];
  assert Array.fromIter(Nat.rangeBy(0, 1, 0)) == [];
  assert Array.fromIter(Nat.rangeBy(1, 0, 0)) == [];
}
