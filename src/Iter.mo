/// Iterators

import Order "Order";
import { todo } "Debug";

module {

  /// An iterator that produces values of type `T`. Calling `next` returns
  /// `null` when iteration is finished.
  ///
  /// Iterators are inherently stateful. Calling `next` "consumes" a value from
  /// the Iterator that cannot be put back, so keep that in mind when sharing
  /// iterators between consumers.
  ///
  /// An iterater `i` can be iterated over using
  /// ```
  /// for (x in i) {
  ///   // do something with x...
  /// }
  /// ```
  public class Iter<T>(next : () -> ?T) = this {
    /// Calls a function `f` on every value produced by an iterator and discards
    /// the results. If you're looking to keep these results use `map` instead.
    ///
    /// ```motoko
    /// import Iter "mo:base/Iter";
    /// var sum = 0;
    /// Iter.range(1, 3).forEach(func(x, _index) {
    ///   sum += x;
    /// });
    /// assert(sum == 6)
    /// ```
    public func forEach(f : (T, Nat) -> ()) {
      todo()
    };

    /// Consumes an iterator and counts how many elements were produced
    /// (discarding them in the process).
    public func size() : Nat {
      todo()
    };

    /// Takes a function and an iterator and returns a new iterator that lazily applies
    /// the function to every element produced by the argument iterator.
    /// ```motoko
    /// import Iter "mo:base/Iter";
    /// let iter = Iter.range(1, 3);
    /// let mapped = iter.map(func (x : Nat) : Nat { x * 2 });
    /// assert(mapped.next() == ?2);
    /// assert(mapped.next() == ?4);
    /// assert(mapped.next() == ?6);
    /// assert(mapped.next() == null);
    /// ```
    public func map<R>(f : T -> R) : Iter<R> {
      todo()
    };

    /// Takes a function and an iterator and returns a new iterator that produces
    /// elements from the original iterator if and only if the predicate is true.
    /// ```motoko
    /// import Iter "mo:base/Iter";
    /// let iter = Iter.range(1, 3);
    /// let filtered = iter.filter(func (x : Nat) : Bool { x % 2 == 1 });
    /// assert(filtered.next() == ?1);
    /// assert(filtered.next() == ?3);
    /// assert(filtered.next() == null);
    /// ```
    public func filter(f : T -> Bool) : Iter<T> {
      todo()
    };

    /// Consumes an iterator and collects its produced elements in an Array.
    /// ```motoko
    /// import Iter "mo:base/Iter";
    /// let iter = Iter.range(1, 3);
    /// assert(iter.toArray(), [1, 2, 3]);
    /// ```
    public func toArray() : [T] {
      todo()
    };

    /// Like `toArray` but for Arrays with mutable elements.
    public func toArrayMut() : [var T] {
      todo()
    };

    /// Sorted iterator.  Will iterate over *all* elements to sort them, necessarily.
    public func sort(compare : (T, T) -> Order.Order) : Iter<T> {
      todo()
    };

    /// Takes two iterators and returns a new iterator that produces
    /// elements from the original iterators sequentally.
    /// ```motoko
    /// import Iter "mo:base/Iter";
    /// let iter1 = Iter.range(1, 2);
    /// let iter2 = Iter.range(5, 6);
    /// let extended = iter1.extend(iter2);
    /// assert(extended.next() == ?1);
    /// assert(extended.next() == ?2);
    /// assert(extended.next() == ?5);
    /// assert(extended.next() == ?6);
    /// assert(extended.next() == null);
    /// ```
    public func extend<A>(other : Iter<A>) : Iter<A> {
      var ended : Bool = false;
      Iter(
        func() {
          if (ended) {
            return other.next()
          };
          switch (this.next()) {
            case (?x) ?x;
            case (null) {
              ended := true;
              other.next()
            }
          }
        }
      )
    }
  };

  /// Creates an iterator that produces all `Nat`s from `x` to `y` including
  /// both of the bounds.
  /// ```motoko
  /// import Iter "mo:base/Iter";
  /// let iter = Iter.range(1, 3);
  /// assert(iter.next() == ?1);
  /// assert(iter.next() == ?2);
  /// assert(iter.next() == ?3);
  /// assert(iter.next() == null);
  /// ```
  public class range(x : Int, y : Int) : Iter<Int> {
    var i = x;
    Iter(
      func() {
        if (i > y) { null } else { let j = i; i += 1; ?j }
      }
    ) : Iter<Int>
  };

  /// Like `range` but produces the values in the opposite
  /// order.
  public class revRange(x : Int, y : Int) : Iter<Int> {
    var i = x;
    Iter(
      func() {
        if (i < y) { null } else { let j = i; i -= 1; ?j }
      }
    ) : Iter<Int>
  };

  /// Creates an iterator that produces an infinite sequence of `x`.
  /// ```motoko
  /// import Iter "mo:base/Iter";
  /// let iter = Iter.infinite(10);
  /// assert(?10 == iter.next());
  /// assert(?10 == iter.next());
  /// assert(?10 == iter.next());
  /// // ...
  /// ```
  public func infinite<T>(x : T) : Iter<T> {
    Iter(func() = ?x)
  };

  /// Takes an Array of iterators and returns a new iterator that produces
  /// elements from the original iterators sequentally.
  /// ```motoko
  /// import Iter "mo:base/Iter";
  /// let iter1 = Iter.range(1, 2);
  /// let iter2 = Iter.range(5, 6);
  /// let concatenated = Iter.concat([iter1, iter2]);
  /// assert(concatenated.next() == ?1);
  /// assert(concatenated.next() == ?2);
  /// assert(concatenated.next() == ?5);
  /// assert(concatenated.next() == ?6);
  /// assert(concatenated.next() == null);
  /// ```
  public func concat<T>(other : [Iter<T>]) : Iter<T> {
    todo()
  };

  /// Creates an iterator that produces the elements of an Array in ascending index order.
  /// ```motoko
  /// import Iter "mo:base/Iter";
  /// let iter = Iter.fromArray([1, 2, 3]);
  /// assert(iter.next() == ?1);
  /// assert(iter.next() == ?2);
  /// assert(iter.next() == ?3);
  /// assert(iter.next() == null);
  /// ```
  public func fromArray<T>(xs : [T]) : Iter<T> {
    var ix : Nat = 0;
    let size = xs.size();
    Iter(
      func() : ?T {
        if (ix >= size) {
          return null
        } else {
          let res = ?(xs[ix]);
          ix += 1;
          return res
        }
      }
    )
  };

  /// Like `fromArray` but for Arrays with mutable elements. Captures
  /// the elements of the Array at the time the iterator is created, so
  /// further modifications won't be reflected in the iterator.
  public func fromArrayMut<T>(xs : [var T]) : Iter<T> {
    fromArray<T>(Array.freeze<T>(xs))
  };

  /// Like `fromArray` but for Lists.
  public let fromList = List.toIter
}
