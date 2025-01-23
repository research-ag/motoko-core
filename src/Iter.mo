/// Utilities for `Iter` (iterator) values

import Type "IterType";
import Order "Order";
import { todo } "Debug";
import Prim "mo:prim";
import Runtime "Runtime";

module {

  public type Iter<T> = Type.Iter<T>;

  public func empty<T>() : Iter<T> = { next = func _ = null };

  public func natRange(fromInclusive : Nat, toExclusive : Nat) : Type.Iter<Nat> {
    object {
      var current = fromInclusive;

      public func next() : ?Nat {
        if (current >= toExclusive) {
          return null;
        };
        let result = current;
        current += 1;
        ?result;
      }
    }
  };

  public class rangeRev(fromInclusive : Int, toExclusive : Int) {
    todo()
  };

  public func forEach<T>(iter : Iter<T>, f : (T, Nat) -> ()) {
    todo()
  };

  public func size<T>(iter : Iter<T>) : Nat {
    todo()
  };

  public func map<T1, T2>(iter : Iter<T1>, f : T1 -> T2) : Iter<T2> {
    todo()
  };

  public func filter<T>(iter : Iter<T>, f : T -> Bool) : Iter<T> {
    todo()
  };

  public func filterMap<T1, T2>(iter : Iter<T1>, f : T1 -> ?T2) : Iter<T2> {
    todo()
  };

  public func infinite<T>(x : T) : Iter<T> {
    todo()
  };

  public func singleton<T>(x : T) : Iter<T> {
    todo()
  };

  public func concat<T>(a : Iter<T>, b : Iter<T>) : Iter<T> {
    todo()
  };

  public func concatAll<T>(iters : [Iter<T>]) : Iter<T> {
    todo()
  };

  public func fromArray<T>(array : [T]) : Iter<T> {
    var index = 0;
    object {
      public func next() : ?T {
        if (index >= array.size()) {
          null;
        } else {
          let value = array[index];
          index += 1;
          ?value
        }
      }
    }
  };

  public func fromVarArray<T>(array : [var T]) : Iter<T> {
    todo()
  };

  public func toArray<T>(iter : Iter<T>) : [T] {
    // TODO: Replace implementation. This is just temporay.
    type Node<T> = { value: T; var next: ?Node<T>};
    var first: ?Node<T> = null;
    var last: ?Node<T> = null;
    var count = 0;

    func add(value: T) {
      let node : Node<T> = { value; var next = null };
      switch (last) {
        case null {
          first := ?node;
        };
        case (?previous) {
          previous.next := ?node;
        }
      };
      last := ?node;
      count += 1;
    };

    for (value in iter) {
      add(value);
    };
    if (count == 0) {
      return [];
    };
    var current = first;
    Prim.Array_tabulate<T>(count, func (_) {
      switch (current) {
        case null Runtime.trap("Node must not be null");
        case (?node) {
          current := node.next;
          node.value
        }
      }
    });
  };

  public func toVarArray<T>(iter : Iter<T>) : [var T] {
    todo()
  };

  public func sort<T>(iter : Iter<T>, compare : (T, T) -> Order.Order) : Iter<T> {
    todo()
  };

}
