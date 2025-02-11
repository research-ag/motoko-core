/// Immutable singly-linked list

import { Array_tabulate } "mo:⛔";
import Array "../Array";
import Iter "../Iter";
import Order "../Order";
import Result "../Result";
import Types "../Types";
import { todo } "../Debug";

module {

  public type List<T> = Types.Pure.List<T>;

  public func empty<T>() : List<T> = null;

  public func isEmpty<T>(list : List<T>) : Bool =
    switch list { case null true; case _ false };

  public func size<T>(list : List<T>) : Nat =
    switch list {
      case null 0;
      case (?(_, t)) 1 + size t
    };

  public func contains<T>(list : List<T>, item : T) : Bool {
    todo()
  };

  public func get<T>(list : List<T>, n : Nat) : ?T =
    switch list {
      case null null;
      case (?(h, t)) if (n == 0) ?h else get(t, n - 1)
    };

  public func push<T>(list : List<T>, item : T) : List<T> = ?(item, list);

  public func last<T>(list : List<T>) : ?T =
    switch list {
      case (?(h, null)) ?h;
      case null null;
      case (?(_, t)) last t
    };

  public func pop<T>(list : List<T>) : (?T, List<T>) =
    switch list {
      case null (null, null);
      case (?(h, t)) (?h, t)
    };

  public func reverse<T>(list : List<T>) : List<T> {
    func go(acc : List<T>, list : List<T>) : List<T> =
      switch list {
        case null acc;
        case (?(h, t)) go(?(h, acc), t)
      };
    go(null, list)
  };

  public func forEach<T>(list : List<T>, f : T -> ()) =
    switch list {
      case null ();
      case (?(h, t)) { f h; forEach(t, f) }
    };

  public func map<T1, T2>(list : List<T1>, f : T1 -> T2) : List<T2> =
    switch list {
      case null null;
      case (?(h, t)) ?(f h, map(t, f))
    };

  public func filter<T>(list : List<T>, f : T -> Bool) : List<T> =
    switch list {
      case null null;
      case (?(h, t)) if (f h) ?(h, filter(t, f)) else filter(t, f)
    };

  public func filterMap<T, R>(list : List<T>, f : T -> ?R) : List<R> =
    switch list {
      case null null;
      case (?(h, t)) {
        let ?v = f h else return filterMap(t, f);
        ?(v, filterMap(t, f))
      }
    };

  public func mapResult<T, R, E>(list : List<T>, f : T -> Result.Result<R, E>) : Result.Result<List<R>, E> {
    todo()
  };

  public func partition<T>(list : List<T>, f : T -> Bool) : (List<T>, List<T>) =
    switch list {
      case null (null, null);
      case (?(h, t)) {
        let left = f h;
        let (l, r) = partition(t, f);
        if left (?(h, l), r) else (l, ?(h, r))
      }
    };

  public func concat<T>(list1 : List<T>, list2 : List<T>) : List<T> =
    switch list1 {
      case null list2;
      case (?(h, t)) ?(h, concat(t, list2))
    };

  public func join<T>(list : Iter.Iter<List<T>>) : List<T> {
    todo()
  };

  public func flatten<T>(list : List<List<T>>) : List<T> {
    todo()
  };

  public func take<T>(list : List<T>, n : Nat) : List<T> =
    if (n == 0) null
    else switch list {
      case null null;
      case (?(h, t)) ?(h, take(t, n - 1))
    };

  public func drop<T>(list : List<T>, n : Nat) : List<T> =
    if (n == 0) list
    else switch list {
      case null null;
      case (?(h, t)) drop(t, n - 1)
    };

  public func foldLeft<T, A>(list : List<T>, base : A, combine : (A, T) -> A) : A =
    switch list {
      case null base;
      case (?(h, t)) foldLeft(t, combine(base, h), combine)
    };

  public func foldRight<T, A>(list : List<T>, base : A, combine : (T, A) -> A) : A =
    switch list {
      case null base;
      case (?(h, t)) combine(h, foldRight(t, base, combine))
    };

  public func find<T>(list : List<T>, f : T -> Bool) : ?T =
    switch list {
      case null null;
      case (?(h, t)) if (f h) ?h else find(t, f)
    };

  public func all<T>(list : List<T>, f : T -> Bool) : Bool =
    switch list {
      case null true;
      case (?(h, t)) f h and all(t, f)
    };

  public func any<T>(list : List<T>, f : T -> Bool) : Bool =
    switch list {
      case null false;
      case (?(h, t)) f h or any(t, f)
    };

  public func merge<T>(list1 : List<T>, list2 : List<T>, lessThanOrEqual : (T, T) -> Bool) : List<T> =
    switch (list1, list2) {
      case (?(h1, t1), ?(h2, t2))
        if (lessThanOrEqual(h1, h2)) ?(h1, merge(t1, list2, lessThanOrEqual))
        else if (lessThanOrEqual(h2, h1)) ?(h2, merge(list1, t2, lessThanOrEqual))
        else ?(h1, ?(h2, merge(list1, list2, lessThanOrEqual)));
      case (null, _) list2;
      case (_, null) list1
    };

  public func compare<T>(list1 : List<T>, list2 : List<T>, compare : (T, T) -> Order.Order) : Order.Order {
    todo()
  };

  public func tabulate<T>(n : Nat, f : Nat -> T) : List<T> {
    func go(at : Nat, n : Nat) : List<T> =
      if (n == 0) null else ?(f at, go(at + 1, n - 1));
    go(0, n)
  };

  public func singleton<T>(item : T) : List<T> = ?(item, null);

  public func repeat<T>(item : T, n : Nat) : List<T> =
    if (n == 0) null else ?(item, repeat(item, n - 1));

  public func zip<T, U>(list1 : List<T>, list2 : List<U>) : List<(T, U)> = zipWith<T, U, (T, U)>(list1, list2, func(x, y) = (x, y));

  public func zipWith<T, U, V>(list1 : List<T>, list2 : List<U>, f : (T, U) -> V) : List<V> =
    switch (list1, list2) {
      case (?(h1, t1), ?(h2, t2)) ?(f(h1, h2), zipWith(t1, t2, f));
      case _ null
    };

  public func split<T>(list : List<T>, n : Nat) : (List<T>, List<T>) =
    if (n == 0) (null, list)
    else switch list {
      case null (null, null);
      case (?(h, t)) {
        let (l1, l2) = split(t, n - 1);
        (?(h, l1), l2)
      }
    };

  public func chunks<T>(list : List<T>, n : Nat) : List<List<T>> =
    switch (split(list, n)) {
      case (null, _) null;
      case (pre, null) ?(pre, null);
      case (pre, post) ?(pre, chunks(post, n));
    };

  public func values<T>(list : List<T>) : Iter.Iter<T> = object {
    var l = list;
    public func next() : ?T = switch l {
      case null null;
      case (?(h, t)) {
        l := t;
        ?h
      }
    }
  };

  public func fromArray<T>(array : [T]) : List<T> {
    func go(from : Nat) : List<T> =
      if (from < array.size()) ?(array.get from, go(from + 1)) else null;
    go 0
  };

  public func fromVarArray<T>(array : [var T]) : List<T> = fromArray<T>(Array.fromVarArray<T>(array));

  public func toArray<T>(list : List<T>) : [T] {
    var l = list;
    Array_tabulate<T>(length list, func _ { let ?(h, t) = l else loop(); l := t; h })
  };

  public func toVarArray<T>(list : List<T>) : [var T] = Array.toVarArray<T>(toArray<T>(list));

  public func fromIter<T>(iter : Iter.Iter<T>) : List<T> =
    switch (iter.next()) {
      case null null;
      case (?item) ?(item, fromIter iter)
    };

  public func toText<T>(list : List<T>, f : T -> Text) : Text {
    var text = "[";
    var first = false;
    forEach(
      list,
      func(item : T) {
        if first {
          text #= ", "
        } else {
          first := true
        };
        text #= f(item)
      }
    );
    text # "]"
  };

}
