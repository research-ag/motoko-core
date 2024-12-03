/// Original: `OrderedMap.mo`

import { Iter } "Iter";
import { Order } "Order";
import { todo } "Debug";

module {
  type Args<K> = {
    compare : (K, K) -> Order.Order
  };

  public func fromIter<K, V>(iter : Iter<(K, V)>, args : Args<K>) : Map<K, V> = todo();

  class Map<K, V>(handle : Args<K>) {

    public func contains(key : K) : Bool = todo();

    public func get(key : K) : ?V = todo();

    public func put(key : K, value : V) : Bool = todo();

    public func replace(key : K, value : V) : ?V = todo();

    public func delete<V>(m : Map<K, V>, key : K) : ?V = todo();

    public func maxEntry() : ?(K, V) = todo();

    public func minEntry() : ?(K, V) = todo();

    public func map<V2>(f : (K, V) -> V2) : Map<K, V2> = todo();

    public func filter(f : (K, V) -> Bool) : Map<K, V> = todo();

    public func filterMap<V2>(m : Map<K, V>, f : (K, V) -> ?V2) : Map<K, V2> = todo();

    public func size() : Nat = todo();

    public func keys() : Iter<K> = todo();

    public func vals() : Iter<V> = todo();

    public func items() : Iter<(K, V)> = todo();

    public func foldLeft<A>(base : A, combine : (A, K, V) -> A) : A {
      todo()
    };

    public func foldRight<A>(base : A, combine : (K, V, A) -> A) : A {
      todo()
    };

    public func all(pred : (K, V) -> Bool) : Bool = todo();

    public func any<V>(pred : (K, V) -> Bool) : Bool = todo();

  }
}
