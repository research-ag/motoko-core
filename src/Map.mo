import Order "Order";

module {
  type Map<K, V> = (); // TODO

  func Map<K, V>({ compare : (a : K, b : K) -> Order.Order }) {
    assert false
  }
}
