module {

  /// An iterator that produces values of type `T`. Calling `next` returns
  /// `null` when iteration is finished.
  ///
  /// Iterators are inherently stateful. Calling `next` "consumes" a value from
  /// the Iterator that cannot be put back, so keep that in mind when sharing
  /// iterators between consumers.
  ///
  /// An iterator `i` can be iterated over using
  /// ```
  /// for (x in i) {
  ///   // do something with x...
  /// }
  /// ```
  public type Iter<T> = { next : () -> ?T };
}
