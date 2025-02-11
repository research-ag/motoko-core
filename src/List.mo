/// Resizable array with `O(sqrt(n))` memory overhead.
/// Static type `List` that can be declared `stable`.
/// For the `List` class see the file Class.mo.
///
/// The functions are modeled with respect to naming and semantics after their
/// counterparts for `Buffer` in motoko-base.
///
/// This implementation is adapted from the `vector` Mops package created by Research AG.
///
/// Copyright: 2023 MR Research AG
/// Main author: Andrii Stepanov
/// Contributors: Timo Hanke (timohanke), Andy Gura (andygura), react0r-com

import Prim "mo:⛔";
import { bitcountLeadingZero = leadingZeros; fromNat = Nat32; toNat = Nat } "Nat32";
import Array "Array";
import Iter "Iter";
import { min = natMin; compare = natCompare; range } "Nat";
import Order "Order";
import Option "Option";
import VarArray "VarArray";

module {
  /// `List<T>` provides a mutable list of elements of type `T`.
  /// Based on the paper "Resizable Arrays in Optimal Time and Space" by Brodnik, Carlsson, Demaine, Munro and Sedgewick (1999).
  /// Since this is internally a two-dimensional array the access times for put and get operations
  /// will naturally be 2x slower than Buffer and Array. However, Array is not resizable and Buffer
  /// has `O(size)` memory waste.
  public type List<T> = {
    /// the index block
    var data_blocks : [var [var ?T]];
    /// new element should be assigned to exaclty data_blocks[i_block][i_element]
    /// i_block is in range (0; data_blocks.size()]
    var i_block : Nat;
    /// i_element is in range [0; data_blocks[i_block].size())
    var i_element : Nat
  };

  let INTERNAL_ERROR = "Internal error in List";

  /// Creates a new empty List for elements of type T.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.new<Nat>(); // Creates a new List
  /// ```
  public func new<T>() : List<T> = {
    var data_blocks = [var [var]];
    var i_block = 1;
    var i_element = 0
  };

  /// Create a List with `size` copies of the initial value.
  ///
  /// ```
  /// let list = List.init<Nat>(4, 2); // [2, 2, 2, 2]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func init<T>(size : Nat, initValue : T) : List<T> {
    let (i_block, i_element) = locate(size);

    let blocks = new_index_block_length(Nat32(if (i_element == 0) { i_block - 1 } else i_block));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    while (i < i_block) {
      data_blocks[i] := VarArray.repeat<?T>(?initValue, data_block_size(i));
      i += 1
    };
    if (i_element != 0 and i_block < blocks) {
      let block = VarArray.repeat<?T>(null, data_block_size(i));
      var j = 0;
      while (j < i_element) {
        block[j] := ?initValue;
        j += 1
      };
      data_blocks[i] := block
    };

    {
      var data_blocks = data_blocks;
      var i_block = i_block;
      var i_element = i_element
    }
  };

  /// Add to vector `count` copies of the initial value.
  ///
  /// ```
  /// let list = List.init<Nat>(4, 2); // [2, 2, 2, 2]
  /// List.addMany(list, 2, 1); // [2, 2, 2, 2, 1, 1]
  /// ```
  ///
  /// Runtime: `O(count)`
  public func addMany<T>(list : List<T>, count : Nat, initValue : T) {
    let (i_block, i_element) = locate(size(list) + count);
    let blocks = new_index_block_length(Nat32(if (i_element == 0) { i_block - 1 } else i_block));

    let old_blocks = list.data_blocks.size();
    if (old_blocks < blocks) {
      let old_data_blocks = list.data_blocks;
      list.data_blocks := VarArray.repeat<[var ?T]>([var], blocks);
      var i = 0;
      while (i < old_blocks) {
        list.data_blocks[i] := old_data_blocks[i];
        i += 1
      }
    };

    var cnt = count;
    while (cnt > 0) {
      let db_size = data_block_size(list.i_block);
      if (list.i_element == 0 and db_size <= cnt) {
        list.data_blocks[list.i_block] := VarArray.repeat<?T>(?initValue, db_size);
        cnt -= db_size;
        list.i_block += 1
      } else {
        if (list.data_blocks[list.i_block].size() == 0) {
          list.data_blocks[list.i_block] := VarArray.repeat<?T>(null, db_size)
        };
        let from = list.i_element;
        let to = natMin(list.i_element + cnt, db_size);

        let block = list.data_blocks[list.i_block];
        var i = from;
        while (i < to) {
          block[i] := ?initValue;
          i += 1
        };

        list.i_element := to;
        if (list.i_element == db_size) {
          list.i_element := 0;
          list.i_block += 1
        };
        cnt -= to - from
      }
    }
  };

  /// Resets the vector to size 0, de-referencing all elements.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// List.clear(list); // vector is now empty
  /// List.toArray(list) // => []
  /// ```
  ///
  /// Runtime: `O(1)`
  public func clear<T>(list : List<T>) {
    list.data_blocks := [var [var]];
    list.i_block := 1;
    list.i_element := 0
  };

  /// Returns a copy of a List, with the same size.
  ///
  /// Example:
  /// ```motoko
  ///
  /// list.add(1);
  ///
  /// let clone = List.clone(list);
  /// List.toArray(clone); // => [1]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func clone<T>(list : List<T>) : List<T> = {
    var data_blocks = VarArray.tabulate<[var ?T]>(
      list.data_blocks.size(),
      func(i) = VarArray.tabulate<?T>(
        list.data_blocks[i].size(),
        func(j) = list.data_blocks[i][j]
      )
    );
    var i_block = list.i_block;
    var i_element = list.i_element
  };

  /// Creates and returns a new vector, populated with the results of calling a provided function on every element in the provided vector
  ///
  /// Example:
  /// ```motoko
  ///
  /// list.add(1);
  ///
  /// let t = List.map<Nat, Text>(list, Nat.toText);
  /// List.toArray(t); // => ["1"]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func map<T1, T2>(list : List<T1>, f : T1 -> T2) : List<T2> = {
    var data_blocks = VarArray.tabulate<[var ?T2]>(
      list.data_blocks.size(),
      func(i) {
        let db = list.data_blocks[i];
        VarArray.tabulate<?T2>(
          db.size(),
          func(j) = switch (db[j]) {
            case (?item) ?f(item);
            case (null) null
          }
        )
      }
    );
    var i_block = list.i_block;
    var i_element = list.i_element
  };

  /// Returns the current number of elements in the vector.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.size(list) // => 0
  /// ```
  ///
  /// Runtime: `O(1)` (with some internal calculations)
  public func size<T>(list : List<T>) : Nat {
    let d = Nat32(list.i_block);
    let i = Nat32(list.i_element);

    // We call all data blocks of the same capacity an "epoch". We number the epochs 0,1,2,...
    // A data block is in epoch e iff the data block has capacity 2 ** e.
    // Each epoch starting with epoch 1 spans exactly two super blocks.
    // Super block s falls in epoch ceil(s/2).

    // epoch of last data block
    // e = 32 - lz
    let lz = leadingZeros(d / 3);

    // capacity of all prior epochs combined
    // capacity_before_e = 2 * 4 ** (e - 1) - 1

    // data blocks in all prior epochs combined
    // blocks_before_e = 3 * 2 ** (e - 1) - 2

    // then size = d * 2 ** e + i - c
    // where c = blocks_before_e * 2 ** e - capacity_before_e

    // there can be overflows, but the result is without overflows, so use addWrap and subWrap
    // we don't erase bits by >>, so to use <>> is ok
    Nat((d -% (1 <>> lz)) <>> lz +% i)
  };

  func data_block_size(i_block : Nat) : Nat {
    // formula for the size of given i_block
    // don't call it for i_block == 0
    Nat(1 <>> leadingZeros(Nat32(i_block) / 3))
  };

  func new_index_block_length(i_block : Nat32) : Nat {
    if (i_block <= 1) 2 else {
      let s = 30 - leadingZeros(i_block);
      Nat(((i_block >> s) +% 1) << s)
    }
  };

  func grow_index_block_if_needed<T>(list : List<T>) {
    if (list.data_blocks.size() == list.i_block) {
      let new_blocks = VarArray.repeat<[var ?T]>([var], new_index_block_length(Nat32(list.i_block)));
      var i = 0;
      while (i < list.i_block) {
        new_blocks[i] := list.data_blocks[i];
        i += 1
      };
      list.data_blocks := new_blocks
    }
  };

  func shrink_index_block_if_needed<T>(list : List<T>) {
    let i_block = Nat32(list.i_block);
    // kind of index of the first block in the super block
    if ((i_block << leadingZeros(i_block)) << 2 == 0) {
      let new_length = new_index_block_length(i_block);
      if (new_length < list.data_blocks.size()) {
        let new_blocks = VarArray.repeat<[var ?T]>([var], new_length);
        var i = 0;
        while (i < new_length) {
          new_blocks[i] := list.data_blocks[i];
          i += 1
        };
        list.data_blocks := new_blocks
      }
    }
  };

  /// Adds a single element to the end of a List,
  /// allocating a new internal data block if needed,
  /// and resizing the internal index block if needed.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 0); // add 0 to vector
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.toArray(list) // => [0, 1, 2, 3]
  /// ```
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  public func add<T>(list : List<T>, element : T) {
    var i_element = list.i_element;
    if (i_element == 0) {
      grow_index_block_if_needed(list);
      let i_block = list.i_block;

      // When removing last we keep one more data block, so can be not empty
      if (list.data_blocks[i_block].size() == 0) {
        list.data_blocks[i_block] := VarArray.repeat<?T>(
          null,
          data_block_size(i_block)
        )
      }
    };

    let last_data_block = list.data_blocks[list.i_block];

    last_data_block[i_element] := ?element;

    i_element += 1;
    if (i_element == last_data_block.size()) {
      i_element := 0;
      list.i_block += 1
    };
    list.i_element := i_element
  };

  /// Removes and returns the last item in the vector or `null` if
  /// the vector is empty.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.removeLast(list); // => ?11
  /// ```
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  ///
  /// Amortized Space: `O(1)`, Worst Case Space: `O(sqrt(n))`
  public func removeLast<T>(list : List<T>) : ?T {
    var i_element = list.i_element;
    if (i_element == 0) {
      shrink_index_block_if_needed(list);

      var i_block = list.i_block;
      if (i_block == 1) {
        return null
      };
      i_block -= 1;
      i_element := list.data_blocks[i_block].size();

      // Keep one totally empty block when removing
      if (i_block + 2 < list.data_blocks.size()) {
        if (list.data_blocks[i_block + 2].size() == 0) {
          list.data_blocks[i_block + 2] := [var]
        }
      };
      list.i_block := i_block
    };
    i_element -= 1;

    var last_data_block = list.data_blocks[list.i_block];

    let element = last_data_block[i_element];
    last_data_block[i_element] := null;

    list.i_element := i_element;
    return element
  };

  func locate(index : Nat) : (Nat, Nat) {
    // see comments in tests
    let i = Nat32(index);
    let lz = leadingZeros(i);
    let lz2 = lz >> 1;
    if (lz & 1 == 0) {
      (Nat(((i << lz2) >> 16) ^ (0x10000 >> lz2)), Nat(i & (0xFFFF >> lz2)))
    } else {
      (Nat(((i << lz2) >> 15) ^ (0x18000 >> lz2)), Nat(i & (0x7FFF >> lz2)))
    }
  };

  /// Returns the element at index `index`. Indexing is zero-based.
  /// Traps if `index >= size`, error message may not be descriptive.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.get(list, 0); // => 10
  /// ```
  ///
  /// Runtime: `O(1)`
  public func get<T>(list : List<T>, index : Nat) : T {
    // inlined version of:
    //   let (a,b) = locate(index);
    //   switch(list.data_blocks[a][b]) {
    //     case (?element) element;
    //     case (null) Prim.trap "";
    //   };
    let i = Nat32(index);
    let lz = leadingZeros(i);
    let lz2 = lz >> 1;
    switch (
      if (lz & 1 == 0) {
        list.data_blocks[Nat(((i << lz2) >> 16) ^ (0x10000 >> lz2))][Nat(i & (0xFFFF >> lz2))]
      } else {
        list.data_blocks[Nat(((i << lz2) >> 15) ^ (0x18000 >> lz2))][Nat(i & (0x7FFF >> lz2))]
      }
    ) {
      case (?result) return result;
      case (_) Prim.trap "List index out of bounds in get"
    }
  };

  /// Returns the element at index `index` as an option.
  /// Returns `null` when `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// let x = List.getOpt(list, 0); // => ?10
  /// let y = List.getOpt(list, 2); // => null
  /// ```
  ///
  /// Runtime: `O(1)`
  public func getOpt<T>(list : List<T>, index : Nat) : ?T {
    let (a, b) = locate(index);
    if (a < list.i_block or list.i_element != 0 and a == list.i_block) {
      list.data_blocks[a][b]
    } else {
      null
    }
  };

  /// Overwrites the current element at `index` with `element`. Traps if
  /// `index` >= size. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 10);
  /// List.put(list, 0, 20); // overwrites 10 at index 0 with 20
  /// List.toArray(list) // => [20]
  /// ```
  ///
  /// Runtime: `O(1)`
  public func put<T>(list : List<T>, index : Nat, value : T) {
    let (a, b) = locate(index);
    if (a < list.i_block or a == list.i_block and b < list.i_element) {
      list.data_blocks[a][b] := ?value
    } else Prim.trap "List index out of bounds in put"
  };

  /// Sorts the elements in the vector according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 3);
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.sort(list, Nat.compare);
  /// List.toArray(list) // => [1, 2, 3]
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sort<T>(list : List<T>, compare : (T, T) -> Order.Order) {
    if (size(list) < 2) return;
    let arr = toVarArray(list);
    VarArray.sortInPlace(arr, compare);
    for (i in arr.keys()) {
      put(list, i, arr[i])
    }
  };

  /// Finds the first index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.new<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.indexOf<Nat>(3, list, Nat.equal); // => ?2
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func indexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat {
    // inlining would save 10 instructions per entry
    firstIndexWith<T>(list, func(x) = equal(element, x))
  };

  /// Finds the last index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.new<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  /// List.add(list, 2);
  /// List.add(list, 2);
  ///
  /// List.lastIndexOf<Nat>(2, list, Nat.equal); // => ?5
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func lastIndexOf<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : ?Nat {
    // inlining would save 10 instructions per entry
    lastIndexWith<T>(list, func(x) = equal(element, x))
  };

  /// Finds the index of the first element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.new<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.firstIndexWith<Nat>(list, func(i) { i % 2 == 0 }); // => ?1
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func firstIndexWith<T>(list : List<T>, predicate : T -> Bool) : ?Nat {
    let blocks = list.data_blocks.size();
    var i_block = 0;
    var i_element = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    loop {
      if (i_element == size) {
        i_block += 1;
        if (i_block >= blocks) return null;
        db := list.data_blocks[i_block];
        size := db.size();
        if (size == 0) return null;
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) if (predicate(x)) return ?i;
        case (_) return null
      };
      i_element += 1;
      i += 1
    }
  };

  /// Finds the index of the last element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.new<Nat>();
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.lastIndexWith<Nat>(list, func(i) { i % 2 == 0 }); // => ?3
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func lastIndexWith<T>(list : List<T>, predicate : T -> Bool) : ?Nat {
    var i = size(list);
    var i_block = list.i_block;
    var i_element = list.i_element;
    var db : [var ?T] = if (i_block < list.data_blocks.size()) {
      list.data_blocks[i_block]
    } else { [var] };

    loop {
      if (i_block == 1) {
        return null
      };
      if (i_element == 0) {
        i_block -= 1;
        db := list.data_blocks[i_block];
        i_element := db.size() - 1
      } else {
        i_element -= 1
      };
      switch (db[i_element]) {
        case (?x) {
          i -= 1;
          if (predicate(x)) return ?i
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Returns true iff every element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `true`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.forAll<Nat>(list, func x { x > 1 }); // => true
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forAll<T>(list : List<T>, predicate : T -> Bool) : Bool {
    not forSome<T>(list, func(x) : Bool = not predicate(x))
  };

  /// Returns true iff some element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `false`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.forSome<Nat>(list, func x { x > 3 }); // => true
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forSome<T>(list : List<T>, predicate : T -> Bool) : Bool {
    switch (firstIndexWith(list, predicate)) {
      case (null) false;
      case (_) true
    }
  };

  /// Returns true iff no element in `list` satisfies `predicate`.
  /// This is logically equivalent to that all elements in `list` satisfy `not predicate`.
  /// In particular, if `list` is empty the function returns `true`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 2);
  /// List.add(list, 3);
  /// List.add(list, 4);
  ///
  /// List.forNone<Nat>(list, func x { x == 0 }); // => true
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func forNone<T>(list : List<T>, predicate : T -> Bool) : Bool = not forSome(list, predicate);

  /// Returns an Iterator (`Iter`) over the elements of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  ///
  /// var sum = 0;
  /// for (element in List.vals(list)) {
  ///   sum += element;
  /// };
  /// sum // => 33
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func vals<T>(list : List<T>) : Iter.Iter<T> = vals_(list);

  /// Returns an Iterator (`Iter`) over the items, i.e. pairs of value and index of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// Iter.toArray(List.items(list)); // [(10, 0), (11, 1), (12, 2)]
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(T, Nat).
  public func items<T>(list : List<T>) : Iter.Iter<(T, Nat)> = object {
    let blocks = list.data_blocks.size();
    var i_block = 0;
    var i_element = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    public func next() : ?(T, Nat) {
      if (i_element == size) {
        i_block += 1;
        if (i_block >= blocks) return null;
        db := list.data_blocks[i_block];
        size := db.size();
        if (size == 0) return null;
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          let ret = ?(x, i);
          i_element += 1;
          i += 1;
          return ret
        };
        case (_) return null
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the elements of a List in reverse order.
  /// Iterator provides a single method `next()`, which returns
  /// elements in reverse order, or `null` when out of elements to iterate over.
  ///
  /// ```
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  ///
  /// var sum = 0;
  /// for (element in List.vals(list)) {
  ///   sum += element;
  /// };
  /// sum // => 33
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func valsRev<T>(list : List<T>) : Iter.Iter<T> = object {
    var i_block = list.i_block;
    var i_element = list.i_element;
    var db : [var ?T] = if (i_block < list.data_blocks.size()) {
      list.data_blocks[i_block]
    } else { [var] };

    public func next() : ?T {
      if (i_block == 1) {
        return null
      };
      if (i_element == 0) {
        i_block -= 1;
        db := list.data_blocks[i_block];
        i_element := db.size() - 1
      } else {
        i_element -= 1
      };

      db[i_element]
    }
  };

  /// Returns an Iterator (`Iter`) over the items in reverse order, i.e. pairs of value and index of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in reverse order, or `null` when out of elements to iterate over.
  ///
  /// ```
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// Iter.toArray(List.items(list)); // [(12, 0), (11, 1), (10, 2)]
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(T, Nat).
  public func itemsRev<T>(list : List<T>) : Iter.Iter<(T, Nat)> = object {
    var i = size(list);
    var i_block = list.i_block;
    var i_element = list.i_element;
    var db : [var ?T] = if (i_block < list.data_blocks.size()) {
      list.data_blocks[i_block]
    } else { [var] };

    public func next() : ?(T, Nat) {
      if (i_block == 1) {
        return null
      };
      if (i_element == 0) {
        i_block -= 1;
        db := list.data_blocks[i_block];
        i_element := db.size() - 1
      } else {
        i_element -= 1
      };
      switch (db[i_element]) {
        case (?x) {
          i -= 1;
          return ?(x, i)
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the keys (indices) of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```
  ///
  /// List.add(list, 10);
  /// List.add(list, 11);
  /// List.add(list, 12);
  /// Iter.toArray(List.items(list)); // [0, 1, 2]
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  public func keys<T>(list : List<T>) : Iter.Iter<Nat> = range(0, size(list));

  /// Creates a List containing elements from `iter`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.vals();
  ///
  /// let list = List.fromIter<Nat>(iter); // => [1, 1, 1]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromIter<T>(iter : Iter.Iter<T>) : List<T> {
    let list = new<T>();
    for (element in iter) add(list, element);
    list
  };

  /// Adds elements to a List from `iter`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.vals();
  /// let list = List.init<Nat>(1, 2);
  ///
  /// let list = List.addFromIter<Nat>(list, iter); // => [2, 1, 1, 1]
  /// ```
  ///
  /// Runtime: `O(size)`, where n is the size of iter.
  public func addFromIter<T>(list : List<T>, iter : Iter.Iter<T>) {
    for (element in iter) add(list, element)
  };

  /// Creates an immutable array containing elements from a List.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  ///
  /// List.toArray<Nat>(list); // => [1, 2, 3]
  ///
  /// ```
  ///
  /// Runtime: `O(size)`
  public func toArray<T>(list : List<T>) : [T] = Array.tabulate<T>(size(list), vals_(list).unsafe_next_i);

  private func vals_<T>(list : List<T>) : {
    next : () -> ?T;
    unsafe_next : () -> T;
    unsafe_next_i : Nat -> T
  } = object {
    let blocks = list.data_blocks.size();
    var i_block = 0;
    var i_element = 0;
    var db_size = 0;
    var db : [var ?T] = [var];

    public func next() : ?T {
      if (i_element == db_size) {
        i_block += 1;
        if (i_block >= blocks) return null;
        db := list.data_blocks[i_block];
        db_size := db.size();
        if (db_size == 0) return null;
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          i_element += 1;
          return ?x
        };
        case (_) return null
      }
    };

    // version of next() without option type
    // inlined version of
    //   public func unsafe_next() : T = {
    //     let ?x = next() else Prim.trap(INTERNAL_ERROR);
    //     x;
    //   };
    public func unsafe_next() : T {
      if (i_element == db_size) {
        i_block += 1;
        if (i_block >= blocks) Prim.trap(INTERNAL_ERROR);
        db := list.data_blocks[i_block];
        db_size := db.size();
        if (db_size == 0) Prim.trap(INTERNAL_ERROR);
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          i_element += 1;
          return x
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    };

    // version of next() without option type and throw-away argument
    // inlined version of
    //   public func unsafe_next_(i : Nat) : T = unsafe_next();
    public func unsafe_next_i(i : Nat) : T {
      if (i_element == db_size) {
        i_block += 1;
        if (i_block >= blocks) Prim.trap(INTERNAL_ERROR);
        db := list.data_blocks[i_block];
        db_size := db.size();
        if (db_size == 0) Prim.trap(INTERNAL_ERROR);
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          i_element += 1;
          return x
        };
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Creates a List containing elements from an Array.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [2, 3];
  ///
  /// let list = List.fromArray<Nat>(array); // => [2, 3]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromArray<T>(array : [T]) : List<T> {
    let (i_block, i_element) = locate(array.size());

    let blocks = new_index_block_length(Nat32(if (i_element == 0) { i_block - 1 } else i_block));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    var pos = 0;

    func make_block(len : Nat, fill : Nat) : [var ?T] {
      let block = VarArray.repeat<?T>(null, len);
      var j = 0;
      while (j < fill) {
        block[j] := ?array[pos];
        j += 1;
        pos += 1
      };
      block
    };

    while (i < i_block) {
      let len = data_block_size(i);
      data_blocks[i] := make_block(len, len);
      i += 1
    };
    if (i_element != 0 and i_block < blocks) {
      data_blocks[i] := make_block(data_block_size(i), i_element)
    };

    {
      var data_blocks = data_blocks;
      var i_block = i_block;
      var i_element = i_element
    };

  };

  /// Creates a mutable Array containing elements from a List.
  ///
  /// Example:
  /// ```motoko
  ///
  /// List.add(list, 1);
  /// List.add(list, 2);
  /// List.add(list, 3);
  ///
  /// List.toVarArray<Nat>(list); // => [1, 2, 3]
  ///
  /// ```
  ///
  /// Runtime: `O(size)`
  public func toVarArray<T>(list : List<T>) : [var T] {
    let s = size(list);
    if (s == 0) return [var];
    let arr = VarArray.repeat<T>(first(list), s);
    var i = 0;
    let next = vals_(list).unsafe_next;
    while (i < s) {
      arr[i] := next();
      i += 1
    };
    arr
  };

  /// Creates a List containing elements from a mutable Array.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let array = [var 2, 3];
  ///
  /// let list = List.fromVarArray<Nat>(array); // => [2, 3]
  /// ```
  ///
  /// Runtime: `O(size)`
  public func fromVarArray<T>(array : [var T]) : List<T> {
    let (i_block, i_element) = locate(array.size());

    let blocks = new_index_block_length(Nat32(if (i_element == 0) { i_block - 1 } else i_block));
    let data_blocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    var pos = 0;

    func make_block(len : Nat, fill : Nat) : [var ?T] {
      let block = VarArray.repeat<?T>(null, len);
      var j = 0;
      while (j < fill) {
        block[j] := ?array[pos];
        j += 1;
        pos += 1
      };
      block
    };

    while (i < i_block) {
      let len = data_block_size(i);
      data_blocks[i] := make_block(len, len);
      i += 1
    };
    if (i_element != 0 and i_block < blocks) {
      data_blocks[i] := make_block(data_block_size(i), i_element)
    };

    {
      var data_blocks = data_blocks;
      var i_block = i_block;
      var i_element = i_element
    };

  };

  /// Returns the first element of `list`. Traps if `list` is empty.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.init<Nat>(10, 1);
  ///
  /// List.first(list); // => 1
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func first<T>(list : List<T>) : T {
    let ?x = list.data_blocks[1][0] else Prim.trap "List index out of bounds in first";
    x
  };

  /// Returns the last element of `list`. Traps if `list` is empty.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.last(list); // => 3
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func last<T>(list : List<T>) : T {
    let e = list.i_element;
    if (e > 0) {
      let ?x = list.data_blocks[list.i_block][e - 1] else Prim.trap(INTERNAL_ERROR);
      return x
    };
    let ?x = list.data_blocks[list.i_block - 1][0] else Prim.trap "List index out of bounds in first";
    return x
  };

  /// Applies `f` to each element in `list`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.iterate<Nat>(list, func (x) {
  ///   Debug.print(Nat.toText(x)); // prints each element in vector
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func iterate<T>(list : List<T>, f : T -> ()) {
    let blocks = list.data_blocks.size();
    var i_block = 0;
    var i_element = 0;
    var size = 0;
    var db : [var ?T] = [var];

    loop {
      if (i_element == size) {
        i_block += 1;
        if (i_block >= blocks) return;
        db := list.data_blocks[i_block];
        size := db.size();
        if (size == 0) return;
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          f(x);
          i_element += 1
        };
        case (_) return
      }
    }
  };

  /// Applies `f` to each item `(i, x)` in `list` where `i` is the key
  /// and `x` is the value.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.iterateItems<Nat>(list, func (i,x) {
  ///   // prints each item (i,x) in vector
  ///   Debug.print(Nat.toText(i) # Nat.toText(x));
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func iterateItems<T>(list : List<T>, f : (Nat, T) -> ()) {
    /* Inlined version of
      let o = object {
        var i = 0;
        public func fx(x : T) { f(i, x); i += 1; };
      };
      iterate<T>(list, o.fx);
    */
    let blocks = list.data_blocks.size();
    var i_block = 0;
    var i_element = 0;
    var size = 0;
    var db : [var ?T] = [var];
    var i = 0;

    loop {
      if (i_element == size) {
        i_block += 1;
        if (i_block >= blocks) return;
        db := list.data_blocks[i_block];
        size := db.size();
        if (size == 0) return;
        i_element := 0
      };
      switch (db[i_element]) {
        case (?x) {
          f(i, x);
          i_element += 1;
          i += 1
        };
        case (_) return
      }
    }
  };

  /// Like `iterateItems` but iterates through the vector in reverse order,
  /// from end to beginning.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.iterateItemsRev<Nat>(list, func (i,x) {
  ///   // prints each item (i,x) in vector
  ///   Debug.print(Nat.toText(i) # Nat.toText(x));
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func iterateItemsRev<T>(list : List<T>, f : (Nat, T) -> ()) {
    var i_block = list.i_block;
    var i_element = list.i_element;
    var db : [var ?T] = if (i_block < list.data_blocks.size()) {
      list.data_blocks[i_block]
    } else { [var] };
    var i = size(list);

    loop {
      if (i_block == 1) {
        return
      };
      if (i_element == 0) {
        i_block -= 1;
        db := list.data_blocks[i_block];
        i_element := db.size() - 1
      } else {
        i_element -= 1
      };
      i -= 1;
      switch (db[i_element]) {
        case (?x) f(i, x);
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Applies `f` to each element in `list` in reverse order.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// List.iterate<Nat>(list, func (x) {
  ///   Debug.print(Nat.toText(x)); // prints each element in vector in reverse order
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func iterateRev<T>(list : List<T>, f : T -> ()) {
    var i_block = list.i_block;
    var i_element = list.i_element;
    var db : [var ?T] = if (i_block < list.data_blocks.size()) {
      list.data_blocks[i_block]
    } else { [var] };

    loop {
      if (i_block == 1) {
        return
      };
      if (i_element == 0) {
        i_block -= 1;
        db := list.data_blocks[i_block];
        i_element := db.size() - 1
      } else {
        i_element -= 1
      };
      switch (db[i_element]) {
        case (?x) f(x);
        case (_) Prim.trap(INTERNAL_ERROR)
      }
    }
  };

  /// Returns true if List contains element with respect to equality
  /// defined by `equal`.
  ///
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// List.add(list, 2);
  /// List.add(list, 0);
  /// List.add(list, 3);
  ///
  /// List.contains<Nat>(list, 2, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func contains<T>(list : List<T>, equal : (T, T) -> Bool, element : T) : Bool {
    Option.isSome(indexOf(list, equal, element))
  };

  /// Finds the greatest element in `list` defined by `compare`.
  /// Returns `null` if `list` is empty.
  ///
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// List.add(list, 1);
  /// List.add(list, 2);
  ///
  /// List.max<Nat>(list, Nat.compare); // => ?2
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func max<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T {
    if (size(list) == 0) return null;

    var maxSoFar = get(list, 0);
    iterate<T>(
      list,
      func(x) = switch (compare(x, maxSoFar)) {
        case (#greater) maxSoFar := x;
        case _ {}
      }
    );

    return ?maxSoFar
  };

  /// Finds the least element in `list` defined by `compare`.
  /// Returns `null` if `list` is empty.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// List.add(list, 1);
  /// List.add(list, 2);
  ///
  /// List.min<Nat>(list, Nat.compare); // => ?1
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func min<T>(list : List<T>, compare : (T, T) -> Order.Order) : ?T {
    if (size(list) == 0) return null;

    var minSoFar = get(list, 0);
    iterate<T>(
      list,
      func(x) = switch (compare(x, minSoFar)) {
        case (#less) minSoFar := x;
        case _ {}
      }
    );

    return ?minSoFar
  };

  /// Defines equality for two vectors, using `equal` to recursively compare elements in the
  /// vectors. Returns true iff the two vectors are of the same size, and `equal`
  /// evaluates to true for every pair of elements in the two vectors of the same
  /// index.
  ///
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([1,2]);
  /// let list2 = List.new<Nat>();
  /// list2.add(1);
  /// list2.add(2);
  ///
  /// List.equal<Nat>(list1, list2, Nat.equal); // => true
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<T>(list1 : List<T>, list2 : List<T>, equal : (T, T) -> Bool) : Bool {
    let size1 = size(list1);

    if (size1 != size(list2)) return false;

    let next1 = vals_(list1).unsafe_next;
    let next2 = vals_(list2).unsafe_next;
    var i = 0;
    while (i < size1) {
      if (not equal(next1(), next2())) return false;
      i += 1
    };

    return true
  };

  /// Defines comparison for two vectors, using `compare` to recursively compare elements in the
  /// vectors. Comparison is defined lexicographically.
  ///
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([1,2]);
  /// let list2 = List.new<Nat>();
  /// list2.add(1);
  /// list2.add(2);
  ///
  /// List.compare<Nat>(list1, list2, Nat.compare); // => #less
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func compare<T>(list1 : List<T>, list2 : List<T>, compare_fn : (T, T) -> Order.Order) : Order.Order {
    let size1 = size(list1);
    let size2 = size(list2);
    let minSize = if (size1 < size2) { size1 } else { size2 };

    let next1 = vals_(list1).unsafe_next;
    let next2 = vals_(list2).unsafe_next;
    var i = 0;
    while (i < minSize) {
      switch (compare_fn(next1(), next2())) {
        case (#less) return #less;
        case (#greater) return #greater;
        case _ {}
      };
      i += 1
    };

    return natCompare(size1, size2)
  };

  /// Creates a textual representation of `list`, using `toText` to recursively
  /// convert the elements into Text.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3,4]);
  ///
  /// List.toText<Nat>(list, Nat.toText); // => "[1, 2, 3, 4]"
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `toText` runs in O(1) time and space.
  public func toText<T>(list : List<T>, toText_fn : T -> Text) : Text {
    let vsize : Int = size(list);
    let next = vals_(list).unsafe_next;
    var i = 0;
    var text = "";
    while (i < vsize - 1) {
      text := text # toText_fn(next()) # ", "; // Text implemented as rope
      i += 1
    };
    if (vsize > 0) {
      // avoid the trailing comma
      text := text # toText_fn(get<T>(list, i))
    };

    "[" # text # "]"
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// List.foldLeft<Text, Nat>(list, "", func (acc, x) { acc # Nat.toText(x)}); // => "123"
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldLeft<A, T>(list : List<T>, base : A, combine : (A, T) -> A) : A {
    var accumulation = base;

    iterate<T>(
      list,
      func(x) = accumulation := combine(accumulation, x)
    );

    accumulation
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// List.foldRight<Nat, Text>(list, "", func (x, acc) { Nat.toText(x) # acc }); // => "123"
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldRight<T, A>(list : List<T>, base : A, combine : (T, A) -> A) : A {
    var accumulation = base;

    iterateRev<T>(
      list,
      func(x) = accumulation := combine(x, accumulation)
    );

    accumulation
  };

  /// Returns a new vector with capacity and size 1, containing `element`.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.make<Nat>(1);
  /// List.toText<Nat>(list, Nat.toText); // => "[1]"
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func make<T>(element : T) : List<T> = init(1, element);

  /// Reverses the order of elements in `list` by overwriting in place.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// List.reverse<Nat>(list);
  /// List.toText<Nat>(list, Nat.toText); // => "[3, 2, 1]"
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reverse<T>(list : List<T>) {
    let vsize = size(list);
    if (vsize == 0) return;

    var i = 0;
    var j = vsize - 1 : Nat;
    var temp = get(list, 0);
    while (i < vsize / 2) {
      temp := get(list, j);
      put(list, j, get(list, i));
      put(list, i, temp);
      i += 1;
      j -= 1
    }
  };

  /// Reverses the order of elements in `list` and returns a new
  /// List.
  ///
  /// Example:
  /// ```motoko
  ///
  /// import Nat "mo:base/Nat";
  ///
  /// let list = List.fromArray<Nat>([1,2,3]);
  ///
  /// let rlist = List.reversed<Nat>(list);
  /// List.toText<Nat>(rlist, Nat.toText); // => "[3, 2, 1]"
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reversed<T>(list : List<T>) : List<T> {
    let rlist = new<T>();

    iterateRev<T>(
      list,
      func(x) = add(rlist, x)
    );

    rlist
  };

  /// Returns true if and only if the vector is empty.
  ///
  /// Example:
  /// ```motoko
  ///
  /// let list = List.fromArray<Nat>([2,0,3]);
  /// List.isEmpty<Nat>(list); // => false
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func isEmpty<T>(list : List<T>) : Bool {
    list.i_block == 1 and list.i_element == 0
  }
}
