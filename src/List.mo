/// A mutable growable array data structure with efficient random access and dynamic resizing.
/// `List` provides O(1) access time and O(sqrt(n)) memory overhead. In contrast, `pure/List` is a purely functional linked list.
/// Can be declared `stable` for orthogonal persistence.
///
/// The List size is in practice limited only by available memory. The
/// theoretical limit is 2^61 elements.
///
/// Performance note: on a 64-bit build (enhanced orthogonal persistence, the
/// compiler default) all index and size arithmetic in this module operates on
/// unboxed values, so there are no boxing-related performance cliffs or
/// hidden allocations.
/// On a 32-bit build (deprecated legacy persistence) `Nat` values >= 2^30 are
/// boxed, so an operation receiving such an index, or internally computing
/// such a size, would take slower bignum code paths and allocate a few dozen
/// bytes per call. This is theoretical only: a 32-bit heap is limited to
/// 4 GB and a list occupies at least 4 bytes per element, so a list can
/// never actually grow to 2^30 elements there, and indices >= 2^30 can only
/// occur as out-of-range arguments.
///
/// This implementation is adapted with permission from the `vector` Mops package created by Research AG.
///
/// Copyright: 2023 MR Research AG
/// Main author: Andrii Stepanov (AStepanov25)
/// Contributors: Timo Hanke (timohanke), Andy Gura (andygura), react0r-com
///
/// ```motoko name=import
/// import List "mo:core/List";
/// ```

import PureList "pure/List";
import Prim "mo:⛔";
import Nat32 "Nat32";
import Nat64 "Nat64";
import Array "Array";
import Nat "Nat";
import Option "Option";
import VarArray "VarArray";
import Types "Types";

module {
  /// `List<T>` provides a mutable list of elements of type `T`.
  /// Based on the paper "Resizable Arrays in Optimal Time and Space" by Brodnik, Carlsson, Demaine, Munro and Sedgewick (1999).
  /// Since this is internally a two-dimensional array the access times for put and get operations
  /// will naturally be 2x slower than Buffer and Array. However, Array is not resizable and Buffer
  /// has `O(size)` memory waste.
  ///
  /// The List size is in practice limited only by available memory; the
  /// theoretical limit is 2^61 elements.
  public type List<T> = Types.List<T>;

  let INTERNAL_ERROR = "List: internal error";

  /// Creates a new empty List for elements of type T.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>(); // Creates a new List
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func empty<T>() : List<T> = {
    // the first block is always empty and is present in each List
    // this is done to optimize locate, at, get, etc
    var blocks = [var [var]];
    // can't be 0 in any List
    var blockIndex = 1;
    var elementIndex = 0
  };

  /// Returns a new list with capacity and size 1, containing `element`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.singleton<Nat>(1);
  /// assert list.toText(Nat.toText) == "List[1]";
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func singleton<T>(element : T) : List<T> = {
    var blockIndex = 2;
    var blocks = [var [var], [var ?element]];
    var elementIndex = 0
  };

  func repeatInternal<T>(initValue : ?T, size : Nat) : List<T> {
    let (blockIndex, elementIndex) = locate(size);

    let blocks = newIndexBlockLength(Nat.toNat32(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let dataBlocks = VarArray.repeat<[var ?T]>([var], blocks);
    var i = 1;
    while (i < blockIndex) {
      dataBlocks[i] := VarArray.repeat(initValue, dataBlockSize(i));
      i += 1
    };
    if (elementIndex != 0) {
      let db = VarArray.repeat<?T>(null, dataBlockSize(blockIndex));
      if (not Option.isNull(initValue)) {
        // Nat64 position, see nextIndexOf
        var j : Nat64 = 0;
        let stop = Nat64.fromIntWrap(elementIndex);
        while (j < stop) {
          db[j.toNat()] := initValue;
          j +%= 1
        }
      };
      dataBlocks[blockIndex] := db
    };

    {
      var blocks = dataBlocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    }
  };

  /// Creates a new List with `size` copies of the initial value.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.repeat<Nat>(2, 4);
  /// assert list.toArray() == [2, 2, 2, 2];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func repeat<T>(initValue : T, size : Nat) : List<T> = repeatInternal(?initValue, size);

  /// Fills all elements in the list with the given value.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  /// list.fill(0); // fills the list with 0
  /// assert list.toArray() == [0, 0, 0];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func fill<T>(self : List<T>, value : T) {
    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    // wrapped once instead of per element
    let slot = ?value;

    // the data blocks before blockIndex are full
    var i = 1;
    while (i < blockIndex) {
      let db = blocks[i];
      for (j in db.keys()) db[j] := slot;
      i += 1
    };

    let elementIndex = self.elementIndex;
    if (elementIndex != 0) {
      let db = blocks[blockIndex];
      // Nat64 position, see nextIndexOf
      var j : Nat64 = 0;
      let stop = Nat64.fromIntWrap(elementIndex);
      while (j < stop) {
        db[j.toNat()] := slot;
        j +%= 1
      }
    }
  };

  /// Converts a mutable `List` to a purely functional `PureList`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  /// let pureList = list.toPure(); // converts to immutable PureList
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  /// @deprecated M0235
  public func toPure<T>(self : List<T>) : PureList.List<T> {
    var result : PureList.List<T> = null;

    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    let elementIndex = self.elementIndex;

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;

    while (i > 0) {
      let db = blocks[i];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) result := ?(x, result);
          case null Prim.trap INTERNAL_ERROR
        }
      };
      i -= 1
    };

    result
  };

  /// Converts a purely functional `PureList` to a `List`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import PureList "mo:core/pure/List";
  ///
  /// let pureList = PureList.fromArray([1, 2, 3]);
  /// let list = List.fromPure<Nat>(pureList); // converts to List
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  /// @deprecated M0235
  public func fromPure<T>(pure : PureList.List<T>) : List<T> {
    let list = empty<T>();
    // local insertion point, see nextOutBlock
    var out : [var ?T] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;
    var p = pure;
    loop {
      switch (p) {
        case (?(x, xs)) {
          if (e == outSize) {
            out := nextOutBlock(list, outSize);
            outSize := Nat64.fromIntWrap(out.size());
            e := 0
          };
          out[e.toNat()] := ?x;
          e +%= 1;
          p := xs
        };
        case null {
          finishOut(list, e, outSize);
          return list
        }
      }
    }
  };

  func addRepeatInternal<T>(list : List<T>, initValue : ?T, count : Nat) {
    let (b, e) = locate(size(list) + count);
    let blocksCount = newIndexBlockLength(Nat.toNat32(if (e == 0) b - 1 else b));

    let oldBlocksCount = list.blocks.size();
    if (oldBlocksCount < blocksCount) {
      let oldBlocks = list.blocks;
      let blocks = VarArray.repeat<[var ?T]>([var], blocksCount);
      var i = 0;
      while (i < oldBlocksCount) {
        blocks[i] := oldBlocks[i];
        i += 1
      };
      list.blocks := blocks
    };

    let blocks = list.blocks;
    var blockIndex = list.blockIndex;
    var elementIndex = list.elementIndex;

    var cnt = count;
    label L while (cnt > 0) {
      if (blocks[blockIndex].size() == 0) {
        let dbSize = dataBlockSize(blockIndex);
        if (cnt >= dbSize) {
          blocks[blockIndex] := VarArray.repeat(initValue, dbSize);
          blockIndex += 1;
          cnt -= dbSize;
          continue L
        };
        blocks[blockIndex] := VarArray.repeat(null, dbSize)
      };

      let block = blocks[blockIndex];
      let dbSize = block.size();
      let to = Nat.min(elementIndex + cnt, dbSize);
      cnt -= to - elementIndex;

      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(elementIndex);
      let stop = Nat64.fromIntWrap(to);
      while (j < stop) {
        block[j.toNat()] := initValue;
        j +%= 1
      };
      elementIndex := to;

      if (elementIndex == dbSize) {
        elementIndex := 0;
        blockIndex += 1
      }
    };

    list.blockIndex := blockIndex;
    list.elementIndex := elementIndex
  };

  private func reserve<T>(list : List<T>, size : Nat) {
    let blockIndex = list.blockIndex;
    let elementIndex = list.elementIndex;

    addRepeatInternal(list, null, size);

    list.blockIndex := blockIndex;
    list.elementIndex := elementIndex
  };

  /// Add to list `count` copies of the initial value.
  ///
  /// ```motoko include=import
  /// let list = List.repeat<Nat>(2, 4); // [2, 2, 2, 2]
  /// list.addRepeat(2, 1); // [2, 2, 2, 2, 1, 1]
  /// ```
  ///
  /// The List size is in practice limited only by available memory; the
  /// theoretical limit is 2^61 elements.
  ///
  /// Runtime: `O(count)`
  ///
  /// Space: `O(count)`
  public func addRepeat<T>(self : List<T>, initValue : T, count : Nat) = addRepeatInternal(self, ?initValue, count);

  /// Truncates the list to the specified size.
  /// If the new size is larger than the current size, it will do nothing.
  /// If the new size is equal to the current list size, after the operation list will be equal to cloned version of itself.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  /// list.truncate(3); // list is now [1, 2, 3]
  /// assert list.toArray() == [1, 2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func truncate<T>(self : List<T>, newSize : Nat) {
    if (newSize > size(self)) return;

    // if newSize == size(self) then after the operation self will be equal to List.clone(self)
    let (blockIndex, elementIndex) = locate(newSize);
    self.blockIndex := blockIndex;
    self.elementIndex := elementIndex;
    let newBlocksCount = newIndexBlockLength(Nat.toNat32(if (elementIndex == 0) blockIndex - 1 else blockIndex));

    let newBlocks = if (newBlocksCount < self.blocks.size()) {
      let oldDataBlocks = self.blocks;
      self.blocks := VarArray.tabulate(newBlocksCount, func(i) = oldDataBlocks[i]);
      self.blocks
    } else self.blocks;

    var i = if (elementIndex == 0) blockIndex else blockIndex + 1;
    while (i < newBlocksCount) {
      newBlocks[i] := [var];
      i += 1
    };
    if (elementIndex != 0) {
      let block = newBlocks[blockIndex];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(elementIndex);
      let stop = Nat64.fromIntWrap(block.size());
      while (j < stop) {
        block[j.toNat()] := null;
        j +%= 1
      }
    }
  };

  /// Resets the list to size 0, de-referencing all elements.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// list.add(12);
  /// list.clear(); // list is now empty
  /// assert list.toArray() == [];
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func clear<T>(self : List<T>) {
    self.blocks := [var [var]];
    self.blockIndex := 1;
    self.elementIndex := 0
  };

  /// Creates a list of size `size`. Each element at index i
  /// is created by applying `generator` to i.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.tabulate<Nat>(4, func i = i * 2);
  /// assert list.toArray() == [0, 2, 4, 6];
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `generator` runs in O(1) time and space.
  public func tabulate<T>(size : Nat, generator : Nat -> T) : List<T> {
    let (blockIndex, elementIndex) = locate(size);

    let blocks = newIndexBlockLength(Nat.toNat32(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let dataBlocks = VarArray.repeat<[var ?T]>([var], blocks);

    // Each data block is allocated null-filled and then written in a loop
    // over its keys, which the compiler turns into a plain loop: there is
    // no closure call per element as with VarArray.tabulate. The running
    // index is Nat64 so that it advances without bignum-capable Nat
    // arithmetic.
    var i = 1;
    var pos : Nat64 = 0;

    while (i < blockIndex) {
      let db = VarArray.repeat<?T>(null, dataBlockSize(i));
      for (j in db.keys()) {
        db[j] := ?generator(pos.toNat());
        pos +%= 1
      };
      dataBlocks[i] := db;
      i += 1
    };
    if (elementIndex != 0 and blockIndex < blocks) {
      let db = VarArray.repeat<?T>(null, dataBlockSize(blockIndex));
      var j : Nat64 = 0;
      let stop = Nat64.fromIntWrap(elementIndex);
      while (j < stop) {
        db[j.toNat()] := ?generator(pos.toNat());
        j +%= 1;
        pos +%= 1
      };
      dataBlocks[i] := db
    };

    {
      var blocks = dataBlocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    }
  };

  /// Combines a list of lists into a single list. Retains the original
  /// ordering of the elements.
  ///
  /// This has better performance compared to `List.join()`.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let lists = List.fromArray<List.List<Nat>>([
  ///   List.fromArray([0, 1, 2]),
  ///   List.fromArray([2, 3]),
  ///   List.fromArray([]),
  ///   List.fromArray([4])
  /// ]);
  /// let flatList = lists.flatten();
  /// assert flatList.equal(List.fromArray<Nat>([0, 1, 2, 2, 3, 4]), Nat.equal);
  /// ```
  ///
  /// Runtime: O(number of elements in list)
  ///
  /// Space: O(number of elements in list)
  public func flatten<T>(self : List<List<T>>) : List<T> {
    let result = empty<T>();
    // local insertion point, see nextOutBlock
    var out : [var ?T] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();
    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?sublist) {
            let (o, e1, s1) = copyOut(result, sublist, out, e, outSize);
            out := o;
            e := e1;
            outSize := s1
          };
          case null break l
        }
      };
      i += 1
    };

    finishOut(result, e, outSize);
    result
  };

  /// Combines an iterator of lists into a single list.
  /// Retains the original ordering of the elements.
  ///
  /// Consider using `List.flatten()` for better performance.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let lists = [List.fromArray<Nat>([0, 1, 2]), List.fromArray<Nat>([2, 3]), List.fromArray<Nat>([]), List.fromArray<Nat>([4])];
  /// let joinedList = lists.values().join();
  /// assert joinedList.equal(List.fromArray<Nat>([0, 1, 2, 2, 3, 4]), Nat.equal);
  /// ```
  ///
  /// Runtime: O(number of elements in list)
  ///
  /// Space: O(number of elements in list)
  public func join<T>(self : Types.Iter<List<T>>) : List<T> {
    // ignore-self-type-check
    let result = empty<T>();
    // local insertion point, see nextOutBlock
    var out : [var ?T] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;
    for (list in self) {
      let (o, e1, s1) = copyOut(result, list, out, e, outSize);
      out := o;
      e := e1;
      outSize := s1
    };
    finishOut(result, e, outSize);
    result
  };

  /// Returns a copy of a List, with the same size.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(1);
  ///
  /// let clone = list.clone();
  /// assert clone.toArray() == [1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func clone<T>(self : List<T>) : List<T> = {
    // VarArray.clone initializes each copy without the write barrier that a
    // copy loop would pay per element, which is faster even with a closure
    // call per element
    var blocks = VarArray.tabulate(
      copyIndexBlockLength(self),
      func(i) = self.blocks[i].clone()
    );
    var blockIndex = self.blockIndex;
    var elementIndex = self.elementIndex
  };

  /// Creates a new list by applying the provided function to each element in the input list.
  /// The resulting list has the same size as the input list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.singleton<Nat>(123);
  /// let textList = list.map(Nat.toText);
  /// assert textList.toArray() == ["123"];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func map<T, R>(self : List<T>, f : T -> R) : List<R> {
    let oldBlocks = self.blocks;
    // Building each data block with VarArray.tabulate avoids the write
    // barrier that a store per element would pay, which costs more than
    // the closure call per element. Unused slots stay null.
    let blocks = VarArray.tabulate(
      copyIndexBlockLength(self),
      func i {
        let oldBlock = oldBlocks[i];
        VarArray.tabulate(
          oldBlock.size(),
          func j = switch (oldBlock[j]) {
            case (?item) ?f(item);
            case null null
          }
        )
      }
    );

    {
      var blocks = blocks;
      var blockIndex = self.blockIndex;
      var elementIndex = self.elementIndex
    }
  };

  /// Applies `f` to each element of `list` in place,
  /// retaining the original ordering of elements.
  /// This modifies the original list.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([0, 1, 2, 3]);
  /// list.mapInPlace(func x = x * 3);
  /// assert list.equal(List.fromArray<Nat>([0, 3, 6, 9]), Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapInPlace<T>(self : List<T>, f : T -> T) {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // loops over the keys of a block compile to plain loops, see clone
    var i = 1;
    while (i < blockCount) {
      let db = blocks[i];
      for (j in db.keys()) {
        switch (db[j]) {
          case (?x) db[j] := ?f(x);
          case null return
        }
      };
      i += 1
    }
  };

  /// Creates a new list by applying `f` to each element in `list` and its index.
  /// Retains original ordering of elements.
  ///
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([10, 10, 10, 10]);
  /// let newList = list.mapEntries<Nat, Nat>(func(x, i) = i * x);
  /// assert newList.equal(List.fromArray<Nat>([0, 10, 20, 30]), Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapEntries<T, R>(self : List<T>, f : (T, Nat) -> R) : List<R> {
    let blocksCount = copyIndexBlockLength(self);
    let blocks = VarArray.repeat<[var ?R]>([var], blocksCount);
    let oldBlocks = self.blocks;

    // the running index is Nat64, see nextIndexOf
    var index : Nat64 = 0;

    var i = 1;
    label l while (i < blocksCount) {
      let oldBlock = oldBlocks[i];
      let newBlock = VarArray.repeat<?R>(null, oldBlock.size());
      blocks[i] := newBlock;
      for (j in oldBlock.keys()) {
        switch (oldBlock[j]) {
          case (?item) newBlock[j] := ?f(item, index.toNat());
          case null break l
        };
        index +%= 1
      };
      i += 1
    };

    {
      var blocks = blocks;
      var blockIndex = self.blockIndex;
      var elementIndex = self.elementIndex
    }
  };

  /// Creates a new list by applying `f` to each element in `list`.
  /// If any invocation of `f` produces an `#err`, returns an `#err`. Otherwise
  /// returns an `#ok` containing the new list.
  ///
  /// ```motoko include=import
  /// import Result "mo:core/Result";
  ///
  /// let list = List.fromArray<Nat>([4, 3, 2, 1, 0]);
  /// // divide 100 by every element in the list
  /// let result = list.mapResult<Nat, Nat, Text>(func x {
  ///   if (x > 0) {
  ///     #ok(100 / x)
  ///   } else {
  ///     #err "Cannot divide by zero"
  ///   }
  /// });
  /// assert Result.isErr(result);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func mapResult<T, R, E>(self : List<T>, f : T -> Types.Result<R, E>) : Types.Result<List<R>, E> {
    var error : ?E = null;

    let oldBlocks = self.blocks;
    // data blocks are built with VarArray.tabulate, see map
    let blocks = VarArray.tabulate(
      copyIndexBlockLength(self),
      func i {
        let oldBlock = oldBlocks[i];
        VarArray.tabulate(
          oldBlock.size(),
          func j = switch (oldBlock[j]) {
            case (?item) switch (f(item)) {
              case (#ok x) ?x;
              case (#err e) {
                // only the first error is kept
                if (Option.isNull(error)) error := ?e;
                null
              }
            };
            case null null
          }
        )
      }
    );

    switch (error) {
      case (null) #ok {
        var blocks = blocks;
        var blockIndex = self.blockIndex;
        var elementIndex = self.elementIndex
      };
      case (?e) #err e
    }
  };

  /// Returns a new list containing only the elements from `list` for which the predicate returns true.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// let evenNumbers = list.filter(func x = x % 2 == 0);
  /// assert evenNumbers.toArray() == [2, 4];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func filter<T>(self : List<T>, predicate : T -> Bool) : List<T> {
    let filtered = empty<T>();
    // local insertion point, see nextOutBlock
    var out : [var ?T] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();
    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) if (predicate(x)) {
            if (e == outSize) {
              out := nextOutBlock(filtered, outSize);
              outSize := Nat64.fromIntWrap(out.size());
              e := 0
            };
            // the stored option is reused as is
            out[e.toNat()] := slot;
            e +%= 1
          };
          case null break l
        }
      };
      i += 1
    };

    finishOut(filtered, e, outSize);
    filtered
  };

  /// Retains only the elements in `list` for which the predicate returns true.
  /// Modifies the original list in place.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// list.retain(func x = x % 2 == 0);
  /// assert list.toArray() == [2, 4];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(sqrt(size))` if `list` was truncated otherwise `O(1)`
  public func retain<T>(self : List<T>, predicate : T -> Bool) {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // The kept elements are compacted to the front, in place: the write
    // cursor (wb, we) in data block `out` never passes the read position,
    // so every block it moves to is already allocated.
    var wb = 1;
    var out : [var ?T] = if (blockCount > 1) blocks[1] else [var];
    var we : Nat64 = 0;
    var outSize = Nat64.fromIntWrap(out.size());

    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) if (predicate(x)) {
            if (we == outSize) {
              wb += 1;
              out := blocks[wb];
              outSize := Nat64.fromIntWrap(out.size());
              we := 0
            };
            out[we.toNat()] := slot;
            we +%= 1
          };
          case null break l
        }
      };
      i += 1
    };

    setInsertionPoint(self, wb, we, outSize);
    // clears the slots after the kept elements and shrinks the index block
    truncate(self, size(self))
  };

  /// Returns a new list containing all elements from `list` for which the function returns ?element.
  /// Discards all elements for which the function returns null.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// let doubled = list.filterMap<Nat, Nat>(func x = if (x % 2 == 0) ?(x * 2) else null);
  /// assert doubled.toArray() == [4, 8];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in `O(1)` time and space.
  public func filterMap<T, R>(self : List<T>, f : T -> ?R) : List<R> {
    let filtered = empty<R>();
    // local insertion point, see nextOutBlock
    var out : [var ?R] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();
    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) switch (f(x)) {
            case (?y) {
              if (e == outSize) {
                out := nextOutBlock(filtered, outSize);
                outSize := Nat64.fromIntWrap(out.size());
                e := 0
              };
              out[e.toNat()] := ?y;
              e +%= 1
            };
            case null {}
          };
          case null break l
        }
      };
      i += 1
    };

    finishOut(filtered, e, outSize);
    filtered
  };

  /// Creates a new list by applying `k` to each element in `list`,
  /// and concatenating the resulting iterators in order.
  ///
  /// ```motoko include=import
  /// import Int "mo:core/Int"
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  /// let newList = list.flatMap(func x = [x, -x].values());
  /// assert newList.equal(List.fromArray([1, -1, 2, -2, 3, -3, 4, -4]), Int.equal);
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `k` runs in O(1) time and space.
  public func flatMap<T, R>(self : List<T>, k : T -> Types.Iter<R>) : List<R> {
    let result = empty<R>();
    // local insertion point, see nextOutBlock
    var out : [var ?R] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();
    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) for (y in k(x)) {
            if (e == outSize) {
              out := nextOutBlock(result, outSize);
              outSize := Nat64.fromIntWrap(out.size());
              e := 0
            };
            out[e.toNat()] := ?y;
            e +%= 1
          };
          case null break l
        }
      };
      i += 1
    };

    finishOut(result, e, outSize);
    result
  };

  func indexByBlockElement<T>(blockIndex : Nat, elementIndex : Nat) : Nat {
    // Both arguments are compact Nats (blockIndex <= 3 * 2^30,
    // elementIndex < 2^31), so the wrapping conversions are exact and skip
    // the overflow check of toNat64.
    let d = Nat64.fromIntWrap(blockIndex);

    // We call all data blocks of the same capacity an "epoch". We number the epochs 0,1,2,...
    // A data block is in epoch e iff the data block has capacity 2 ** e.
    // Each epoch starting with epoch 1 spans exactly two super blocks.
    // Super block s falls in epoch ceil(s/2).

    // epoch of position's data block
    // e = 64 - lz
    // d / 3 is computed as (d * 0xAAAA_AAAB) >> 33, which is exact for
    // d < 2^32 and cheaper than a division.
    let lz = Nat64.bitcountLeadingZero((d *% 0xAAAA_AAAB) >> 33);

    // capacity of all prior epochs combined
    // capacity_before_e = 2 * 4 ** (e - 1)

    // data block indices before epoch e (counting the dummy block 0)
    // blocks_before_e = 3 * 2 ** (e - 1)

    // then size = d * 2 ** e + i - c
    // where c = blocks_before_e * 2 ** e - capacity_before_e = 4 ** e
    // i.e. size = (d - 2 ** e) * 2 ** e + i, which is the line below

    // Intermediate values may exceed 2^64; that is harmless because every
    // operation used (-%, +%, <>>) is a bijection mod 2^64, so the final
    // value is exact whenever the true result fits in 64 bits. A plain >>
    // would erase the shifted-out bits and break this argument, which is
    // why the rotation <>> is used.
    ((d -% (1 <>> lz)) <>> lz +% Nat64.fromIntWrap(elementIndex)).toNat()
  };

  /// Returns the current number of elements in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// assert list.size() == 0
  /// ```
  ///
  /// Runtime: `O(1)` (with some internal calculations)
  ///
  /// Space: `O(1)`
  public func size<T>(self : List<T>) : Nat {
    // due to the design of List (blockIndex, elementIndex) pair points
    // exactly to the place where size-th element should be added
    // so, it's the inlined version of indexByBlockElement (see there for
    // the wrapping conversions and the division by 3)
    let d = Nat64.fromIntWrap(self.blockIndex);
    let lz = Nat64.bitcountLeadingZero((d *% 0xAAAA_AAAB) >> 33);
    Nat64.toNat((d -% (1 <>> lz)) <>> lz +% Nat64.fromIntWrap(self.elementIndex))
  };

  // Returns the size of data block blockIndex, i.e. 2^e where e is the
  // epoch of the block. Do not call it for blockIndex == 0.
  func dataBlockSize(blockIndex : Nat) : Nat {
    Nat32.toNat(1 <>> Nat32.bitcountLeadingZero(blockIndex.toNat32() / 3))
  };

  // Returns the length of the index block needed to accommodate data
  // block blockIndex: the smallest length in the ladder {2^j, 3 * 2^j}
  // that is strictly greater than blockIndex. Correct for
  // blockIndex < 3 * 2^30. For blockIndex >= 3 * 2^30 the true result
  // 2^32 does not fit in Nat32: the round-up 4 << 30 wraps and 0 is
  // returned (see the size limit section at the end of this file).
  func newIndexBlockLength(blockIndex : Nat32) : Nat {
    if (blockIndex <= 1) 2 else {
      let s = 30 - Nat32.bitcountLeadingZero(blockIndex);
      Nat32.toNat(((blockIndex >> s) +% 1) << s)
    }
  };

  // Returns the index block length a copy of `list` needs: the ladder
  // length for its last used data block, capped at the current length so
  // a copy never carries more spare index slots than the original.
  func copyIndexBlockLength<T>(list : List<T>) : Nat = Nat.min(
    newIndexBlockLength(Nat.toNat32(if (list.elementIndex == 0) list.blockIndex - 1 else list.blockIndex)),
    list.blocks.size()
  );

  // If the index block is exactly full then grow it to the next ladder
  // length, otherwise do nothing.
  func growIndexBlockIfNeeded<T>(list : List<T>) {
    if (list.blocks.size() == list.blockIndex) {
      let newBlocks = VarArray.repeat<[var ?T]>([var], newIndexBlockLength(list.blockIndex.toNat32()));
      var i = 0;
      while (i < list.blockIndex) {
        newBlocks[i] := list.blocks[i];
        i += 1
      };
      list.blocks := newBlocks
    }
  };

  func shrinkIndexBlockIfNeeded<T>(list : List<T>) {
    let blockIndex = list.blockIndex.toNat32();
    // No shrink is possible for blockIndex >= 2^31: the only rung there
    // is the top rung 3 * 2^30 (the completely full 2^61 List), where
    // the index block is at its exactly-full maximal length -- but
    // newIndexBlockLength would wrap to 0 there and shrink the index
    // block to nothing, so return early.
    if (blockIndex >> 31 != 0) return;
    // only when blockIndex is the first block of a super block (i.e. of
    // the form 2^j or 3 * 2^j, the index block ladder) can a shrink be due
    if ((blockIndex << Nat32.bitcountLeadingZero(blockIndex)) << 2 == 0) {
      let newLength = newIndexBlockLength(blockIndex);
      if (newLength < list.blocks.size()) {
        let newBlocks = VarArray.repeat<[var ?T]>([var], newLength);
        var i = 0;
        while (i < newLength) {
          newBlocks[i] := list.blocks[i];
          i += 1
        };
        list.blocks := newBlocks
      }
    }
  };

  /// Adds a single element to the end of a List,
  /// allocating a new internal data block if needed,
  /// and resizing the internal index block if needed.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(0); // add 0 to list
  /// list.add(1);
  /// list.add(2);
  /// list.add(3);
  /// assert list.toArray() == [0, 1, 2, 3];
  /// ```
  ///
  /// The List size is in practice limited only by available memory; the
  /// theoretical limit is 2^61 elements, beyond which `add` traps.
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  ///
  /// Amortized Space: `O(1)`, Worst Case Space: `O(sqrt(n))`
  public func add<T>(self : List<T>, element : T) {
    // Nat64 position, see nextIndexOf; elementIndex is a compact Nat
    var elementIndex = Nat64.fromIntWrap(self.elementIndex);
    if (elementIndex == 0) {
      growIndexBlockIfNeeded(self);
      let blockIndex = self.blockIndex;

      // When removing last we keep one more data block, so can be not empty
      if (self.blocks[blockIndex].size() == 0) {
        self.blocks[blockIndex] := VarArray.repeat(
          null,
          dataBlockSize(blockIndex)
        )
      }
    };

    let lastDataBlock = self.blocks[self.blockIndex];

    lastDataBlock[elementIndex.toNat()] := ?element;

    elementIndex +%= 1;
    if (elementIndex == Nat64.fromIntWrap(lastDataBlock.size())) {
      elementIndex := 0;
      self.blockIndex += 1
    };
    self.elementIndex := elementIndex.toNat()
  };

  // Add an element without checking and resizing the List
  // Local insertion point. A function that appends many elements to a
  // List it has just created (filter, filterMap, flatMap, fromIter,
  // fromPure, flatten, join) keeps the insertion point in locals instead
  // of calling add per element: the current data block `out`, the
  // position `e` in it and its size `outSize`, all Nat64 so that the
  // fast path runs without bignum-capable Nat arithmetic. `out = [var]`
  // and `e = outSize = 0` stand for the insertion point of an empty List.
  // The List's fields are only updated when a data block is full
  // (nextOutBlock) and at the end (finishOut); the List's elementIndex
  // stays 0 in between.

  // Moves the insertion point of `list` past the full data block of size
  // `filled` (no block when `filled == 0`) and returns the next data
  // block, allocating it and growing the index block as add does.
  func nextOutBlock<T>(list : List<T>, filled : Nat64) : [var ?T] {
    if (filled != 0) list.blockIndex += 1;
    growIndexBlockIfNeeded(list);
    let blocks = list.blocks;
    let blockIndex = list.blockIndex;
    if (blocks[blockIndex].size() == 0) {
      blocks[blockIndex] := VarArray.repeat(null, dataBlockSize(blockIndex))
    };
    blocks[blockIndex]
  };

  // Stores the local insertion point (position `e` in a data block of size
  // `outSize`) back into `list`.
  func finishOut<T>(list : List<T>, e : Nat64, outSize : Nat64) {
    if (outSize == 0) return; // nothing was written
    if (e == outSize) {
      list.blockIndex += 1;
      list.elementIndex := 0
    } else list.elementIndex := e.toNat()
  };

  // Appends all elements of `source` to `list` through the local
  // insertion point (out, e, outSize) and returns the updated one.
  func copyOut<T>(list : List<T>, source : List<T>, out0 : [var ?T], e0 : Nat64, outSize0 : Nat64) : ([var ?T], Nat64, Nat64) {
    var out = out0;
    var e = e0;
    var outSize = outSize0;
    let blocks = source.blocks;
    let blockCount = blocks.size();
    var i = 1;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?_) {
            if (e == outSize) {
              out := nextOutBlock(list, outSize);
              outSize := Nat64.fromIntWrap(out.size());
              e := 0
            };
            out[e.toNat()] := slot;
            e +%= 1
          };
          case null break l
        }
      };
      i += 1
    };
    (out, e, outSize)
  };

  // Sets the insertion point of `list` to position `e` of data block `b`
  // of size `outSize`, moving past the block when it is full.
  func setInsertionPoint<T>(list : List<T>, b : Nat, e : Nat64, outSize : Nat64) {
    if (e == outSize and outSize != 0) {
      list.blockIndex := b + 1;
      list.elementIndex := 0
    } else {
      list.blockIndex := b;
      list.elementIndex := e.toNat()
    }
  };

  /// Removes and returns the last item in the list or `null` if
  /// the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// assert list.removeLast() == ?11;
  /// assert list.removeLast() == ?10;
  /// assert list.removeLast() == null;
  /// ```
  ///
  /// Amortized Runtime: `O(1)`, Worst Case Runtime: `O(sqrt(n))`
  ///
  /// Amortized Space: `O(1)`, Worst Case Space: `O(sqrt(n))`
  public func removeLast<T>(self : List<T>) : ?T {
    // Nat64 position, see nextIndexOf; elementIndex is a compact Nat
    var elementIndex = Nat64.fromIntWrap(self.elementIndex);
    if (elementIndex == 0) {
      var blockIndex = self.blockIndex;
      if (blockIndex == 1) {
        return null
      };

      shrinkIndexBlockIfNeeded(self);

      blockIndex -= 1;
      elementIndex := Nat64.fromIntWrap(self.blocks[blockIndex].size());

      // Keep one totally empty block when removing
      if (blockIndex + 2 < self.blocks.size()) self.blocks[blockIndex + 2] := [var];

      self.blockIndex := blockIndex
    };
    elementIndex -%= 1;

    let lastDataBlock = self.blocks[self.blockIndex];

    let e = elementIndex.toNat();
    let element = lastDataBlock[e];
    lastDataBlock[e] := null;

    self.elementIndex := e;
    return element
  };

  // Returns the (blockIndex, elementIndex) position of the element with
  // the given index. For example, locate(2^32) = (131_072, 0).
  // Also often used with a size-valued argument, in which case the next
  // insertion position is returned (see prevIndexOf, truncate,
  // repeat/tabulate/addRepeat).
  func locate(index : Nat) : (Nat, Nat) {
    // For the derivation of the bit arithmetic see locate_readable64
    // and locate_optimal64 in test/List.test.mo; the latter is a
    // verbatim copy of this function and tests its arithmetic across
    // the full supported range.
    let i = index.toNat64();
    let lz = Nat64.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    if (lz & 1 == 0) {
      ((((i << lz2) >> 32) ^ (0x1_0000_0000 >> lz2)).toNat(), (i & (0xFFFF_FFFF >> lz2)).toNat())
    } else {
      ((((i << lz2) >> 31) ^ (0x1_8000_0000 >> lz2)).toNat(), (i & (0x7FFF_FFFF >> lz2)).toNat())
    }
  };

  /// Returns the element at index `index`. Indexing is zero-based.
  /// Traps if `index >= size`, error message may not be descriptive.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// assert list.at(0) == 10;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func at<T>(self : List<T>, index : Nat) : T {
    // inlined version of:
    //   let (a,b) = locate(index);
    //   switch(self.blocks[a][b]) {
    //     case (?element) element;
    //     case (null) Prim.trap "";
    //   };
    // for index >= size the block lookup or the null slot check
    // below traps as required.
    let i = index.toNat64();
    let lz = Nat64.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    switch (
      if (lz & 1 == 0) {
        self.blocks[(((i << lz2) >> 32) ^ (0x1_0000_0000 >> lz2)).toNat()][(i & (0xFFFF_FFFF >> lz2)).toNat()]
      } else {
        self.blocks[(((i << lz2) >> 31) ^ (0x1_8000_0000 >> lz2)).toNat()][(i & (0x7FFF_FFFF >> lz2)).toNat()]
      }
    ) {
      case (?result) return result;
      case (_) Prim.trap "List index out of bounds in at"
    }
  };

  /// Returns the element at index `index` as an option.
  /// Returns `null` when `index >= size`. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// assert list.get(0) == ?10;
  /// assert list.get(2) == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func get<T>(self : List<T>, index : Nat) : ?T {
    // No valid index is >= 2^61 (the size limit). 2^61 - 1 is still a
    // compact Nat, so this is a cheap comparison, and it makes the wrapping
    // conversion below exact.
    if (index > 0x1FFF_FFFF_FFFF_FFFF) return null;
    // inlined version of locate. The two cases are written out in full and
    // the bounds checks are done on Nat64 values: both avoid bignum-capable
    // Nat comparisons and the boxing of a (Nat64, Nat64) tuple.
    // A data block is either empty or of full capacity, and the element
    // index computed by locate always fits a full one.
    let i = Nat64.fromIntWrap(index);
    let lz = Nat64.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    let blocks = self.blocks;
    if (lz & 1 == 0) {
      let a = ((i << lz2) >> 32) ^ (0x1_0000_0000 >> lz2);
      if (a >= Nat64.fromIntWrap(blocks.size())) return null;
      let db = blocks[a.toNat()];
      let b = i & (0xFFFF_FFFF >> lz2);
      if (b < Nat64.fromIntWrap(db.size())) db[b.toNat()] else null
    } else {
      let a = ((i << lz2) >> 31) ^ (0x1_8000_0000 >> lz2);
      if (a >= Nat64.fromIntWrap(blocks.size())) return null;
      let db = blocks[a.toNat()];
      let b = i & (0x7FFF_FFFF >> lz2);
      if (b < Nat64.fromIntWrap(db.size())) db[b.toNat()] else null
    }
  };

  /// Overwrites the current element at `index` with `element`.
  /// Traps if `index` >= size, error message may not be descriptive. Indexing is zero-based.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.put(0, 20); // overwrites 10 at index 0 with 20
  /// assert list.toArray() == [20];
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func put<T>(self : List<T>, index : Nat, value : T) {
    // inlined version of locate.
    // for index >= size the block lookup or the null slot check
    // below traps as required.
    let i = index.toNat64();
    let lz = Nat64.bitcountLeadingZero(i);
    let lz2 = lz >> 1;
    let (block, element) = if (lz & 1 == 0) {
      (self.blocks[(((i << lz2) >> 32) ^ (0x1_0000_0000 >> lz2)).toNat()], (i & (0xFFFF_FFFF >> lz2)).toNat())
    } else {
      (self.blocks[(((i << lz2) >> 31) ^ (0x1_8000_0000 >> lz2)).toNat()], (i & (0x7FFF_FFFF >> lz2)).toNat())
    };

    switch (block[element]) {
      case (?_) block[element] := ?value;
      case _ Prim.trap "List index out of bounds in put"
    }
  };

  /// Sorts the elements in the list according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(3);
  /// list.add(1);
  /// list.add(2);
  /// list.sortInPlace();
  /// assert list.toArray() == [1, 2, 3];
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sortInPlace<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order)) {
    if (size(self) < 2) return;
    let array = toVarArray(self);

    array.sortInPlace(compare);

    // Nat64 index, see nextIndexOf
    var index : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();

    var i = 1;
    while (i < blockCount) {
      let db = blocks[i];
      for (j in db.keys()) {
        switch (db[j]) {
          case (?_) db[j] := ?array[index.toNat()];
          case _ return
        };
        index +%= 1
      };
      i += 1
    }
  };

  /// Sorts the elements in the list according to `compare`.
  /// Sort is deterministic, stable, and in-place.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(3);
  /// list.add(1);
  /// list.add(2);
  /// let sorted = list.sort();
  /// assert sorted.toArray() == [1, 2, 3];
  /// ```
  ///
  /// Runtime: O(size * log(size))
  ///
  /// Space: O(size)
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func sort<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order)) : List<T> {
    let array = toVarArray(self);
    array.sortInPlace(compare);
    fromVarArray(array)
  };

  /// Checks whether the `list` is sorted.
  ///
  /// Example:
  /// ```
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  /// assert list.isSorted();
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func isSorted<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order)) : Bool {
    var prev = switch (first(self)) {
      case (?x) x;
      case _ return true
    };

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // block 1 holds only the first element; the array iterator compiles to
    // a plain loop
    var i = 2;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) switch (compare(x, prev)) {
            case (#greater or #equal) prev := x;
            case (#less) return false
          };
          case null return true
        }
      };
      i += 1
    };

    true
  };

  /// Remove adjacent duplicates from the `list`, if the `list` is sorted all elements will be unique.
  ///
  /// Example:
  /// ```
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 1, 2, 2, 3]);
  /// list.deduplicate();
  /// assert list.equal(List.fromArray<Nat>([1, 2, 3]), Nat.equal);
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func deduplicate<T>(self : List<T>, equal : (implicit : (T, T) -> Bool)) {
    var prev = switch (first(self)) {
      case (?x) x;
      case _ return
    };

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // The kept elements are compacted to the front, in place, as in
    // retain. The first element stays; block 1 holds only it.
    var wb = 1;
    var out : [var ?T] = blocks[1];
    var we : Nat64 = 1;
    var outSize : Nat64 = 1;

    var i = 2;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) {
            if (not equal(x, prev)) {
              if (we == outSize) {
                wb += 1;
                out := blocks[wb];
                outSize := Nat64.fromIntWrap(out.size());
                we := 0
              };
              out[we.toNat()] := slot;
              we +%= 1
            };
            prev := x
          };
          case null break l
        }
      };
      i += 1
    };

    setInsertionPoint(self, wb, we, outSize);
    // clears the slots after the kept elements and shrinks the index block
    truncate(self, size(self))
  };

  /// Finds the first index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(1);
  /// list.add(2);
  /// list.add(3);
  /// list.add(4);
  ///
  /// assert list.indexOf(3) == ?2;
  /// assert list.indexOf(5) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func indexOf<T>(self : List<T>, equal : (implicit : (T, T) -> Bool), element : T) : ?Nat {
    if (isEmpty(self)) return null;
    nextIndexOf(self, equal, element, 0)
  };

  /// Returns the index of the next occurence of `element` in the `list` starting from the `from` index (inclusive).
  ///
  /// Traps if `fromInclusive >= size(list)`.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  /// let list = List.fromArray(['c', 'o', 'f', 'f', 'e', 'e']);
  /// assert list.nextIndexOf('c', 0) == ?0;
  /// assert list.nextIndexOf('f', 0) == ?2;
  /// assert list.nextIndexOf('f', 2) == ?2;
  /// assert list.nextIndexOf('f', 3) == ?3;
  /// assert list.nextIndexOf('f', 4) == null;
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func nextIndexOf<T>(self : List<T>, equal : (implicit : (T, T) -> Bool), element : T, fromInclusive : Nat) : ?Nat {
    if (fromInclusive >= size(self)) Prim.trap "List index out of bounds in nextIndexOf";

    let (blockIndex, elementIndex) = locate(fromInclusive);
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // The first data block is scanned from elementIndex on. Positions are
    // Nat64 so that the loop runs without bignum-capable Nat arithmetic.
    do {
      let db = blocks[blockIndex];
      let sz = Nat64.fromIntWrap(db.size());
      var j = Nat64.fromIntWrap(elementIndex);
      while (j < sz) {
        switch (db[j.toNat()]) {
          case (?x) if (equal(x, element)) return ?indexByBlockElement(blockIndex, j.toNat());
          case null return null
        };
        j +%= 1
      }
    };

    // The following data blocks are scanned whole, with the array iterator
    // that the compiler turns into a plain loop without bounds checks.
    // Empty data blocks only follow the last used one, so the scan stops
    // at the first null either way.
    var i = blockIndex + 1;
    while (i < blockCount) {
      var j : Nat64 = 0;
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) if (equal(x, element)) return ?indexByBlockElement(i, j.toNat());
          case null return null
        };
        j +%= 1
      };
      i += 1
    };

    null
  };

  /// Finds the last index of `element` in `list` using equality of elements defined
  /// by `equal`. Returns `null` if `element` is not found.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 2, 2]);
  ///
  /// assert list.lastIndexOf(2) == ?5;
  /// assert list.lastIndexOf(5) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in `O(1)` time and space.
  public func lastIndexOf<T>(self : List<T>, equal : (implicit : (T, T) -> Bool), element : T) : ?Nat = prevIndexOf(
    self,
    equal,
    element,
    size(self)
  );

  /// Returns the index of the previous occurence of `element` in the `list` starting from the `from` index (exclusive).
  ///
  /// Traps if `fromExclusive > size(list)`. Note that `fromExclusive = size(list)`
  /// is valid and starts the search from the end of the list.
  ///
  /// ```motoko include=import
  /// import Char "mo:core/Char";
  /// let list = List.fromArray(['c', 'o', 'f', 'f', 'e', 'e']);
  /// assert list.prevIndexOf('c', list.size()) == ?0;
  /// assert list.prevIndexOf('e', list.size()) == ?5;
  /// assert list.prevIndexOf('e', 5) == ?4;
  /// assert list.prevIndexOf('e', 4) == null;
  /// ```
  ///
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  public func prevIndexOf<T>(self : List<T>, equal : (implicit : (T, T) -> Bool), element : T, fromExclusive : Nat) : ?Nat {
    if (fromExclusive > size(self)) Prim.trap "List index out of bounds in prevIndexOf";

    let blocks = self.blocks;
    let (blockIndex, elementIndex) = locate(fromExclusive);

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;
    while (i > 0) {
      let db = blocks[i];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) if (equal(x, element)) return ?indexByBlockElement(i, j.toNat());
          case null Prim.trap INTERNAL_ERROR
        }
      };
      i -= 1
    };

    null
  };

  /// Returns the first value in `list` for which `predicate` returns true.
  /// If no element satisfies the predicate, returns null.
  ///
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 9, 4, 8]);
  /// let found = list.find(func(x) { x > 8 });
  /// assert found == ?9;
  /// ```
  /// Runtime: O(size)
  ///
  /// Space: O(1)
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func find<T>(self : List<T>, predicate : T -> Bool) : ?T {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // the array iterator compiles to a plain loop
    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          // return the stored option as is to avoid allocating a new one
          case (?x) if (predicate(x)) return slot;
          case null return null
        }
      };
      i += 1
    };

    null
  };

  /// Finds the index of the first element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(1);
  /// list.add(2);
  /// list.add(3);
  /// list.add(4);
  ///
  /// assert list.findIndex(func(i) { i % 2 == 0 }) == ?1;
  /// assert list.findIndex(func(i) { i > 5 }) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func findIndex<T>(self : List<T>, predicate : T -> Bool) : ?Nat {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // the array iterator compiles to a plain loop; Nat64 position, see
    // nextIndexOf
    var i = 1;
    while (i < blockCount) {
      var j : Nat64 = 0;
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) if (predicate(x)) return ?indexByBlockElement(i, j.toNat());
          case null return null
        };
        j +%= 1
      };
      i += 1
    };

    null
  };

  /// Finds the index of the last element in `list` for which `predicate` is true.
  /// Returns `null` if no such element is found.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(1);
  /// list.add(2);
  /// list.add(3);
  /// list.add(4);
  ///
  /// assert list.findLastIndex(func(i) { i % 2 == 0 }) == ?3;
  /// assert list.findLastIndex(func(i) { i > 5 }) == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in `O(1)` time and space.
  public func findLastIndex<T>(self : List<T>, predicate : T -> Bool) : ?Nat {
    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    let elementIndex = self.elementIndex;

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;

    while (i > 0) {
      let db = blocks[i];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) if (predicate(x)) return ?indexByBlockElement(i, j.toNat());
          case null Prim.trap INTERNAL_ERROR
        }
      };
      i -= 1
    };

    null
  };

  /// Performs binary search on a sorted list to find the index of the `element`.
  /// Returns `#found(index)` if the element is found, or `#insertionIndex(index)` with the index
  /// where the element would be inserted according to the ordering if not found.
  ///
  /// If there are multiple equal elements, no guarantee is made about which index is returned.
  /// The list must be sorted in ascending order according to the `compare` function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 3, 5, 7, 9, 11]);
  /// assert list.binarySearch(5) == #found(2);
  /// assert list.binarySearch(6) == #insertionIndex(3);
  /// ```
  ///
  /// Runtime: `O(log(size))`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in `O(1)` time and space.
  public func binarySearch<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order), element : T) : {
    #found : Nat;
    #insertionIndex : Nat
  } {
    // We call all data blocks of the same capacity an "epoch". We number the epochs 0,1,2,...
    // A data block is in epoch e iff the data block has capacity 2 ** e.
    // Each epoch starting with epoch 1 spans exactly two super blocks.
    // Super block s falls in epoch ceil(s/2).
    // Each epoch except e=0 contains 3 * 2 ** (e - 1) data blocks

    let blocks = self.blocks;
    let b = self.blockIndex - (if (self.elementIndex == 0) 1 else 0) : Nat;

    // block index x such that blocks[x][0] <= element
    let lessOrEqual = do {
      // epoch of the last data block
      let epoch = 32 - Nat32.bitcountLeadingZero(b.toNat32() / 3);
      // initially block index is the first in the epoch
      var lessOrEqual = Nat32.toNat((1 << epoch) / 2);

      // lessOrEqual * 3 is always the first data block in an epoch
      // while the first element of the first data block in an epoch is actually greater than element go to the previous epoch
      // as the last epoch is half of the array we each iteration of the search divides the interval in four
      while (lessOrEqual != 0 and compare(blocks[lessOrEqual * 3][0].unwrap(), element) == #greater) {
        lessOrEqual /= 2
      };

      lessOrEqual * 3
    };

    // Linear search in e=0, there are just two elements
    if (lessOrEqual == 0) {
      let to = Nat.min(size(self), 2);
      for (i in Nat.range(0, to)) {
        let x = at(self, i);
        switch (compare(x, element)) {
          case (#less) {};
          case (#equal) return #found(i);
          case (#greater) return #insertionIndex(i)
        }
      };
      return #insertionIndex(to)
    };

    // binary search the blockIndex in [left, right)
    let blockIndex = do {
      // guaranteed less or equal to element
      var left = lessOrEqual;
      // right is either outside of the array or greater than element
      var right = Nat.min(b + 1, lessOrEqual * 2);
      while (right - left : Nat > 1) {
        let mid = (left + right) / 2;
        switch (compare(blocks[mid][0].unwrap(), element)) {
          case (#less) left := mid;
          case (#greater) right := mid;
          case (#equal) return #found(indexByBlockElement(mid, 0))
        }
      };
      left
    };

    // binary search the elementIndex
    let elementIndex = do {
      let block = blocks[blockIndex];
      var left = 0;
      var right = if (blockIndex == self.blockIndex) self.elementIndex else block.size();
      while (left != right) {
        let mid = (left + right) / 2;
        switch (compare(block[mid].unwrap(), element)) {
          case (#less) left := mid + 1;
          case (#greater) right := mid;
          case (#equal) return #found(indexByBlockElement(blockIndex, mid))
        }
      };
      left
    };

    #insertionIndex(indexByBlockElement(blockIndex, elementIndex))
  };

  /// Returns true iff every element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `true`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(2);
  /// list.add(3);
  /// list.add(4);
  ///
  /// assert list.all(func x { x > 1 });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func all<T>(self : List<T>, predicate : T -> Bool) : Bool {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // the array iterator compiles to a plain loop
    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) if (not predicate(x)) return false;
          case null return true
        }
      };
      i += 1
    };

    true
  };

  /// Returns true iff some element in `list` satisfies `predicate`.
  /// In particular, if `list` is empty the function returns `false`.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(2);
  /// list.add(3);
  /// list.add(4);
  ///
  /// assert list.any(func x { x > 3 });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `predicate` runs in O(1) time and space.
  public func any<T>(self : List<T>, predicate : T -> Bool) : Bool = findIndex(self, predicate) != null;

  /// Returns an Iterator (`Iter`) over the elements of a List.
  /// Iterator provides a single method `next()`, which returns
  /// elements in order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// list.add(12);
  ///
  /// var sum = 0;
  /// for (element in list.values()) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func values<T>(self : List<T>) : Types.Iter<T> = object {
    let blocks = self.blocks.size();
    var blockIndex = 0;
    // position in data block `db`, kept as Nat32, see reader
    var elementIndex : Nat32 = 0;
    var db : [var ?T] = self.blocks[blockIndex];
    var dbSize = Nat32.fromIntWrap(db.size());

    public func next() : ?T {
      // each field is read once per call
      var position = elementIndex;
      if (position == dbSize) {
        blockIndex += 1;
        if (blockIndex >= blocks) return null;
        db := self.blocks[blockIndex];
        let sz = Nat32.fromIntWrap(db.size());
        dbSize := sz;
        if (sz == 0) {
          // stay consistent so that later calls keep returning null
          elementIndex := 0;
          return null
        };
        position := 0
      };
      let slot = db[position.toNat()];
      switch (slot) {
        case (?_) {
          elementIndex := position +% 1;
          // the stored option is returned as is
          slot
        };
        case (_) null
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the items (index-value pairs) in the list.
  /// Each item is a tuple of `(index, value)`. The iterator provides a single method
  /// `next()` which returns elements in order, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// list.add(12);
  /// assert list.enumerate().toArray() == [(0, 10), (1, 11), (2, 12)];
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(Nat, T).
  public func enumerate<T>(self : List<T>) : Types.Iter<(Nat, T)> = object {
    let blocks = self.blocks.size();
    var blockIndex = 0;
    // position in data block `db` as Nat32 and the running index as
    // Nat64, see reader
    var elementIndex : Nat32 = 0;
    var size : Nat32 = 0;
    var db : [var ?T] = [var];
    var i : Nat64 = 0;

    public func next() : ?(Nat, T) {
      var position = elementIndex;
      if (position == size) {
        blockIndex += 1;
        if (blockIndex >= blocks) return null;
        db := self.blocks[blockIndex];
        let sz = Nat32.fromIntWrap(db.size());
        size := sz;
        if (sz == 0) {
          // stay consistent so that later calls keep returning null
          elementIndex := 0;
          return null
        };
        position := 0
      };
      switch (db[position.toNat()]) {
        case (?x) {
          let index = i;
          elementIndex := position +% 1;
          i := index +% 1;
          ?(index.toNat(), x)
        };
        case (_) null
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the elements of the list in reverse order.
  /// The iterator provides a single method `next()` which returns elements from
  /// last to first, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// list.add(12);
  ///
  /// var sum = 0;
  /// for (element in list.reverseValues()) {
  ///   sum += element;
  /// };
  /// assert sum == 33;
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func reverseValues<T>(self : List<T>) : Types.Iter<T> = object {
    var blockIndex = self.blockIndex;
    // position in data block `db`, kept as Nat32, see reader
    var elementIndex = Nat32.fromIntWrap(self.elementIndex);
    var db : [var ?T] = if (blockIndex < self.blocks.size()) {
      self.blocks[blockIndex]
    } else { [var] };

    public func next() : ?T {
      var position = elementIndex;
      if (position != 0) {
        position -%= 1
      } else {
        // the first data block is exhausted: stay there and keep
        // returning null
        if (blockIndex <= 1) return null;
        blockIndex -= 1;
        db := self.blocks[blockIndex];
        position := Nat32.fromIntWrap(db.size()) -% 1
      };
      elementIndex := position;
      db[position.toNat()]
    }
  };

  /// Returns an Iterator (`Iter`) over the items in reverse order, i.e. pairs of index and value.
  /// Iterator provides a single method `next()`, which returns
  /// elements in reverse order, or `null` when out of elements to iterate over.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(10);
  /// list.add(11);
  /// list.add(12);
  /// assert list.reverseEnumerate().toArray() == [(2, 12), (1, 11), (0, 10)];
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  ///
  /// Warning: Allocates memory on the heap to store ?(T, Nat).
  public func reverseEnumerate<T>(self : List<T>) : Types.Iter<(Nat, T)> = object {
    // the running index as Nat64, the position in data block `db` as
    // Nat32, see reader
    var i = Nat64.fromIntWrap(size(self));
    var blockIndex = self.blockIndex;
    var elementIndex = Nat32.fromIntWrap(self.elementIndex);
    var db : [var ?T] = if (blockIndex < self.blocks.size()) {
      self.blocks[blockIndex]
    } else { [var] };

    public func next() : ?(Nat, T) {
      var position = elementIndex;
      if (position != 0) {
        position -%= 1
      } else {
        // the first data block is exhausted: stay there and keep
        // returning null
        if (blockIndex <= 1) return null;
        blockIndex -= 1;
        db := self.blocks[blockIndex];
        position := Nat32.fromIntWrap(db.size()) -% 1
      };
      elementIndex := position;
      switch (db[position.toNat()]) {
        case (?x) {
          let index = i -% 1;
          i := index;
          return ?(index.toNat(), x)
        };
        // a null slot here means the list shrank during iteration;
        // end the iteration like `values`, `enumerate` and `reverseValues`
        case (_) return null
      }
    }
  };

  /// Returns an Iterator (`Iter`) over the indices (keys) of the list.
  /// The iterator provides a single method `next()` which returns indices
  /// from 0 to size-1, or `null` when out of elements.
  ///
  /// ```motoko include=import
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.empty<Text>();
  /// list.add("A");
  /// list.add("B");
  /// list.add("C");
  /// list.keys().toArray() // [0, 1, 2]
  /// ```
  ///
  /// Note: This does not create a snapshot. If the returned iterator is not consumed at once,
  /// and instead the consumption of the iterator is interleaved with other operations on the
  /// List, then this may lead to unexpected results.
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func keys<T>(self : List<T>) : Types.Iter<Nat> = Nat.range(0, size(self));

  /// Creates a new List containing all elements from the provided iterator.
  /// Elements are added in the order they are returned by the iterator.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.values();
  ///
  /// let list = List.fromIter<Nat>(iter);
  /// assert list.values().toArray() == [1, 1, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func fromIter<T>(iter : Types.Iter<T>) : List<T> {
    let list = empty<T>();
    // local insertion point, see nextOutBlock
    var out : [var ?T] = [var];
    var e : Nat64 = 0;
    var outSize : Nat64 = 0;
    for (element in iter) {
      if (e == outSize) {
        out := nextOutBlock(list, outSize);
        outSize := Nat64.fromIntWrap(out.size());
        e := 0
      };
      out[e.toNat()] := ?element;
      e +%= 1
    };
    finishOut(list, e, outSize);
    list
  };

  /// Convert an iterator to a new mutable List.
  /// Elements are added in the order they are returned by the iterator.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.values();
  ///
  /// let list = iter.toList<Nat>();
  /// assert list.values().toArray() == [1, 1, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func toList<T>(self : Types.Iter<T>) : List<T> {
    // ignore-self-type-check
    fromIter(self)
  };

  /// Appends all elements from `added` to the end of `list`.
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2]);
  /// let added = List.fromArray<Nat>([3, 4]);
  /// list.append(added);
  /// assert list.toArray() == [1, 2, 3, 4];
  /// ```
  ///
  /// Runtime: `O(size(added))`
  ///
  /// Space: `O(size(added))`
  public func append<T>(self : List<T>, added : List<T>) {
    let count = size(added);
    if (count == 0) return;
    reserve(self, count);

    // Copy exactly `count` elements, so that appending a list to itself
    // copies it once. The source is read after reserve, which may have
    // replaced the index block of `self`, and so of `added` if they are
    // the same List. The write cursor (wb, we) moves through the data
    // blocks that reserve allocated.
    let source = added.blocks;
    let blocks = self.blocks;
    var wb = self.blockIndex;
    var out = blocks[wb];
    var we = Nat64.fromIntWrap(self.elementIndex);
    var outSize = Nat64.fromIntWrap(out.size());
    var remaining = Nat64.fromIntWrap(count);

    var i = 1;
    label l while (true) {
      for (slot in source[i].values()) {
        if (remaining == 0) break l;
        switch (slot) {
          case (?_) {
            if (we == outSize) {
              wb += 1;
              out := blocks[wb];
              outSize := Nat64.fromIntWrap(out.size());
              we := 0
            };
            out[we.toNat()] := slot;
            we +%= 1;
            remaining -%= 1
          };
          case null Prim.trap INTERNAL_ERROR
        }
      };
      if (remaining == 0) break l;
      i += 1
    };

    setInsertionPoint(self, wb, we, outSize)
  };

  /// Adds all elements from the provided iterator to the end of the list.
  /// Elements are added in the order they are returned by the iterator.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [1, 1, 1];
  /// let iter = array.values();
  /// let list = List.repeat<Nat>(2, 1);
  ///
  /// list.addAll(iter);
  /// assert list.values().toArray() == [2, 1, 1, 1];
  /// ```
  ///
  /// The List size is in practice limited only by available memory; the
  /// theoretical limit is 2^61 elements.
  ///
  /// Runtime: `O(n)`, where `n` is the number of elements in `iter`.
  ///
  /// Space: `O(n)`
  public func addAll<T>(self : List<T>, iter : Types.Iter<T>) {
    for (element in iter) add(self, element)
  };

  /// Creates a new immutable array containing all elements from the list.
  /// Elements appear in the same order as in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// assert list.toArray() == [1, 2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func toArray<T>(self : List<T>) : [T] {
    // the cursor of sliceToArray, see there
    Array.tabulate(size(self), sliceToArrayBase(self, 0).next)
  };

  /// Creates a List containing elements from an Array.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [2, 3];
  /// let list = List.fromArray<Nat>(array);
  /// assert list.values().toArray() == [2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func fromArray<T>(array : [T]) : List<T> {
    let (blockIndex, elementIndex) = locate(array.size());

    let blocks = newIndexBlockLength(Nat.toNat32(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let dataBlocks = VarArray.repeat<[var ?T]>([var], blocks);

    // One pass over the source with the array iterator, which the compiler
    // turns into a plain loop, opening a null-filled data block whenever
    // the current one is full. Positions are Nat64, see nextIndexOf.
    var i = 1;
    var db : [var ?T] = [var];
    var j : Nat64 = 0;
    var sz : Nat64 = 0;
    for (x in array.values()) {
      if (j == sz) {
        db := VarArray.repeat(null, dataBlockSize(i));
        dataBlocks[i] := db;
        i += 1;
        j := 0;
        sz := Nat64.fromIntWrap(db.size())
      };
      db[j.toNat()] := ?x;
      j +%= 1
    };

    {
      var blocks = dataBlocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    }
  };

  /// Creates a new mutable array containing all elements from the list.
  /// Elements appear in the same order as in the list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import VarArray "mo:core/VarArray";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// let varArray = list.toVarArray();
  /// assert varArray.toArray() == [1, 2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func toVarArray<T>(self : List<T>) : [var T] {
    let ?fs = first(self) else return [var];
    let array = VarArray.repeat(fs, size(self));

    // the array iterator compiles to a plain loop; Nat64 index, see
    // nextIndexOf
    var index : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();

    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) array[index.toNat()] := x;
          case null return array
        };
        index +%= 1
      };
      i += 1
    };

    array
  };

  /// Creates a new List containing all elements from the mutable array.
  /// Elements appear in the same order as in the array.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let array = [var 2, 3];
  /// let list = List.fromVarArray(array);
  /// assert list.values().toArray() == [2, 3];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  public func fromVarArray<T>(array : [var T]) : List<T> {
    let (blockIndex, elementIndex) = locate(array.size());

    let blocks = newIndexBlockLength(Nat.toNat32(if (elementIndex == 0) { blockIndex - 1 } else blockIndex));
    let dataBlocks = VarArray.repeat<[var ?T]>([var], blocks);

    // same as fromArray
    var i = 1;
    var db : [var ?T] = [var];
    var j : Nat64 = 0;
    var sz : Nat64 = 0;
    for (x in array.values()) {
      if (j == sz) {
        db := VarArray.repeat(null, dataBlockSize(i));
        dataBlocks[i] := db;
        i += 1;
        j := 0;
        sz := Nat64.fromIntWrap(db.size())
      };
      db[j.toNat()] := ?x;
      j +%= 1
    };

    {
      var blocks = dataBlocks;
      var blockIndex = blockIndex;
      var elementIndex = elementIndex
    }
  };

  /// Returns the first element of `list`, or `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert List.fromArray<Nat>([1, 2, 3]).first() == ?1;
  /// assert List.empty<Nat>().first() == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func first<T>(self : List<T>) : ?T {
    if (self.blockIndex == 1) null else self.blocks[1][0]
  };

  /// Returns the last element of `list`, or `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// assert List.fromArray<Nat>([1, 2, 3]).last() == ?3;
  /// assert List.empty<Nat>().last() == null;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func last<T>(self : List<T>) : ?T {
    let e = self.elementIndex;
    if (e > 0) return self.blocks[self.blockIndex][e - 1];

    let b = self.blockIndex - 1 : Nat;
    if (b == 0) null else {
      let block = self.blocks[b];
      block[block.size() - 1]
    }
  };

  /// Applies `f` to each element in `list`.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// list.forEach(func(x) {
  ///   Debug.print(x.toText()); // prints each element in list
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func forEach<T>(self : List<T>, f : T -> ()) {
    let blocks = self.blocks;
    let blockCount = blocks.size();

    // the array iterator compiles to a plain loop
    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) f(x);
          case null return
        }
      };
      i += 1
    }
  };

  /// Applies `f` to each item `(i, x)` in `list` where `i` is the key
  /// and `x` is the value.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// list.forEachEntry(func(i, x) {
  ///   // prints each item (i,x) in list
  ///   Debug.print(i.toText() # x.toText());
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func forEachEntry<T>(self : List<T>, f : (Nat, T) -> ()) {
    // the array iterator compiles to a plain loop; Nat64 index, see
    // nextIndexOf
    var index : Nat64 = 0;

    let blocks = self.blocks;
    let blockCount = blocks.size();

    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) f(index.toNat(), x);
          case null return
        };
        index +%= 1
      };
      i += 1
    }
  };

  func actualInterval(fromInclusive : Int, toExclusive : Int, size : Nat) : (Nat, Nat) {
    let startInt = if (fromInclusive < 0) {
      let s = size + fromInclusive;
      if (s < 0) { 0 } else { s }
    } else {
      if (fromInclusive > size) { size } else { fromInclusive }
    };
    let endInt = if (toExclusive < 0) {
      let e = size + toExclusive;
      if (e < 0) { 0 } else { e }
    } else {
      if (toExclusive > size) { size } else { toExclusive }
    };
    (Prim.abs(startInt), Prim.abs(if (endInt < startInt) startInt else endInt))
  };

  /// Returns an iterator over a slice of `list` starting at `fromInclusive` up to (but not including) `toExclusive`.
  ///
  /// Negative indices are relative to the end of the list. For example, `-1` corresponds to the last element in the list.
  ///
  /// If the indices are out of bounds, they are clamped to the list bounds.
  /// If the first index is greater than the second, the function returns an empty iterator.
  ///
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  /// let iter1 = list.range(3, list.size());
  /// assert iter1.next() == ?4;
  /// assert iter1.next() == ?5;
  /// assert iter1.next() == null;
  ///
  /// let iter2 = list.range(3, -1);
  /// assert iter2.next() == ?4;
  /// assert iter2.next() == null;
  ///
  /// let iter3 = list.range(0, 0);
  /// assert iter3.next() == null;
  /// ```
  ///
  /// Runtime: O(1)
  ///
  /// Space: O(1)
  public func range<T>(self : List<T>, fromInclusive : Int, toExclusive : Int) : Types.Iter<T> = object {
    let (start, end) = actualInterval(fromInclusive, toExclusive, size(self));

    // The iterator walks data blocks from the one holding `start` to the one
    // holding `end - 1`. Within the current data block `db` it reads slots
    // `j` until `stop`. Positions within a block are below 2^31 and kept
    // as Nat32, see reader.
    let (lastBlock, lastStop) : (Nat, Nat32) = if (start < end) {
      let (block, element) = locate(end - 1 : Nat);
      (block, Nat32.fromIntWrap(element) +% 1)
    } else (0, 0);

    var blockIndex = 0;
    var db : [var ?T] = [var];
    var j : Nat32 = 0;
    var stop : Nat32 = 0;
    if (start < end) {
      let (block, element) = locate(start);
      blockIndex := block;
      db := self.blocks[block];
      j := Nat32.fromIntWrap(element);
      stop := if (block == lastBlock) lastStop else Nat32.fromIntWrap(db.size())
    } else {
      // an empty range: next() returns null right away
      blockIndex := lastBlock
    };

    public func next() : ?T {
      // each field is read once per call
      var position = j;
      if (position == stop) {
        if (blockIndex == lastBlock) return null;
        blockIndex += 1;
        db := self.blocks[blockIndex];
        let sz = Nat32.fromIntWrap(db.size());
        // the List shrank during iteration
        if (sz == 0) return null;
        position := 0;
        stop := if (blockIndex == lastBlock) lastStop else sz
      };
      j := position +% 1;
      db[position.toNat()]
    }
  };

  func sliceToArrayBase<T>(self : List<T>, start : Nat) : {
    next(i : Nat) : T
  } = object {
    // a cursor like the one of reader, see there
    var blockIndex = 0;
    var elementIndex : Nat32 = 0;
    if (start != 0) {
      let (block, element) = locate(start - 1);
      blockIndex := block;
      elementIndex := Nat32.fromIntWrap(element) +% 1
    };
    var db : [var ?T] = self.blocks[blockIndex];
    var dbSize = Nat32.fromIntWrap(db.size());

    public func next(_ : Nat) : T {
      var position = elementIndex;
      if (position == dbSize) {
        blockIndex += 1;
        db := self.blocks[blockIndex];
        dbSize := Nat32.fromIntWrap(db.size());
        position := 0
      };
      switch (db[position.toNat()]) {
        case (?x) {
          elementIndex := position +% 1;
          return x
        };
        case null Prim.trap INTERNAL_ERROR
      }
    }
  };

  /// Returns a new array containing elements from `list` starting at index `fromInclusive` up to (but not including) index `toExclusive`.
  /// If the indices are out of bounds, they are clamped to the array bounds.
  ///
  /// ```motoko include=import
  /// let array = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  ///
  /// let slice1 = array.sliceToArray(1, 4);
  /// assert slice1 == [2, 3, 4];
  ///
  /// let slice2 = array.sliceToArray(1, -1);
  /// assert slice2 == [2, 3, 4];
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToArray<T>(self : List<T>, fromInclusive : Int, toExclusive : Int) : [T] {
    let (start, end) = actualInterval(fromInclusive, toExclusive, size(self));
    Array.tabulate(end - start : Nat, sliceToArrayBase(self, start).next)
  };

  /// Returns a new var array containing elements from `list` starting at index `fromInclusive` up to (but not including) index `toExclusive`.
  /// If the indices are out of bounds, they are clamped to the array bounds.
  ///
  /// ```motoko include=import
  /// import VarArray "mo:core/VarArray";
  /// import Nat "mo:core/Nat";
  ///
  /// let array = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  ///
  /// let slice1 = array.sliceToVarArray(1, 4);
  /// assert slice1.equal([var 2, 3, 4], Nat.equal);
  ///
  /// let slice2 = array.sliceToVarArray(1, -1);
  /// assert slice2.equal([var 2, 3, 4], Nat.equal);
  /// ```
  ///
  /// Runtime: O(toExclusive - fromInclusive)
  ///
  /// Space: O(toExclusive - fromInclusive)
  public func sliceToVarArray<T>(self : List<T>, fromInclusive : Int, toExclusive : Int) : [var T] {
    let (start, end) = actualInterval(fromInclusive, toExclusive, size(self));
    if (start == end) return [var];

    // Copy block by block into an array seeded with the first element,
    // instead of calling a generator closure per element. Positions are
    // Nat64, see nextIndexOf.
    let array = VarArray.repeat(at(self, start), end - start : Nat);
    let blocks = self.blocks;
    let (fromBlock, fromElement) = locate(start);
    let (toBlock, toElement) = locate(end);
    // copies slots [from, to) of block, which are all in use, to array
    // from position k on and returns the next position. k is passed
    // around rather than captured: a captured var would live in a heap box.
    func copyPart(block : [var ?T], from : Nat, to : Nat, k : Nat64) : Nat64 {
      var j = Nat64.fromIntWrap(from);
      var p = k;
      let stop = Nat64.fromIntWrap(to);
      while (j < stop) {
        switch (block[j.toNat()]) {
          case (?x) array[p.toNat()] := x;
          case null Prim.trap INTERNAL_ERROR
        };
        j +%= 1;
        p +%= 1
      };
      p
    };

    if (fromBlock == toBlock) {
      ignore copyPart(blocks[fromBlock], fromElement, toElement, 0);
      return array
    };
    var k = copyPart(blocks[fromBlock], fromElement, blocks[fromBlock].size(), 0);
    var i = fromBlock + 1;
    while (i < toBlock) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) array[k.toNat()] := x;
          case null Prim.trap INTERNAL_ERROR
        };
        k +%= 1
      };
      i += 1
    };
    // toBlock may not exist when toElement == 0
    if (toElement != 0) ignore copyPart(blocks[toBlock], 0, toElement, k);
    array
  };

  /// Like `forEachEntryRev` but iterates through the list in reverse order,
  /// from end to beginning.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// list.reverseForEachEntry(func(i, x) {
  ///   // prints each item (i,x) in list
  ///   Debug.print(i.toText() # x.toText());
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func reverseForEachEntry<T>(self : List<T>, f : (Nat, T) -> ()) {
    // Nat64 index and positions, see nextIndexOf
    var index : Nat64 = 0;

    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    let elementIndex = self.elementIndex;

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;

    while (i > 0) {
      let db = blocks[i];
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) f(index.toNat(), x);
          case null Prim.trap INTERNAL_ERROR
        };
        index +%= 1
      };
      i -= 1
    }
  };

  /// Applies `f` to each element in `list` in reverse order.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Debug "mo:core/Debug";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// list.reverseForEach(func(x) {
  ///   Debug.print(x.toText()); // prints each element in list in reverse order
  /// });
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `f` runs in O(1) time and space.
  public func reverseForEach<T>(self : List<T>, f : T -> ()) {
    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    let elementIndex = self.elementIndex;

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;

    while (i > 0) {
      let db = blocks[i];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) f(x);
          case null Prim.trap INTERNAL_ERROR
        }
      };
      i -= 1
    }
  };

  /// Executes the closure over a slice of `list` starting at `fromInclusive` up to (but not including) `toExclusive`.
  ///
  /// ```motoko include=import
  /// import Debug "mo:core/Debug";
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  /// list.forEachInRange(func x = Debug.print(x.toText()), 1, 2); // prints 2 and 3
  /// ```
  ///
  /// Runtime: `O(toExclusive - fromExclusive)`
  ///
  /// Space: `O(1)`
  public func forEachInRange<T>(self : List<T>, f : T -> (), fromInclusive : Nat, toExclusive : Nat) {
    if (not (fromInclusive <= toExclusive and toExclusive <= size(self))) Prim.trap("List range out of bounds in forEachInRange");

    // calls f on slots [from, to) of block, which are all in use.
    // Positions are Nat64, see nextIndexOf.
    func traverseBlock(block : [var ?T], f : T -> (), from : Nat, to : Nat) {
      var i = Nat64.fromIntWrap(from);
      let stop = Nat64.fromIntWrap(to);
      while (i < stop) {
        switch (block[i.toNat()]) {
          case (?value) f(value);
          case null Prim.trap(INTERNAL_ERROR)
        };
        i +%= 1
      }
    };

    let (fromBlock, fromElement) = locate(fromInclusive);
    let (toBlock, toElement) = locate(toExclusive);
    let blocks = self.blocks;
    let sz = blocks.size();

    if (fromBlock == toBlock) {
      if (fromBlock < sz) traverseBlock(blocks[fromBlock], f, fromElement, toElement);
      return
    };

    traverseBlock(blocks[fromBlock], f, fromElement, blocks[fromBlock].size());

    // the blocks in between are full: traverse them with the array
    // iterator, which the compiler turns into a plain loop
    var i = fromBlock + 1;
    let to = Nat.min(toBlock, sz);
    while (i < to) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?value) f(value);
          case null Prim.trap(INTERNAL_ERROR)
        }
      };
      i += 1
    };

    if (toBlock < sz) traverseBlock(blocks[toBlock], f, 0, toElement)
  };

  /// Returns true if the list contains the specified element according to the provided
  /// equality function. Uses the provided `equal` function to compare elements.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(2);
  /// list.add(0);
  /// list.add(3);
  ///
  /// assert list.contains(2);
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func contains<T>(self : List<T>, equal : (implicit : (T, T) -> Bool), element : T) : Bool {
    Option.isSome(indexOf(self, equal, element))
  };

  /// Returns the greatest element in the list according to the ordering defined by `compare`.
  /// Returns `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(1);
  /// list.add(2);
  ///
  /// assert list.max() == ?2;
  /// assert List.empty<Nat>().max() == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func max<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order)) : ?T {
    var maxSoFar : T = switch (first(self)) {
      case (?x) x;
      case null return null
    };

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // block 1 holds only the first element; the array iterator compiles to
    // a plain loop
    var i = 2;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) switch (compare(x, maxSoFar)) {
            case (#greater) maxSoFar := x;
            case _ {}
          };
          case null return ?maxSoFar
        }
      };
      i += 1
    };

    ?maxSoFar
  };

  /// Returns the least element in the list according to the ordering defined by `compare`.
  /// Returns `null` if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.empty<Nat>();
  /// list.add(1);
  /// list.add(2);
  ///
  /// assert list.min() == ?1;
  /// assert List.empty<Nat>().min() == null;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func min<T>(self : List<T>, compare : (implicit : (T, T) -> Types.Order)) : ?T {
    var minSoFar : T = switch (first(self)) {
      case (?x) x;
      case null return null
    };

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // block 1 holds only the first element; the array iterator compiles to
    // a plain loop
    var i = 2;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) switch (compare(x, minSoFar)) {
            case (#less) minSoFar := x;
            case _ {}
          };
          case null return ?minSoFar
        }
      };
      i += 1
    };

    ?minSoFar
  };

  /// Tests if two lists are equal by comparing their elements using the provided `equal` function.
  /// Returns true if and only if both lists have the same size and all corresponding elements
  /// are equal according to the provided function.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([1, 2]);
  /// let list2 = List.empty<Nat>();
  /// list2.add(1);
  /// list2.add(2);
  ///
  /// assert list1.equal(list2, Nat.equal);
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `equal` runs in O(1) time and space.
  public func equal<T>(self : List<T>, other : List<T>, equal : (implicit : (T, T) -> Bool)) : Bool {
    if (size(self) != size(other)) return false;

    let blocks1 = self.blocks;
    let blocks2 = other.blocks;
    let blockCount = Nat.min(blocks1.size(), blocks2.size());

    var i = 1;
    while (i < blockCount) {
      let db1 = blocks1[i];
      let db2 = blocks2[i];
      // Nat64 position, see nextIndexOf
      let sz = Nat64.fromIntWrap(Nat.min(db1.size(), db2.size()));
      if (sz == 0) return true;
      var j : Nat64 = 0;
      while (j < sz) {
        let k = j.toNat();
        // nested switches only to avoid allocating a tuple for a pair switch
        switch (db1[k]) {
          case (?x) switch (db2[k]) {
            case (?y) if (not equal(x, y)) return false;
            case null return true
          };
          case null return true
        };
        j +%= 1
      };
      i += 1
    };
    return true
  };

  /// Compares two lists lexicographically using the provided `compare` function.
  /// Elements are compared pairwise until a difference is found or one list ends.
  /// If all elements compare equal, the shorter list is considered less than the longer list.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list1 = List.fromArray<Nat>([0, 1]);
  /// let list2 = List.fromArray<Nat>([2]);
  /// let list3 = List.fromArray<Nat>([0, 1, 2]);
  ///
  /// assert list1.compare(list2, Nat.compare) == #less;
  /// assert list1.compare(list3, Nat.compare) == #less;
  /// assert list2.compare(list3, Nat.compare) == #greater;
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `compare` runs in O(1) time and space.
  public func compare<T>(self : List<T>, other : List<T>, compare : (implicit : (T, T) -> Types.Order)) : Types.Order {
    let blocks1 = self.blocks;
    let blocks2 = other.blocks;
    let blockCount = Nat.min(blocks1.size(), blocks2.size());

    var i = 1;
    label l while (i < blockCount) {
      let db1 = blocks1[i];
      let db2 = blocks2[i];
      // Nat64 position, see nextIndexOf
      let sz = Nat64.fromIntWrap(Nat.min(db1.size(), db2.size()));
      if (sz == 0) break l;
      var j : Nat64 = 0;
      while (j < sz) {
        let k = j.toNat();
        // nested switches only to avoid allocating a tuple for a pair switch
        switch (db1[k]) {
          case (?x) switch (db2[k]) {
            case (?y) switch (compare(x, y)) {
              case (#less) return #less;
              case (#greater) return #greater;
              case _ {}
            };
            case null break l
          };
          case null break l
        };
        j +%= 1
      };
      i += 1
    };
    return Nat.compare(size(self), size(other))
  };

  /// Creates a textual representation of `list`, using `toText` to recursively
  /// convert the elements into Text.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3, 4]);
  ///
  /// assert list.toText(Nat.toText) == "List[1, 2, 3, 4]";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(size)`
  ///
  /// *Runtime and space assumes that `toText` runs in O(1) time and space.
  public func toText<T>(self : List<T>, toText : (implicit : T -> Text)) : Text {
    var text = switch (first(self)) {
      case (?x) toText(x);
      case null ""
    };

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // block 1 holds only the first element; the array iterator compiles to
    // a plain loop
    var i = 2;
    label l while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) text #= ", " # toText(x);
          case null break l
        }
      };
      i += 1
    };

    "List[" # text # "]"
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// left to right.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// assert list.foldLeft("", func(acc, x) { acc # x.toText() }) == "123";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldLeft<A, T>(self : List<T>, base : A, combine : (A, T) -> A) : A {
    var accumulation = base;

    let blocks = self.blocks;
    let blockCount = blocks.size();

    // the array iterator compiles to a plain loop
    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        switch (slot) {
          case (?x) accumulation := combine(accumulation, x);
          case null return accumulation
        }
      };
      i += 1
    };

    accumulation
  };

  /// Collapses the elements in `list` into a single value by starting with `base`
  /// and progessively combining elements into `base` with `combine`. Iteration runs
  /// right to left.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// assert list.foldRight("", func(x, acc) { x.toText() # acc }) == "123";
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  ///
  /// *Runtime and space assumes that `combine` runs in O(1)` time and space.
  public func foldRight<T, A>(self : List<T>, base : A, combine : (T, A) -> A) : A {
    var accumulation = base;

    let blocks = self.blocks;
    let blockIndex = self.blockIndex;
    let elementIndex = self.elementIndex;

    var i = blockIndex;
    if (elementIndex == 0) i -= 1;

    while (i > 0) {
      let db = blocks[i];
      // Nat64 position, see nextIndexOf
      var j = Nat64.fromIntWrap(if (i == blockIndex) elementIndex else db.size());
      while (j > 0) {
        j -%= 1;
        switch (db[j.toNat()]) {
          case (?x) accumulation := combine(x, accumulation);
          case null Prim.trap INTERNAL_ERROR
        }
      };
      i -= 1
    };

    accumulation
  };

  /// Reverses the order of elements in `list` by overwriting in place.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// list.reverseInPlace();
  /// assert list.values().toArray() == [3, 2, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reverseInPlace<T>(self : List<T>) {
    let vsize = size(self);
    if (vsize <= 1) return;

    let (finalBlock, finalElement) = locate(vsize / 2);
    let blocks = self.blocks;

    // front cursor (i, j) and back cursor (blockIndexBack,
    // elementIndexBack); element positions are Nat64, see nextIndexOf
    var blockIndexBack = self.blockIndex;
    var elementIndexBack = Nat64.fromIntWrap(self.elementIndex);
    var dbBack : [var ?T] = if (blockIndexBack < blocks.size()) {
      blocks[blockIndexBack]
    } else { [var] };

    var i = 1;
    while (i <= finalBlock) {
      let db = blocks[i];
      let sz = Nat64.fromIntWrap(if (i == finalBlock) finalElement else db.size());
      var j : Nat64 = 0;
      while (j < sz) {
        if (elementIndexBack == 0) {
          blockIndexBack -= 1;
          dbBack := blocks[blockIndexBack];
          elementIndexBack := Nat64.fromIntWrap(dbBack.size()) -% 1
        } else {
          elementIndexBack -%= 1
        };
        let front = j.toNat();
        let back = elementIndexBack.toNat();
        let temp = db[front];
        db[front] := dbBack[back];
        dbBack[back] := temp;
        j +%= 1
      };
      i += 1
    }
  };

  /// Returns a new List with the elements from `list` in reverse order.
  ///
  /// Example:
  /// ```motoko include=import
  /// import Nat "mo:core/Nat";
  /// import Iter "mo:core/Iter";
  ///
  /// let list = List.fromArray<Nat>([1, 2, 3]);
  ///
  /// let rlist = list.reverse();
  /// assert rlist.values().toArray() == [3, 2, 1];
  /// ```
  ///
  /// Runtime: `O(size)`
  ///
  /// Space: `O(1)`
  public func reverse<T>(self : List<T>) : List<T> {
    let rlist = repeatInternal<T>(null, size(self));

    let blocks = self.blocks;
    let blockCount = blocks.size();
    let rblocks = rlist.blocks;

    // back cursor into rlist; Nat64 position, see nextIndexOf
    var blockIndexBack = rlist.blockIndex;
    var elementIndexBack = Nat64.fromIntWrap(rlist.elementIndex);
    var dbBack : [var ?T] = if (blockIndexBack < rblocks.size()) {
      rblocks[blockIndexBack]
    } else { [var] };

    // the array iterator compiles to a plain loop; the copy ends when the
    // back cursor reaches the front of rlist
    var i = 1;
    while (i < blockCount) {
      for (slot in blocks[i].values()) {
        if (elementIndexBack == 0) {
          blockIndexBack -= 1;
          if (blockIndexBack == 0) return rlist;
          dbBack := rblocks[blockIndexBack];
          elementIndexBack := Nat64.fromIntWrap(dbBack.size()) -% 1
        } else {
          elementIndexBack -%= 1
        };
        dbBack[elementIndexBack.toNat()] := slot
      };
      i += 1
    };

    rlist
  };

  /// Returns true if and only if the list is empty.
  ///
  /// Example:
  /// ```motoko include=import
  /// let list = List.fromArray<Nat>([2, 0, 3]);
  /// assert not list.isEmpty();
  /// assert List.empty<Nat>().isEmpty();
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func isEmpty<T>(self : List<T>) : Bool {
    self.blockIndex == 1
  };

  /// Unsafe iterator starting from `start`.
  ///
  /// Example:
  /// ```
  /// let list = List.fromArray<Nat>([1, 2, 3, 4, 5]);
  /// let reader = list.reader<Nat>(2);
  /// assert reader() == 3;
  /// assert reader() == 4;
  /// assert reader() == 5;
  /// ```
  ///
  /// Runtime: `O(1)`
  ///
  /// Space: `O(1)`
  public func reader<T>(self : List<T>, start : Nat) : () -> T {
    // The cursor is the data block `db` at blockIndex and the position
    // elementIndex in it. The cursor lives in the closure, where a Nat
    // would need bignum-capable arithmetic and a Nat64 boxing checks. The
    // position is below 2^31 (the largest data block holds 2^31 elements),
    // so it is kept as a Nat32, whose boxed form is just a shift.
    var blockIndex = 0;
    var elementIndex : Nat32 = 0;
    if (start != 0) {
      let (block, element) = locate(start - 1);
      blockIndex := block;
      elementIndex := Nat32.fromIntWrap(element) +% 1
    };
    var db : [var ?T] = self.blocks[blockIndex];
    var dbSize = Nat32.fromIntWrap(db.size());

    func next() : T {
      // Note: next() traps when reading beyond end of list
      // each field is read once per call
      var position = elementIndex;
      if (position == dbSize) {
        blockIndex += 1;
        db := self.blocks[blockIndex];
        dbSize := Nat32.fromIntWrap(db.size());
        position := 0
      };
      switch (db[position.toNat()]) {
        case (?ret) {
          elementIndex := position +% 1;
          return ret
        };
        case (_) Prim.trap("List index out of bounds in reader")
      }
    };
    next
  };

  // The theoretical size limit of 2^61 elements
  //
  // The Nat32 index block arithmetic bounds the List size to exactly
  // 2^61 elements: newIndexBlockLength is correct up to the top ladder
  // length 3 * 2^30 (see its contract), and the index block of that
  // length fills exactly when the List reaches size 2^61 (insertion
  // state (3 * 2^30, 0)). Growing any further -- add through
  // growIndexBlockIfNeeded, or constructor and addRepeat targets in
  // (2^61, 2^62) through their newIndexBlockLength call sites -- builds
  // the wrapped zero-length index block and traps with "Array index out
  // of bounds": the effect of a capacity guard, with a less descriptive
  // message. Targets >= 2^62 trap in the Nat.toNat32 conversions at the
  // call sites instead. All other operations keep working at size 2^61,
  // including removeLast, whose shrink path dodges the wrap with the
  // early return in shrinkIndexBlockIfNeeded. The bound is purely
  // theoretical: 2^61 elements need at least 16 EiB of data blocks.
  // test/List.indexBlock.test.mo exercises the machinery at this bound
  // with width-scaled (Nat8) copies of these functions.

}
