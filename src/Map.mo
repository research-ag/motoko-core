/// An imperative key-value map based on order/comparison of the keys.
/// The map data structure type is stable and can be used for orthogonal persistence.
///
/// Example:
/// ```motoko
/// import Map "mo:base/Map";
/// import Nat "mo:base/Nat";
///
/// persistent actor {
///   let numberNames = Map.empty<Nat, Text>();
///   Map.add(numberNames, Nat.compare, 0, "Zero");
///   Map.add(numberNames, Nat.compare, 1, "One");
///   Map.add(numberNames, Nat.compare, 2, "Two");
/// }
/// ```
///
/// The internal implementation is a B-tree with order 32.
///
/// Performance:
/// * Runtime: `O(log(n))` worst case cost per insertion, removal, and retrieval operation.
/// * Space: `O(n)` for storing the entire tree.
/// `n` denotes the number of key-value entries stored in the map.

// Data structure implementation is courtesy of Byron Becker.
// Source: https://github.com/canscale/StableHeapBTreeMap
// Copyright (c) 2022 Byron Becker.
// Distributed under Apache 2.0 license

// import ImmutableMap "immutable/Map";
import IterType "IterType";
import Order "Order";
import VarArray "VarArray";
import Runtime "Runtime";
import Stack "Stack";
import Option "Option";

module {
  let btreeOrder = 32; // Should be >= 4 and <= 512.

  public type Node<K, V> = {
    #leaf : Leaf<K, V>;
    #internal : Internal<K, V>
  };

  public type Data<K, V> = {
    kvs : [var ?(K, V)];
    var count : Nat
  };

  public type Internal<K, V> = {
    data : Data<K, V>;
    children : [var ?Node<K, V>]
  };

  public type Leaf<K, V> = {
    data : Data<K, V>
  };

  public type Map<K, V> = {
    var root : Node<K, V>;
    var size : Nat
  };

  // /// Convert the mutable key-value map to an immutable key-value map.
  // ///
  // /// Example:
  // /// ```motoko
  // /// import Map "mo:base/Map";
  // /// import ImmutableMap "mo:base/ImmutableMap";
  // /// import Nat "mo:base/Nat";
  // ///
  // /// persistent actor {
  // ///   let mutableMap = Map.empty<Nat, Text>();
  // ///   Map.add(mutableMap, Nat.compare, 0, "Zero");
  // ///   Map.add(mutableMap, Nat.compare, 1, "One");
  // ///   Map.add(mutableMap, Nat.compare, 2, "Two");
  // ///   let immutableMap = Map.freeze(mutableMap);
  // ///   assert(ImmutableMap.get(0) == Map.get(0));
  // /// }
  // /// ```
  // ///
  // /// Runtime: `O(n * log(n))`.
  // /// Space: `O(n)` retained memory plus garbage, see the note below.
  // /// where `n` denotes the number of key-value entries stored in the map and
  // /// assuming that the `compare` function implements an `O(1)` comparison.
  // ///
  // /// Note: Creates `O(n * log(n))` temporary objects that will be collected as garbage.
  // public func freeze<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order) : ImmutableMap.Map<K, V> {
  //   ImmutableMap.fromIter(entries(map), compare);
  // };

  // /// Convert an immutable key-value map to a mutable key-value map.
  // ///
  // /// Example:
  // /// ```motoko
  // /// import ImmutableMap "mo:base/ImmutableMap";
  // /// import Map "mo:base/Map";
  // /// import Nat "mo:base/Nat";
  // ///
  // /// persistent actor {
  // ///   var immutableMap = ImmutableMap.empty<Nat, Text>();
  // ///   immutableMap := ImmutableMap.add(immutableMap, Nat.compare, 0, "Zero");
  // ///   immutableMap := ImmutableMap.add(immutableMap, Nat.compare, 1, "One");
  // ///   immutableMap := ImmutableMap.add(immutableMap, Nat.compare, 2, "Two");
  // ///   let mutableMap = Map.thaw(immutableMap);
  // /// }
  // /// ```
  // ///
  // /// Runtime: `O(n * log(n))`.
  // /// Space: `O(n)`.
  // /// where `n` denotes the number of key-value entries stored in the map and
  // /// assuming that the `compare` function implements an `O(1)` comparison.
  // public func thaw<K, V>(map : ImmutableMap.Map<K, V>) : Map<K, V> {
  //   fromIter(ImmtableMap.entries(map), compare)
  // };

  /// Create a copy of the mutable key-value map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  ///
  /// persistent actor {
  ///   let originalMap = Map.empty<Nat, Text>();
  ///   Map.add(originalMap, Nat.compare, 0, "Zero");
  ///   Map.add(originalMap, Nat.compare, 1, "One");
  ///   Map.add(originalMap, Nat.compare, 2, "Two");
  ///   let clonedMap = Map.clone(originalMap);
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(n)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func clone<K, V>(map : Map<K, V>) : Map<K, V> {
    {
      var root = cloneNode(map.root);
      var size = map.size
    }
  };

  /// Create a new empty mutable key-value map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Debug.print(debug_show(Map.size(map))); // prints `0`
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func empty<K, V>() : Map<K, V> {
    {
      var root = #leaf({
        data = {
          kvs = VarArray.generate<?(K, V)>(btreeOrder - 1, func(index) { null });
          var count = 0
        }
      });
      var size = 0
    }
  };

  /// Create a new empty mutable key-value map with a single entry.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let cityCodes = Map.singleton<Text, Nat>("Zurich", 8000);
  ///   Debug.print(debug_show(Map.size(cityCodes))); // prints `1`
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func singleton<K, V>(key : K, value : V) : Map<K, V> {
    {
      var root = #leaf({
        data = {
          kvs = VarArray.generate<?(K, V)>(
            btreeOrder - 1,
            func(index) {
              if (index == 0) {
                ?(key, value)
              } else {
                null
              }
            }
          );
          var count = 1
        }
      });
      var size = 1
    }
  };

  /// Delete all the entries in the key-value map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///   Debug.print(debug_show(Map.size(map))); // prints `3`
  ///
  ///   Map.clear(map);
  ///   Debug.print(debug_show(Map.size(map))); // prints `0`
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func clear<K, V>(map : Map<K, V>) : () {
    let emptyMap = empty<K, V>();
    map.root := emptyMap.root;
    map.size := 0
  };

  /// Determines whether a key-value map is empty.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.isEmpty(map))); // prints `false`
  ///   Map.clear(map);
  ///   Debug.print(debug_show(Map.isEmpty(map))); // prints `true`
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func isEmpty(map : Map<Any, Any>) : Bool {
    map.size == 0
  };

  /// Return the number of entries in a key-value map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.size(map))); // prints `3`
  /// }
  /// ```
  ///
  /// Runtime: `O(1)`.
  /// Space: `O(1)`.
  public func size(map : Map<Any, Any>) : Nat {
    map.size
  };

  /// Test whether two imperative maps have equal entries.
  /// The order of the keys in both maps are defined by `compare`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map1 = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///   let map2 = Map.clone(map1, Nat.compare);
  ///
  ///   assert(Map.equal(map1, map2, Nat.compare, Text.equal);
  /// }
  /// ```
  ///
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func equal<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compare : (K, K) -> Order.Order, equal : (V, V) -> Bool) : Bool {
    let iterator1 = entries(map1);
    let iterator2 = entries(map2);
    loop {
      let next1 = iterator1.next();
      let next2 = iterator2.next();
      switch (next1, next2) {
        case (null, null) {
          return true
        };
        case (?(key1, value1), ?(key2, value2)) {
          if (compare(key1, key2) != #equal or not equal(value1, value2)) {
            return false
          }
        };
        case _ { return false }
      }
    }
  };

  /// Tests whether the map contains the provided key.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.containsKey(1))); // prints `true`
  ///   Debug.print(debug_show(Map.containsKey(3))); // prints `false`
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func containsKey<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : Bool {
    Option.isSome(get(map, compare, key))
  };

  /// Get the value associated with key in the given map if present and `null` otherwise.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.get(1))); // prints `?One`
  ///   Debug.print(debug_show(Map.get(3))); // prints `null`
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func get<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V {
    switch (map.root) {
      case (#internal(internalNode)) {
        getFromInternal(internalNode, compare, key)
      };
      case (#leaf(leafNode)) { getFromLeaf(leafNode, compare, key) }
    }
  };

  /// Insert a new key-value entry in the map.
  /// Traps if the key is already present in the map.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 1, "Eins");
  ///
  ///   let oldZero = Map.put(0, "Zero"); // inserts new key-value pair.
  ///   Debug.print(debug_show(oldZero)); // prints `null`, key was inserted.
  ///
  ///   let oldOne = Map.put(1, "One");   // overwrites value for existing key.
  ///   Debug.print(debug_show(oldOne)); // prints `eins`, previous value.
  ///   Debug.print(debug_show(Map.get(1))); // prints `One`, new value.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func add<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : () {
    switch (put(map, compare, key, value)) {
      case null {};
      case (?value) Runtime.trap("Key is already present")
    }
  };

  /// Associates the value with the key in the map.
  /// If the key is not yet present in the map, a new key-value pair is added and `null` is returned.
  /// Otherwise, if the key is already present, the value is overwritten and the previous value is returned.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 1, "Eins");
  ///
  ///   let oldZero = Map.put(0, "Zero"); // inserts new key-value pair.
  ///   Debug.print(debug_show(oldZero)); // prints `null`, key was inserted.
  ///
  ///   let oldOne = Map.put(1, "One");   // overwrites value for existing key.
  ///   Debug.print(debug_show(oldOne)); // prints `eins`, previous value.
  ///   Debug.print(debug_show(Map.get(1))); // prints `One`, new value.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func put<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : ?V {
    let insertResult = switch (map.root) {
      case (#leaf(leafNode)) {
        leafInsertHelper<K, V>(leafNode, btreeOrder, compare, key, value)
      };
      case (#internal(internalNode)) {
        internalInsertHelper<K, V>(internalNode, btreeOrder, compare, key, value)
      }
    };

    switch (insertResult) {
      case (#insert(ov)) {
        switch (ov) {
          // if inserted a value that was not previously there, increment the tree size counter
          case null { map.size += 1 };
          case _ {}
        };
        ov
      };
      case (#promote({ kv; leftChild; rightChild })) {
        map.root := #internal({
          data = {
            kvs = VarArray.generate<?(K, V)>(
              btreeOrder - 1,
              func(i) {
                if (i == 0) { ?kv } else { null }
              }
            );
            var count = 1
          };
          children = VarArray.generate<?(Node<K, V>)>(
            btreeOrder,
            func(i) {
              if (i == 0) { ?leftChild } else if (i == 1) { ?rightChild } else {
                null
              }
            }
          )
        });
        // promotion always comes from inserting a new element, so increment the tree size counter
        map.size += 1;

        null
      }
    }
  };

  /// Overwrites the value of an existing key and returns the previous value.
  /// If the key does not exist, it has no effect and returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Null");
  ///
  ///   let oldZero = Map.replaceIfExists(0, "Zero"); // overwrites the value for existing key.
  ///   Debug.print(debug_show(oldZero)); // prints `Null`, previous value.
  ///   Debug.print(debug_show(Map.get(0))); // prints `Zero`, new value.
  ///
  ///   let oldOne = Map.replaceIfExists(1, "One");  // no effect, key is absent
  ///   Debug.print(debug_show(oldOne)); // prints `null`, key was absent.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))`.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  public func replaceIfExists<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K, value : V) : ?V {
    // TODO: Could be optimized in future
    if (containsKey(map, compare, key)) {
      put(map, compare, key, value)
    } else {
      null
    }
  };

  /// Delete an existing entry by its key in the map.
  /// Traps if the key does not exist in the map.
  ///
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.delete(0)));
  ///   Debug.print(debug_show(Map.containsKey(0))); // prints `false`.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(log(n))` including garbage, see below.
  /// where `n` denotes the number of key-value entries stored in the map and
  /// assuming that the `compare` function implements an `O(1)` comparison.
  ///
  /// Note: Creates `O(log(n))` objects that will be collected as garbage.
  public func delete<K, V>(map : Map<K, V>, compare : (K, K) -> Order.Order, key : K) : () {
    let deleted = switch (map.root) {
      case (#leaf(leafNode)) {
        // TODO: think about how this can be optimized so don't have to do two steps (search and then insert)?
        switch (NodeUtil.getKeyIndex<K, V>(leafNode.data, compare, key)) {
          case (#keyFound(deleteIndex)) {
            leafNode.data.count -= 1;
            let (_, deletedValue) = ArrayUtil.deleteAndShiftValuesOver<(K, V)>(leafNode.data.kvs, deleteIndex);
            map.size -= 1;
            ?deletedValue
          };
          case _ { null }
        }
      };
      case (#internal(internalNode)) {
        let deletedValueResult = switch (internalDeleteHelper(internalNode, btreeOrder, compare, key, false)) {
          case (#delete(value)) { value };
          case (#mergeChild({ internalChild; deletedValue })) {
            if (internalChild.data.count > 0) {
              map.root := #internal(internalChild)
            }
            // This case will be hit if the BTree has order == 4
            // In this case, the internalChild has no keys (last key was merged with new child), so need to promote that merged child (its only child)
            else {
              map.root := switch (internalChild.children[0]) {
                case (?node) { node };
                case null {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.delete(), element deletion failed, due to a null replacement node error")
                }
              }
            };
            deletedValue
          }
        };
        switch (deletedValueResult) {
          // if deleted a value from the BTree, decrement the size
          case (?deletedValue) { map.size -= 1 };
          case null {}
        };
        deletedValueResult
      }
    };
    if (Option.isNull(deleted)) {
      Runtime.trap("Key is not present")
    }
  };

  /// Returns the maximum key in a BTree with its associated value. If the BTree is empty, returns null
  /// Retrieves the key-value pair from the map `m` with the maximum key.
  /// If the map is empty, returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.maxEntry(map, Nat.compare))); // prints `?(2, "Two")`.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func maxEntry<K, V>(map : Map<K, V>) : ?(K, V) {
    reverseEntries(map).next()
  };

  /// Retrieves the key-value pair from the map with the minimum key.
  /// If the map is empty, returns `null`.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.maxEntry(map, Nat.compare))); // prints `?(2, "Two")`.
  /// }
  /// ```
  ///
  /// Runtime: `O(log(n))`.
  /// Space: `O(1)`.
  /// where `n` denotes the number of key-value entries stored in the map.
  public func minEntry<K, V>(map : Map<K, V>) : ?(K, V) {
    entries(map).next()
  };

  /// Returns an iterator over the key-value pairs in the map,
  /// traversing the entries in the ascending order of the keys.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.entries(map)));
  ///   // prints `[(0, "Zero"), (1, "One"), (2, "Two")]`.
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func entries<K, V>(map : Map<K, V>) : IterType.Iter<(K, V)> {
    switch (map.root) {
      case (#leaf(leafNode)) { return leafEntries(leafNode) };
      case (#internal(internalNode)) { internalEntries(internalNode) }
    }
  };

  /// Returns an iterator over the key-value pairs in the map,
  /// traversing the entries in the descending order of the keys.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.reverseEntries(map)));
  ///   // prints `[(2, "Two"), (1, "One"), (0, "Zero")]`.
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func reverseEntries<K, V>(map : Map<K, V>) : IterType.Iter<(K, V)> {
    switch (map.root) {
      case (#leaf(leafNode)) { return reverseLeafEntries(leafNode) };
      case (#internal(internalNode)) { reverseInternalEntries(internalNode) }
    }
  };

  /// Returns an iterator over the keys in the map,
  /// traversing all keys in ascending order.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.keys(map)));
  ///   // prints `[0, 1, 2]`.
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func keys<K>(map : Map<K, Any>) : IterType.Iter<K> {
    object {
      let iterator = entries(map);

      public func next() : ?K {
        switch (iterator.next()) {
          case null null;
          case (?(key, _)) ?key
        }
      }
    }
  };

  /// Returns an iterator over the values in the map,
  /// traversing the values in the ascending order of the keys to which they are associated.
  ///
  /// Example:
  /// ```motoko
  /// import Map "mo:base/Map";
  /// import Nat "mo:base/Nat";
  /// import Debug "mo:base/Debug";
  ///
  /// persistent actor {
  ///   let map = Map.empty<Nat, Text>();
  ///   Map.add(map, Nat.compare, 0, "Zero");
  ///   Map.add(map, Nat.compare, 1, "One");
  ///   Map.add(map, Nat.compare, 2, "Two");
  ///
  ///   Debug.print(debug_show(Map.values(map)));
  ///   // prints `["Zero", "One", "Two"]`.
  /// }
  /// ```
  /// Cost of iteration over all elements:
  /// Runtime: `O(n)`.
  /// Space: `O(1)`.
  public func values<V>(map : Map<Any, V>) : IterType.Iter<V> {
    object {
      let iterator = entries(map);

      public func next() : ?V {
        switch (iterator.next()) {
          case null null;
          case (?(_, value)) ?value
        }
      }
    }
  };

  // /// The direction of iteration
  // /// \#fwd -> forward (ascending)
  // /// \#bwd -> backwards (descending)
  // public type Direction = { #fwd; #bwd };

  // /// The object returned from a scan contains:
  // /// * results - a key value array of all results found (within the bounds and limit provided)
  // /// * nextKey - an optional next key if there exist more results than the limit provided within the given bounds
  // public type ScanLimitResult<K, V> = {
  //   results : [(K, V)];
  //   nextKey : ?K
  // };

  // /// Performs a in-order scan of the Red-Black Tree between the provided key bounds, returning a number of matching entries in the direction specified (ascending/descending) limited by the limit parameter specified in an array formatted as (K, V) for each entry
  // ///
  // /// * tree - the BTree being scanned
  // /// * compare - the comparison function used to compare (in terms of order) the provided bounds against the keys in the BTree
  // /// * lowerBound - the lower bound used in the scan
  // /// * upperBound - the upper bound used in the scan
  // /// * dir - the direction of the scan
  // /// * limit - the maximum possible number of items to scan (that are between the lower and upper bounds) before returning
  // public func scanLimit<K, V>(tree : BTree<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, dir : Direction, limit : Nat) : ScanLimitResult<K, V> {
  //   if (limit == 0) { return { results = []; nextKey = null } };

  //   switch (compare(lowerBound, upperBound)) {
  //     // return empty array if lower bound is greater than upper bound
  //     case (#greater) { { results = []; nextKey = null } };
  //     // return the single entry if exists if the lower and upper bounds are equivalent
  //     case (#equal) {
  //       switch (get<K, V>(tree, compare, lowerBound)) {
  //         case null { { results = []; nextKey = null } };
  //         case (?value) { { results = [(lowerBound, value)]; nextKey = null } }
  //       }
  //     };
  //     case (#less) {
  //       // add 1 to limit to allow additional space for next key without worrying about Nat underflow
  //       let limitPlusNextKey = limit + 1;
  //       let { resultBuffer; nextKey } = iterScanLimit<K, V>(tree.root, compare, lowerBound, upperBound, dir, limitPlusNextKey);
  //       { results = Buffer.toArray(resultBuffer); nextKey = nextKey }
  //     }
  //   }
  // };

  // ///////////////////////////////////////
  // /* Internal Library Helper functions*/
  // /////////////////////////////////////

  // // gets the max key value pair in the leaf node
  // func getLeafMin<K, V>({ data } : Leaf<K, V>) : ?(K, V) {
  //   if (data.count == 0) null else { data.kvs[0] }
  // };

  // // gets the min key value pair in the internal node
  // func getInternalMin<K, V>(internal : Internal<K, V>) : ?(K, V) {
  //   var currentInternal = internal;
  //   var minKV : ?(K, V) = null;
  //   label l loop {
  //     let child = switch (currentInternal.children[0]) {
  //       case (?child) { child };
  //       case null {
  //         Runtime.trap("UNREACHABLE_ERROR: file a bug report! In BTree.internalmin(), null child error")
  //       }
  //     };

  //     switch (child) {
  //       case (#leaf(leafNode)) {
  //         minKV := getLeafMin<K, V>(leafNode);
  //         break l
  //       };
  //       case (#internal(internalNode)) { currentInternal := internalNode }
  //     }
  //   };
  //   minKV
  // };

  // // gets the max key value pair in the leaf node
  // func getLeafMax<K, V>({ data } : Leaf<K, V>) : ?(K, V) {
  //   if (data.count == 0) null else { data.kvs[data.count - 1] }
  // };

  // // gets the max key value pair in the internal node
  // func getInternalMax<K, V>(internal : Internal<K, V>) : ?(K, V) {
  //   var currentInternal = internal;
  //   var maxKV : ?(K, V) = null;
  //   label l loop {
  //     let child = switch (currentInternal.children[currentInternal.data.count]) {
  //       case (?child) { child };
  //       case null {
  //         Runtime.trap("UNREACHABLE_ERROR: file a bug report! In BTree.internalGetMax(), null child error")
  //       }
  //     };

  //     switch (child) {
  //       case (#leaf(leafNode)) {
  //         maxKV := getLeafMax<K, V>(leafNode);
  //         break l
  //       };
  //       case (#internal(internalNode)) { currentInternal := internalNode }
  //     }
  //   };
  //   maxKV
  // };

  // // Appends all kvs in the leaf to the entriesAccumulator buffer
  // func appendLeafKVs<K, V>({ data } : Leaf<K, V>, entriesAccumulator : Buffer.Buffer<(K, V)>) : () {
  //   var i = 0;
  //   while (i < data.count) {
  //     switch (data.kvs[i]) {
  //       case (?kv) { entriesAccumulator.add(kv) };
  //       case null {
  //         Debug.trap("UNREACHABLE_ERROR: file a bug report! In appendLeafEntries data.kvs[i] is null with data.count=" # Nat.toText(data.count) # " and i=" # Nat.toText(i))
  //       }
  //     };
  //     i += 1
  //   }
  // };

  // // Iterates through the entire internal node, appending all kvs to the entriesAccumulator buffer
  // func appendInternalKVs<K, V>(internal : Internal<K, V>, entriesAccumulator : Buffer.Buffer<(K, V)>) : () {
  //   // Holds an internal node stack cursor for iterating through the BTree
  //   let internalNodeStack = initializeInternalNodeStack(internal, entriesAccumulator);
  //   var internalCursor = internalNodeStack.pop();

  //   label l loop {
  //     switch (internalCursor) {
  //       case (?{ internal; kvIndex }) {
  //         switch (internal.data.kvs[kvIndex]) {
  //           case (?kv) { entriesAccumulator.add(kv) };
  //           case null {
  //             Debug.trap("UNREACHABLE_ERROR: file a bug report! In internalEntries internal.data.kvs[kvIndex] is null with internal.data.count=" # Nat.toText(internal.data.count) # " and kvIndex=" # Nat.toText(kvIndex))
  //           }
  //         };
  //         let lastKV = (internal.data.count - 1 : Nat);
  //         if (kvIndex > lastKV) {
  //           Debug.trap("UNREACHABLE_ERROR: file a bug report! In internalEntries kvIndex=" # Nat.toText(kvIndex) # " is greater than internal.data.count=" # Nat.toText(internal.data.count))
  //         };

  //         // push the new internalCursor onto the stack, and traverse the left child of the internal node
  //         // increment the kvIndex of the internalCursor,
  //         let nextCursor = { internal = internal; kvIndex = kvIndex + 1 };
  //         // if the kvIndex is less than the number of keys in the internal node, push the new internalCursor onto the stack,
  //         if (kvIndex < lastKV) {
  //           internalNodeStack.push(nextCursor)
  //         };

  //         // traverse the next child's min subtree and push the resulting internal cursors to the stack
  //         traverseInternalMinSubtree(internalNodeStack, nextCursor, entriesAccumulator);
  //         // pop the next internalCursor off the stack and continue
  //         internalCursor := internalNodeStack.pop()
  //       };
  //       // nothing left in the internalNodeStack, signalling that we have traversed the entire BTree and added all kv pairs to the entriesAccumulator
  //       case null { return }
  //     }
  //   }
  // };

  // func initializeInternalNodeStack<K, V>(internal : Internal<K, V>, entriesAccumulator : Buffer.Buffer<(K, V)>) : Stack.Stack<InternalCursor<K, V>> {
  //   let internalNodeStack = Stack.Stack<InternalCursor<K, V>>();
  //   let internalCursor : InternalCursor<K, V> = {
  //     internal;
  //     kvIndex = 0
  //   };
  //   internalNodeStack.push(internalCursor);
  //   traverseInternalMinSubtree(internalNodeStack, internalCursor, entriesAccumulator);

  //   internalNodeStack
  // };

  // // traverse the min subtree of the current internal cursor, passing each new element to the node cursor stack
  // // once a leaf node is hit, appends all the leaf entries to the entriesAccumulator buffer and returns
  // func traverseInternalMinSubtree<K, V>(internalNodeStack : Stack.Stack<InternalCursor<K, V>>, internalCursor : InternalCursor<K, V>, entriesAccumulator : Buffer.Buffer<(K, V)>) : () {
  //   var currentNode = internalCursor.internal;
  //   var childIndex = internalCursor.kvIndex;
  //   label l loop {
  //     switch (currentNode.children[childIndex]) {
  //       // If hit a leaf, have hit the bottom of the min subtree, so can just append all leaf entries to the accumulator and return (no need to push to the stack)
  //       case (?#leaf(leafChild)) {
  //         appendLeafKVs(leafChild, entriesAccumulator);
  //         return
  //       };
  //       // If hit an internal node, update the currentNode and childIndex, and push the min child index of that internal node onto the stack
  //       case (?#internal(internalNode)) {
  //         currentNode := internalNode;
  //         childIndex := 0;
  //         internalNodeStack.push({
  //           internal = internalNode;
  //           kvIndex = childIndex
  //         })
  //       };
  //       case null {
  //         Debug.trap("UNREACHABLE_ERROR: file a bug report! In dfsTraverse, currentNode.children[childIndex] is null with currentNode.data.count=" # Nat.toText(currentNode.data.count) # " and childIndex=" # Nat.toText(childIndex))
  //       }
  //     }
  //   }
  // };

  func leafEntries<K, V>({ data } : Leaf<K, V>) : IterType.Iter<(K, V)> {
    var i : Nat = 0;
    object {
      public func next() : ?(K, V) {
        if (i >= data.count) {
          return null
        } else {
          let res = data.kvs[i];
          i += 1;
          return res
        }
      }
    }
  };

  func reverseLeafEntries<K, V>({ data } : Leaf<K, V>) : IterType.Iter<(K, V)> {
    var i : Nat = data.count;
    object {
      public func next() : ?(K, V) {
        if (i == 0) {
          return null
        } else {
          let res = data.kvs[i - 1];
          i -= 1;
          return res
        }
      }
    }
  };

  // Cursor type that keeps track of the current node and the current key-value index in the node
  type NodeCursor<K, V> = { node : Node<K, V>; kvIndex : Nat };

  func internalEntries<K, V>(internal : Internal<K, V>) : IterType.Iter<(K, V)> {
    object {
      // The nodeCursorStack keeps track of the current node and the current key-value index in the node
      // We use a stack here to push to/pop off the next node cursor to visit
      let nodeCursorStack = initializeForwardNodeCursorStack(internal);

      public func next() : ?(K, V) {
        // pop the next node cursor off the stack
        var nodeCursor = Stack.pop(nodeCursorStack);
        switch (nodeCursor) {
          case null { return null };
          case (?{ node; kvIndex }) {
            switch (node) {
              // if a leaf node, iterate through the leaf node's next key-value pair
              case (#leaf(leafNode)) {
                let lastKV = leafNode.data.count - 1 : Nat;
                if (kvIndex > lastKV) {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.internalEntries(), leaf kvIndex out of bounds")
                };

                let currentKV = switch (leafNode.data.kvs[kvIndex]) {
                  case (?kv) { kv };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Map.internalEntries(), null key-value pair found in leaf node."
                      # "leafNode.data.count=" # debug_show (leafNode.data.count) # ", kvIndex=" # debug_show (kvIndex)
                    )
                  }
                };
                // if not at the last key-value pair, push the next key-value index of the leaf onto the stack and return the current key-value pair
                if (kvIndex < lastKV) {
                  Stack.push(
                    nodeCursorStack,
                    {
                      node = #leaf(leafNode);
                      kvIndex = kvIndex + 1 : Nat
                    }
                  )
                };

                // return the current key-value pair
                ?currentKV
              };
              // if an internal node
              case (#internal(internalNode)) {
                let lastKV = internalNode.data.count - 1 : Nat;
                // Developer facing message in case of a bug
                if (kvIndex > lastKV) {
                  Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.internalEntries(), internal kvIndex out of bounds")
                };

                let currentKV = switch (internalNode.data.kvs[kvIndex]) {
                  case (?kv) { kv };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Map.internalEntries(), null key-value pair found in internal node. " #
                      "internal.data.count=" # debug_show (internalNode.data.count) # ", kvIndex=" # debug_show (kvIndex)
                    )
                  }
                };

                let nextCursor = {
                  node = #internal(internalNode);
                  kvIndex = kvIndex + 1 : Nat
                };
                // if not the last key-value pair, push the next key-value index of the internal node onto the stack
                if (kvIndex < lastKV) {
                  Stack.push(nodeCursorStack, nextCursor)
                };
                // traverse the next child's min subtree and push the resulting node cursors onto the stack
                // then return the current key-value pair of the internal node
                traverseMinSubtreeIter(nodeCursorStack, nextCursor);
                ?currentKV
              }
            }
          }
        }
      }
    }
  };

  func reverseInternalEntries<K, V>(internal : Internal<K, V>) : IterType.Iter<(K, V)> {
    object {
      // The nodeCursorStack keeps track of the current node and the current key-value index in the node
      // We use a stack here to push to/pop off the next node cursor to visit
      let nodeCursorStack = initializeReverseNodeCursorStack(internal);

      public func next() : ?(K, V) {
        // pop the next node cursor off the stack
        var nodeCursor = Stack.pop(nodeCursorStack);
        switch (nodeCursor) {
          case null { return null };
          case (?{ node; kvIndex }) {
            switch (node) {
              // if a leaf node, reverse iterate through the leaf node's next key-value pair
              case (#leaf(leafNode)) {
                let firstKV = 0 : Nat;

                let currentKV = switch (leafNode.data.kvs[kvIndex]) {
                  case (?kv) { kv };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Map.reverseInternalEntries(), null key-value pair found in leaf node."
                      # "leafNode.data.count=" # debug_show (leafNode.data.count) # ", kvIndex=" # debug_show (kvIndex)
                    )
                  }
                };
                // if not at the last key-value pair, push the previous key-value index of the leaf onto the stack and return the current key-value pair
                if (kvIndex > firstKV) {
                  Stack.push(
                    nodeCursorStack,
                    {
                      node = #leaf(leafNode);
                      kvIndex = kvIndex - 1 : Nat
                    }
                  )
                };

                // return the current key-value pair
                ?currentKV
              };
              // if an internal node
              case (#internal(internalNode)) {
                let firstKV = 0 : Nat;

                let currentKV = switch (internalNode.data.kvs[kvIndex]) {
                  case (?kv) { kv };
                  case null {
                    Runtime.trap(
                      "UNREACHABLE_ERROR: file a bug report! In Map.reverseInternalEntries(), null key-value pair found in internal node. " #
                      "internal.data.count=" # debug_show (internalNode.data.count) # ", kvIndex=" # debug_show (kvIndex)
                    )
                  }
                };

                let previousCursor = {
                  node = #internal(internalNode);
                  kvIndex = kvIndex - 1 : Nat
                };
                // if not the first key-value pair, push the previous key-value index of the internal node onto the stack
                if (kvIndex > firstKV) {
                  Stack.push(nodeCursorStack, previousCursor)
                };
                // traverse the previous child's max subtree and push the resulting node cursors onto the stack
                // then return the current key-value pair of the internal node
                traverseMaxSubtreeIter(nodeCursorStack, previousCursor);
                ?currentKV
              }
            }
          }
        }
      }
    }
  };

  func initializeForwardNodeCursorStack<K, V>(internal : Internal<K, V>) : Stack.Stack<NodeCursor<K, V>> {
    let nodeCursorStack = Stack.empty<NodeCursor<K, V>>();
    let nodeCursor : NodeCursor<K, V> = {
      node = #internal(internal);
      kvIndex = 0
    };

    // push the initial cursor to the stack
    Stack.push(nodeCursorStack, nodeCursor);
    // then traverse left
    traverseMinSubtreeIter(nodeCursorStack, nodeCursor);
    nodeCursorStack
  };

  func initializeReverseNodeCursorStack<K, V>(internal : Internal<K, V>) : Stack.Stack<NodeCursor<K, V>> {
    let nodeCursorStack = Stack.empty<NodeCursor<K, V>>();
    let nodeCursor : NodeCursor<K, V> = {
      node = #internal(internal);
      kvIndex = internal.data.count - 1
    };

    // push the initial cursor to the stack
    Stack.push(nodeCursorStack, nodeCursor);
    // then traverse left
    traverseMaxSubtreeIter(nodeCursorStack, nodeCursor);
    nodeCursorStack
  };

  // traverse the min subtree of the current node cursor, passing each new element to the node cursor stack
  func traverseMinSubtreeIter<K, V>(nodeCursorStack : Stack.Stack<NodeCursor<K, V>>, nodeCursor : NodeCursor<K, V>) : () {
    var currentNode = nodeCursor.node;
    var childIndex = nodeCursor.kvIndex;

    label l loop {
      switch (currentNode) {
        // If currentNode is leaf, have hit the minimum element of the subtree and already pushed it's cursor to the stack
        // so can return
        case (#leaf(_)) {
          return
        };
        // If currentNode is internal, add it's left most child to the stack and continue traversing
        case (#internal(internalNode)) {
          switch (internalNode.children[childIndex]) {
            // Push the next min (left most) child node to the stack
            case (?childNode) {
              childIndex := 0;
              currentNode := childNode;
              Stack.push(
                nodeCursorStack,
                {
                  node = currentNode;
                  kvIndex = childIndex
                }
              )
            };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.traverseMinSubtreeIter(), null child node error")
            }
          }
        }
      }
    }
  };

  // traverse the max subtree of the current node cursor, passing each new element to the node cursor stack
  func traverseMaxSubtreeIter<K, V>(nodeCursorStack : Stack.Stack<NodeCursor<K, V>>, nodeCursor : NodeCursor<K, V>) : () {
    var currentNode = nodeCursor.node;
    var childIndex = nodeCursor.kvIndex;

    label l loop {
      switch (currentNode) {
        // If currentNode is leaf, have hit the maximum element of the subtree and already pushed it's cursor to the stack
        // so can return
        case (#leaf(_)) {
          return
        };
        // If currentNode is internal, add it's right most child to the stack and continue traversing
        case (#internal(internalNode)) {
          switch (internalNode.children[childIndex]) {
            // Push the next max (right most) child node to the stack
            case (?childNode) {
              let childDataCount = switch (childNode) {
                case (#internal(internalNode)) internalNode.data.count;
                case (#leaf(leafNode)) leafNode.data.count
              };
              childIndex := childDataCount - 1;
              currentNode := childNode;
              Stack.push(
                nodeCursorStack,
                {
                  node = currentNode;
                  kvIndex = childIndex
                }
              )
            };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.traverseMaxSubtreeIter(), null child node error")
            }
          }
        }
      }
    }
  };

  // // Intermediate result used during the scanning of different BTree node types
  // type IntermediateScanResult<K, V> = {
  //   // the buffer scan result from a specific node
  //   resultBuffer : Buffer.Buffer<(K, V)>;
  //   // the remaining limit
  //   limit : Nat;
  //   // the next key (applicable if limit was hit, but more entries exist)
  //   nextKey : ?K
  // };

  // func iterScanLimit<K, V>(node : Node<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, dir : Direction, limit : Nat) : IntermediateScanResult<K, V> {
  //   switch (dir, node) {
  //     case (#fwd, #leaf(leafNode)) {
  //       iterScanLimitLeafForward<K, V>(leafNode, compare, lowerBound, upperBound, limit)
  //     };
  //     case (#bwd, #leaf(leafNode)) {
  //       iterScanLimitLeafReverse<K, V>(leafNode, compare, lowerBound, upperBound, limit)
  //     };
  //     case (#fwd, #internal(internalNode)) {
  //       iterScanLimitInternalForward<K, V>(internalNode, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat)
  //     };
  //     case (#bwd, #internal(internalNode)) {
  //       iterScanLimitInternalReverse<K, V>(internalNode, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat)
  //     }
  //   }
  // };

  // func iterScanLimitLeafForward<K, V>({ data } : Leaf<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat) : IntermediateScanResult<K, V> {
  //   let resultBuffer : Buffer.Buffer<(K, V)> = Buffer.Buffer(0);
  //   var remainingLimit = limit;
  //   var elementIndex = switch (BS.binarySearchNode(data.kvs, compare, lowerBound, data.count)) {
  //     case (#keyFound(idx)) { idx };
  //     case (#notFound(idx)) {
  //       // skip this leaf if lower bound is greater than all elements in the leaf
  //       if (idx >= data.count) {
  //         return {
  //           resultBuffer;
  //           limit = remainingLimit;
  //           nextKey = null
  //         }
  //       };
  //       idx
  //     }
  //   };

  //   label l while (remainingLimit > 1) {
  //     switch (data.kvs[elementIndex]) {
  //       case (?(k, v)) {
  //         switch (compare(k, upperBound)) {
  //           // iterating forward and key is greater than the upper bound
  //           // Set the limit to 0 and return the buffer to signal stopping the scan in the calling context. There is no next key
  //           case (#greater) {
  //             return {
  //               resultBuffer;
  //               limit = 0;
  //               nextKey = null
  //             }
  //           };
  //           // Key is equal to the upper bound. Add the element to the buffer, then set the limit to 0 and return the buffer to signal stopping the scan in the calling context. There is no next key
  //           case (#equal) {
  //             resultBuffer.add((k, v));
  //             return {
  //               resultBuffer;
  //               limit = 0;
  //               nextKey = null
  //             }
  //           };
  //           // Iterating forward and key is less than the upper bound. Add the element to the buffer, decrement from the limit, and increase the element index
  //           case (#less) {
  //             resultBuffer.add((k, v));
  //             remainingLimit -= 1;
  //             elementIndex += 1;
  //             if (elementIndex >= data.count) { break l }
  //           }
  //         }
  //       };
  //       case null {
  //         Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitLeafForward, attemmpted to add a null element to the result buffer")
  //       }
  //     }
  //   };

  //   // if added all elements in the leaf, return the buffer and remaining limit
  //   if (elementIndex == data.count) {
  //     return {
  //       resultBuffer;
  //       limit = remainingLimit;
  //       nextKey = null
  //     }
  //   };

  //   // otherwise, the remaining limit must equal 1 and we haven't gone through all of the elements in the leaf, set the next key and limit to 0, and return the buffer to signal stopping the scan in the calling context
  //   if (remainingLimit == 1) {
  //     return {
  //       resultBuffer;
  //       limit = 0;
  //       nextKey = switch (data.kvs[elementIndex]) {
  //         case null { null };
  //         case (?kv) {
  //           switch (compare(kv.0, upperBound)) {
  //             // if the next key is greater than the upper bound, have hit the upper bound, so return null
  //             case (#greater) { null };
  //             // otherwise less or equal to the upper bound, so return the next key
  //             case _ { ?kv.0 }
  //           }
  //         }
  //       }
  //     }
  //   };

  //   Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitLeafForward, reached a catch-all case that should not happen with a remaining limit =" # Nat.toText(remainingLimit))
  // };

  // func iterScanLimitLeafReverse<K, V>({ data } : Leaf<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat) : IntermediateScanResult<K, V> {
  //   let resultBuffer : Buffer.Buffer<(K, V)> = Buffer.Buffer(0);
  //   var remainingLimit = limit;
  //   var elementIndex = switch (BS.binarySearchNode(data.kvs, compare, upperBound, data.count)) {
  //     case (#keyFound(idx)) { idx };
  //     case (#notFound(idx)) {
  //       // skip this leaf if upper bound is less than all elements in the leaf
  //       if (idx == 0) {
  //         return {
  //           resultBuffer;
  //           limit = remainingLimit;
  //           nextKey = null
  //         }
  //       };

  //       // We are iterating in reverse and did not find the upper bound, choose the previous element
  //       // idx is not 0, so we can safely subtract 1
  //       idx - 1 : Nat
  //     }
  //   };

  //   label l while (remainingLimit > 1) {
  //     switch (data.kvs[elementIndex]) {
  //       case (?(k, v)) {
  //         switch (compare(k, lowerBound)) {
  //           // Iterating in reverse and key is less than the lower bound.
  //           // Set the limit to 0 and return the buffer to signal stopping the scan in the calling context. There is no next key
  //           case (#less) {
  //             return {
  //               resultBuffer;
  //               limit = 0;
  //               nextKey = null
  //             }
  //           };
  //           // Key is equal to the lower bound. Add the element to the buffer, then set the limit to 0 and return the buffer to signal stopping the scan in the calling context. There is no next key
  //           case (#equal) {
  //             resultBuffer.add((k, v));
  //             return {
  //               resultBuffer;
  //               limit = 0;
  //               nextKey = null
  //             }
  //           };
  //           // Iterating in reverse and key is greater than the lower bound. Add the element to the buffer, decrement from the limit, and decrement the element index
  //           case (#greater) {
  //             resultBuffer.add((k, v));
  //             remainingLimit -= 1;
  //             // if this was the last element of the leaf, return the buffer and remaining limit)
  //             if (elementIndex == 0) {
  //               return {
  //                 resultBuffer;
  //                 limit = remainingLimit;
  //                 nextKey = null
  //               }
  //             };
  //             elementIndex -= 1
  //           }
  //         }
  //       };
  //       case null {
  //         Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitLeafReverse, attemmpted to add a null element to the result buffer")
  //       }
  //     }
  //   };

  //   if (remainingLimit > 1) {
  //     return {
  //       resultBuffer;
  //       limit = remainingLimit;
  //       nextKey = null
  //     }
  //   };

  //   // otherwise, the remaining limit must equal 1 and we haven't gone through all of the elements in the leaf, set the next key and limit to 0, and return the buffer to signal stopping the scan in the calling context
  //   if (remainingLimit == 1) {
  //     return {
  //       resultBuffer;
  //       limit = 0;
  //       nextKey = switch (data.kvs[elementIndex]) {
  //         case null { null };
  //         case (?kv) { ?kv.0 }
  //       }
  //     }
  //   };

  //   Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitLeafReverse, reached a catch-all case that should not happen with a remaining limit =" # Nat.toText(remainingLimit))
  // };

  // // Cursor of the next key value pair in the internal node to explore from
  // type ScanCursor<K, V> = {
  //   #leafCursor : Leaf<K, V>;
  //   #internalCursor : InternalCursor<K, V>
  // };
  // // An Internal Cursor used to keep track of the kv index being iterated upon within an internal node
  // type InternalCursor<K, V> = {
  //   internal : Internal<K, V>;
  //   kvIndex : Nat
  // };

  // func iterScanLimitInternalForward<K, V>(internal : Internal<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat) : IntermediateScanResult<K, V> {
  //   // keep in mind that the limit has a + 1; (this additional 1 is for the nextKey)
  //   var remainingLimit = limit;
  //   // result buffer being appended to
  //   let resultBuffer : Buffer.Buffer<(K, V)> = Buffer.Buffer(0);
  //   // the next key to be returned
  //   var nextKey : ?K = null;
  //   // seed the initial node stack used to iterate throught the BTree
  //   let nodeStack = seedInitialCursorStack<K, V>(internal, compare, lowerBound, upperBound, #fwd, remainingLimit);

  //   label l while (remainingLimit > 0) {
  //     switch (nodeStack.pop()) {
  //       case (?#leafCursor(leaf)) {
  //         let intermediateScanResult = iterScanLimitLeafForward(leaf, compare, lowerBound, upperBound, remainingLimit);
  //         resultBuffer.append(intermediateScanResult.resultBuffer);
  //         remainingLimit := intermediateScanResult.limit;
  //         nextKey := intermediateScanResult.nextKey
  //       };
  //       case (?#internalCursor(internalCursor)) {
  //         let poppedInternalKV = switch (internalCursor.internal.data.kvs[internalCursor.kvIndex]) {
  //           case (?kv) { kv };
  //           case null {
  //             Debug.trap(
  //               "UNREACHABLE_ERROR: file a bug report! In iterScanLimitInternalForward, a popped #internalCursor has an invalid kvIndex. the internal returned has internal node count=" # Nat.toText(internalCursor.internal.data.count) #
  //               " and index=" # Nat.toText(internalCursor.kvIndex)
  //             )
  //           }
  //         };
  //         switch (
  //           compare(
  //             poppedInternalKV.0,
  //             upperBound
  //           )
  //         ) {
  //           case (#less) {
  //             if (remainingLimit == 1) {
  //               return {
  //                 resultBuffer;
  //                 limit = 0;
  //                 nextKey = ?poppedInternalKV.0
  //               }
  //             };

  //             resultBuffer.add(poppedInternalKV);
  //             remainingLimit -= 1;

  //             // move the cursor to the kv to the right (greater)
  //             let childCursor = {
  //               internal = internalCursor.internal;
  //               kvIndex = internalCursor.kvIndex + 1
  //             };

  //             // if not at the end of the internal node's kvs, push the next internal cursor kv to the stack
  //             if (internalCursor.kvIndex < (internalCursor.internal.data.count - 1 : Nat)) {
  //               nodeStack.push(#internalCursor(childCursor))
  //             };

  //             // Then traverse from the new cursor's child predecessor
  //             traverse<K, V>(nodeStack, childCursor, compare, lowerBound, upperBound, #fwd, remainingLimit)
  //           };
  //           // add this kv and then are done
  //           case (#equal) {
  //             if (remainingLimit == 1) {
  //               return {
  //                 resultBuffer;
  //                 limit = 0;
  //                 nextKey = ?poppedInternalKV.0
  //               }
  //             };

  //             resultBuffer.add(poppedInternalKV);
  //             remainingLimit -= 1;
  //             return {
  //               resultBuffer;
  //               limit = remainingLimit;
  //               nextKey = null
  //             }
  //           };
  //           // have reached the end, we are done
  //           case (#greater) {
  //             return {
  //               resultBuffer;
  //               limit = remainingLimit;
  //               nextKey = null
  //             }
  //           }
  //         }
  //       };
  //       // if no more nodes left in the stack to pop, return the result
  //       case null {
  //         return {
  //           resultBuffer;
  //           limit = remainingLimit;
  //           nextKey = null
  //         }
  //       }
  //     }
  //   };

  //   return {
  //     resultBuffer;
  //     limit = remainingLimit;
  //     nextKey
  //   };

  //   Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitInternalForward, breached the scan loop without first returning a result.")
  // };

  // func iterScanLimitInternalReverse<K, V>(internal : Internal<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, limit : Nat) : IntermediateScanResult<K, V> {
  //   // keep in mind that the limit has a + 1; (this additional 1 is for the nextKey)
  //   var remainingLimit = limit;
  //   // result buffer being appended to
  //   let resultBuffer : Buffer.Buffer<(K, V)> = Buffer.Buffer(0);
  //   // the next key to be returned
  //   var nextKey : ?K = null;
  //   // seed the initial node stack used to iterate throught the BTree
  //   let nodeStack = seedInitialCursorStack<K, V>(internal, compare, lowerBound, upperBound, #bwd, remainingLimit);

  //   label l while (remainingLimit > 0) {
  //     // pop the next node "cursor" from the stack
  //     switch (nodeStack.pop()) {
  //       case (?#leafCursor(leaf)) {
  //         let intermediateScanResult = iterScanLimitLeafReverse(leaf, compare, lowerBound, upperBound, remainingLimit);
  //         resultBuffer.append(intermediateScanResult.resultBuffer);
  //         remainingLimit := intermediateScanResult.limit;
  //         nextKey := intermediateScanResult.nextKey
  //       };
  //       case (?#internalCursor(internalCursor)) {
  //         let poppedInternalKV = switch (internalCursor.internal.data.kvs[internalCursor.kvIndex]) {
  //           case (?kv) { kv };
  //           case null {
  //             Debug.trap(
  //               "UNREACHABLE_ERROR: file a bug report! In iterScanLimitInternalReverse, a popped #internalCursor has an invalid kvIndex. the internal returned has internal node count=" # Nat.toText(internalCursor.internal.data.count) #
  //               " and index=" # Nat.toText(internalCursor.kvIndex)
  //             )
  //           }
  //         };

  //         switch (
  //           compare(
  //             poppedInternalKV.0,
  //             lowerBound
  //           )
  //         ) {
  //           case (#greater) {
  //             // if one spot left, the popped kv contains the next key
  //             if (remainingLimit == 1) {
  //               return {
  //                 resultBuffer;
  //                 limit = 0;
  //                 nextKey = ?poppedInternalKV.0
  //               }
  //             };

  //             resultBuffer.add(poppedInternalKV);
  //             remainingLimit -= 1;
  //             let childCursor = {
  //               internal = internalCursor.internal;
  //               kvIndex = internalCursor.kvIndex
  //             };
  //             // if not at the last kv of this internal node, move the cursor to the left (less) and push it to the stack
  //             if (internalCursor.kvIndex > 0) {
  //               nodeStack.push(#internalCursor({ childCursor with kvIndex = internalCursor.kvIndex - 1 : Nat }))
  //             };

  //             // traverse the childCursor
  //             traverse<K, V>(nodeStack, childCursor, compare, lowerBound, upperBound, #bwd, remainingLimit)
  //           };
  //           // add this kv and then are done
  //           case (#equal) {
  //             if (remainingLimit == 1) {
  //               return {
  //                 resultBuffer;
  //                 limit = 0;
  //                 nextKey = ?poppedInternalKV.0
  //               }
  //             };

  //             resultBuffer.add(poppedInternalKV);
  //             remainingLimit -= 1;
  //             return {
  //               resultBuffer;
  //               limit = remainingLimit;
  //               nextKey = null
  //             }
  //           };
  //           // have reached the end, we are done
  //           case (#less) {
  //             return {
  //               resultBuffer;
  //               limit = remainingLimit;
  //               nextKey = null
  //             }
  //           }
  //         }
  //       };
  //       // if no more nodes left in the stack to pop, return the result
  //       case null {
  //         return {
  //           resultBuffer;
  //           limit = remainingLimit;
  //           nextKey = null
  //         }
  //       }
  //     }
  //   };

  //   return {
  //     resultBuffer;
  //     limit = remainingLimit;
  //     nextKey
  //   };

  //   Debug.trap("UNREACHABLE_ERROR: file a bug report! In iterScanLimitInternalReverse, breached the scan loop without first returning a result.")
  // };

  // func seedInitialCursorStack<K, V>(internal : Internal<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, dir : Direction, limit : Nat) : Stack.Stack<ScanCursor<K, V>> {
  //   // stack of internal nodes coupled with the next node index
  //   var nodeStack = Stack.Stack<ScanCursor<K, V>>();
  //   // the bound to compare against when traversing
  //   let traversalBound = switch (dir) {
  //     case (#fwd) { lowerBound };
  //     case (#bwd) { upperBound }
  //   };
  //   // child index closest to bound
  //   let childIndex = switch (BS.binarySearchNode(internal.data.kvs, compare, traversalBound, internal.data.count)) {
  //     // if found the lower bound key, then add the node and return the node stack (to be the next node popped)
  //     case (#keyFound(kvIndex)) {
  //       nodeStack.push(#internalCursor({ internal; kvIndex }));
  //       return nodeStack
  //     };
  //     // otherwise, the index returned is that of the child
  //     case (#notFound(childIdx)) { childIdx }
  //   };

  //   var childCursor = {
  //     internal;
  //     kvIndex = childIndex
  //   };

  //   switch (dir) {
  //     case (#fwd) {
  //       // if child index is not the last child, push the internal and next kv index onto the stack (to be popped later)
  //       // Note: the child index is equal to the internal node's next kv index in this case, so we can use that here
  //       if (childIndex < internal.data.count) {
  //         nodeStack.push(#internalCursor(childCursor))
  //       }
  //     };
  //     case (#bwd) {
  //       // if child index is not the first child, push the internal and previous kv index onto the stack (to be popped later)
  //       // Note: the child index - 1 is equal to the internal node's previous kv index in this case, so we can use that here
  //       if (childIndex > 0) {
  //         nodeStack.push(#internalCursor({ childCursor with kvIndex = (childIndex - 1 : Nat) }))
  //       }
  //     }
  //   };

  //   // continue traversing the BTree and adding cursors to the stack until the node with the element closest to the lower bound is pushed
  //   traverse(nodeStack, childCursor, compare, lowerBound, upperBound, dir, limit);
  //   nodeStack
  // };

  // func traverse<K, V>(nodeStack : Stack.Stack<ScanCursor<K, V>>, internalCursor : InternalCursor<K, V>, compare : (K, K) -> O.Order, lowerBound : K, upperBound : K, dir : Direction, limit : Nat) : () {
  //   // the current internal node being explored
  //   var currentNode = internalCursor.internal;
  //   // the kv index of the current internal node being explored
  //   var childIndex = internalCursor.kvIndex;
  //   // the bound to compare against when traversing
  //   let traversalBound = switch (dir) {
  //     case (#fwd) { lowerBound };
  //     case (#bwd) { upperBound }
  //   };

  //   label l loop {
  //     switch (currentNode.children[childIndex]) {
  //       // if the child is an internal node, update the current node for the next traversal loop
  //       case (?#internal(internalNode)) { currentNode := internalNode };
  //       // if the child is a leaf node, push it to the stack and return the stack (reached the end of this path)
  //       case (?#leaf(leafNode)) {
  //         nodeStack.push(#leafCursor(leafNode));
  //         return
  //       };
  //       case null {
  //         Debug.trap(
  //           "UNREACHABLE_ERROR: file a bug report! In BTree.traverse(), encountered a null and invalid childIndex=" # Nat.toText(childIndex) #
  //           "for an internal node with count=" # Nat.toText(currentNode.data.count)
  //         )
  //       }
  //     };

  //     // update the child index (for the next node to traverse)
  //     childIndex := switch (BS.binarySearchNode(currentNode.data.kvs, compare, traversalBound, currentNode.data.count)) {
  //       // if found the bound key, then add the node and return the node stack (to be the next node popped)
  //       case (#keyFound(kvIndex)) {
  //         nodeStack.push(#internalCursor({ internal = currentNode; kvIndex }));
  //         return
  //       };
  //       // otherwise, the index returned is that of the child
  //       case (#notFound(childIdx)) { childIdx }
  //     };

  //     // depending on the order of traversal, push the appropriate internal cursor to the stack
  //     // (as long as the cursor is not at the end of that current internal node)
  //     switch (dir) {
  //       case (#fwd) {
  //         // if child index is not the last child, push the internal and next kv index onto the stack (to be popped later)
  //         // Note: the child index is equal to the internal node's next kv index in this case, so we can use that here
  //         if (childIndex < currentNode.data.count) {
  //           nodeStack.push(#internalCursor({ internal = currentNode; kvIndex = childIndex }))
  //         }
  //       };
  //       case (#bwd) {
  //         // if child index is not the last child, push the internal and next kv index onto the stack (to be popped later)
  //         // Note: the child index is equal to the internal node's next kv index in this case, so we can use that here
  //         if (childIndex > 0) {
  //           nodeStack.push(#internalCursor({ internal = currentNode; kvIndex = childIndex - 1 }))
  //         }
  //       }
  //     }
  //   }
  // };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateInternalDeleteResult<K, V> = {
    // element was deleted or not found, returning the old value (?value or null)
    #delete : ?V;
    // deleted an element, but was unable to successfully borrow and rebalance at the previous level without merging children
    // the internalChild is the merged child that needs to be rebalanced at the next level up in the BTree
    #mergeChild : {
      internalChild : Internal<K, V>;
      deletedValue : ?V
    }
  };

  func internalDeleteHelper<K, V>(internalNode : Internal<K, V>, order : Nat, compare : (K, K) -> Order.Order, deleteKey : K, skipNode : Bool) : IntermediateInternalDeleteResult<K, V> {
    let minKeys = NodeUtil.minKeysFromOrder(order);
    let keyIndex = NodeUtil.getKeyIndex<K, V>(internalNode.data, compare, deleteKey);

    // match on both the result of the node binary search, and if this node level should be skipped even if the key is found (internal kv replacement case)
    switch (keyIndex, skipNode) {
      // if key is found in the internal node
      case (#keyFound(deleteIndex), false) {
        let deletedValue = switch (internalNode.data.kvs[deleteIndex]) {
          case (?kv) { ?kv.1 };
          case null { assert false; null }
        };
        // TODO: (optimization) replace with deletion in one step without having to retrieve the maxKey first
        let replaceKV = NodeUtil.getMaxKeyValue(internalNode.children[deleteIndex]);
        internalNode.data.kvs[deleteIndex] := ?replaceKV;
        switch (internalDeleteHelper(internalNode, order, compare, replaceKV.0, true)) {
          case (#delete(_)) { #delete(deletedValue) };
          case (#mergeChild({ internalChild })) {
            #mergeChild({ internalChild; deletedValue })
          }
        }
      };
      // if key is not found in the internal node OR the key is found, but skipping this node (because deleting the in order precessor i.e. replacement kv)
      // in both cases need to descend and traverse to find the kv to delete
      case ((#keyFound(_), true) or (#notFound(_), _)) {
        let childIndex = switch (keyIndex) {
          case (#keyFound(replacedSkipKeyIndex)) { replacedSkipKeyIndex };
          case (#notFound(childIndex)) { childIndex }
        };
        let child = switch (internalNode.children[childIndex]) {
          case (?c) { c };
          case null {
            Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.internalDeleteHelper, child index of #keyFound or #notfound is null")
          }
        };
        switch (child) {
          // if child is internal
          case (#internal(internalChild)) {
            switch (internalDeleteHelper(internalChild, order, compare, deleteKey, false), childIndex == 0) {
              // if value was successfully deleted and no additional tree re-balancing is needed, return the deleted value
              case (#delete(v), _) { #delete(v) };
              // if internalChild needs rebalancing and pulling child is left most
              case (#mergeChild({ internalChild; deletedValue }), true) {
                // try to pull left-most key and child from right sibling
                switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex + 1, #successor)) {
                  // if can pull up sibling kv and child
                  case (#borrowed({ deletedSiblingKVPair; child })) {
                    NodeUtil.rotateBorrowedKVsAndChildFromSibling(
                      internalNode,
                      childIndex,
                      deletedSiblingKVPair,
                      child,
                      internalChild,
                      #right
                    );
                    #delete(deletedValue)
                  };
                  // unable to pull from sibling, need to merge with right sibling and push down parent
                  case (#notEnoughKeys(sibling)) {
                    // get the parent kv that will be pushed down the the child
                    let kvPairToBePushedToChild = ?ArrayUtil.deleteAndShiftValuesOver(internalNode.data.kvs, 0);
                    internalNode.data.count -= 1;
                    // merge the children and push down the parent
                    let newChild = NodeUtil.mergeChildrenAndPushDownParent<K, V>(internalChild, kvPairToBePushedToChild, sibling);
                    // update children of the parent
                    internalNode.children[0] := ?#internal(newChild);
                    ignore ?ArrayUtil.deleteAndShiftValuesOver(internalNode.children, 1);

                    if (internalNode.data.count < minKeys) {
                      #mergeChild({ internalChild = internalNode; deletedValue })
                    } else {
                      #delete(deletedValue)
                    }
                  }
                }
              };
              // if internalChild needs rebalancing and pulling child is > 0, so a left sibling exists
              case (#mergeChild({ internalChild; deletedValue }), false) {
                // try to pull right-most key and its child directly from left sibling
                switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex - 1 : Nat, #predecessor)) {
                  case (#borrowed({ deletedSiblingKVPair; child })) {
                    NodeUtil.rotateBorrowedKVsAndChildFromSibling(
                      internalNode,
                      childIndex - 1 : Nat,
                      deletedSiblingKVPair,
                      child,
                      internalChild,
                      #left
                    );
                    #delete(deletedValue)
                  };
                  // unable to pull from left sibling
                  case (#notEnoughKeys(leftSibling)) {
                    // if child is not last index, try to pull from the right child
                    if (childIndex < internalNode.data.count) {
                      switch (NodeUtil.borrowFromInternalSibling(internalNode.children, childIndex, #successor)) {
                        // if can pull up sibling kv and child
                        case (#borrowed({ deletedSiblingKVPair; child })) {
                          NodeUtil.rotateBorrowedKVsAndChildFromSibling(
                            internalNode,
                            childIndex,
                            deletedSiblingKVPair,
                            child,
                            internalChild,
                            #right
                          );
                          return #delete(deletedValue)
                        };
                        // if cannot borrow, from left or right, merge (see below)
                        case _ {}
                      }
                    };

                    // get the parent kv that will be pushed down the the child
                    let kvPairToBePushedToChild = ?ArrayUtil.deleteAndShiftValuesOver(internalNode.data.kvs, childIndex - 1 : Nat);
                    internalNode.data.count -= 1;
                    // merge it the children and push down the parent
                    let newChild = NodeUtil.mergeChildrenAndPushDownParent(leftSibling, kvPairToBePushedToChild, internalChild);

                    // update children of the parent
                    internalNode.children[childIndex - 1] := ?#internal(newChild);
                    ignore ?ArrayUtil.deleteAndShiftValuesOver(internalNode.children, childIndex);

                    if (internalNode.data.count < minKeys) {
                      #mergeChild({ internalChild = internalNode; deletedValue })
                    } else {
                      #delete(deletedValue)
                    }
                  }
                }
              }
            }
          };
          // if child is leaf
          case (#leaf(leafChild)) {
            switch (leafDeleteHelper(leafChild, order, compare, deleteKey), childIndex == 0) {
              case (#delete(value), _) { #delete(value) };
              // if delete child is left most, try to borrow from right child
              case (#mergeLeafData({ leafDeleteIndex }), true) {
                switch (NodeUtil.borrowFromRightLeafChild(internalNode.children, childIndex)) {
                  case (?borrowedKVPair) {
                    let kvPairToBePushedToChild = internalNode.data.kvs[childIndex];
                    internalNode.data.kvs[childIndex] := ?borrowedKVPair;

                    let deletedKV = ArrayUtil.insertAtPostionAndDeleteAtPosition<(K, V)>(leafChild.data.kvs, kvPairToBePushedToChild, leafChild.data.count - 1, leafDeleteIndex);
                    #delete(?deletedKV.1)
                  };

                  case null {
                    // can't borrow from right child, delete from leaf and merge with right child and parent kv, then push down into new leaf
                    let rightChild = switch (internalNode.children[childIndex + 1]) {
                      case (?#leaf(rc)) { rc };
                      case _ {
                        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.internalDeleteHelper, if trying to borrow from right leaf child is null, rightChild index cannot be null or internal")
                      }
                    };
                    let (mergedLeaf, deletedKV) = mergeParentWithLeftRightChildLeafNodesAndDelete(
                      internalNode.data.kvs[childIndex],
                      leafChild,
                      rightChild,
                      leafDeleteIndex,
                      #left
                    );
                    // delete the left most internal node kv, since was merging from a deletion in left most child (0) and the parent kv was pushed into the mergedLeaf
                    ignore ArrayUtil.deleteAndShiftValuesOver<(K, V)>(internalNode.data.kvs, 0);
                    // update internal node children
                    ArrayUtil.replaceTwoWithElementAndShift<Node<K, V>>(internalNode.children, #leaf(mergedLeaf), 0);
                    internalNode.data.count -= 1;

                    if (internalNode.data.count < minKeys) {
                      #mergeChild({
                        internalChild = internalNode;
                        deletedValue = ?deletedKV.1
                      })
                    } else {
                      #delete(?deletedKV.1)
                    }

                  }
                }
              };
              // if delete child is middle or right most, try to borrow from left child
              case (#mergeLeafData({ leafDeleteIndex }), false) {
                // if delete child is right most, try to borrow from left child
                switch (NodeUtil.borrowFromLeftLeafChild(internalNode.children, childIndex)) {
                  case (?borrowedKVPair) {
                    let kvPairToBePushedToChild = internalNode.data.kvs[childIndex - 1];
                    internalNode.data.kvs[childIndex - 1] := ?borrowedKVPair;
                    let kvDelete = ArrayUtil.insertAtPostionAndDeleteAtPosition<(K, V)>(leafChild.data.kvs, kvPairToBePushedToChild, 0, leafDeleteIndex);
                    #delete(?kvDelete.1)
                  };
                  case null {
                    // if delete child is in the middle, try to borrow from right child
                    if (childIndex < internalNode.data.count) {
                      // try to borrow from right
                      switch (NodeUtil.borrowFromRightLeafChild(internalNode.children, childIndex)) {
                        case (?borrowedKVPair) {
                          let kvPairToBePushedToChild = internalNode.data.kvs[childIndex];
                          internalNode.data.kvs[childIndex] := ?borrowedKVPair;
                          // insert the successor at the very last element
                          let kvDelete = ArrayUtil.insertAtPostionAndDeleteAtPosition<(K, V)>(leafChild.data.kvs, kvPairToBePushedToChild, leafChild.data.count - 1, leafDeleteIndex);
                          return #delete(?kvDelete.1)
                        };
                        // if cannot borrow, from left or right, merge (see below)
                        case _ {}
                      }
                    };

                    // can't borrow from left child, delete from leaf and merge with left child and parent kv, then push down into new leaf
                    let leftChild = switch (internalNode.children[childIndex - 1]) {
                      case (?#leaf(lc)) { lc };
                      case _ {
                        Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.internalDeleteHelper, if trying to borrow from left leaf child is null, then left child index must not be null or internal")
                      }
                    };
                    let (mergedLeaf, deletedKV) = mergeParentWithLeftRightChildLeafNodesAndDelete(
                      internalNode.data.kvs[childIndex - 1],
                      leftChild,
                      leafChild,
                      leafDeleteIndex,
                      #right
                    );
                    // delete the right most internal node kv, since was merging from a deletion in the right most child and the parent kv was pushed into the mergedLeaf
                    ignore ArrayUtil.deleteAndShiftValuesOver<(K, V)>(internalNode.data.kvs, childIndex - 1);
                    // update internal node children
                    ArrayUtil.replaceTwoWithElementAndShift<Node<K, V>>(internalNode.children, #leaf(mergedLeaf), childIndex - 1);
                    internalNode.data.count -= 1;

                    if (internalNode.data.count < minKeys) {
                      #mergeChild({
                        internalChild = internalNode;
                        deletedValue = ?deletedKV.1
                      })
                    } else {
                      #delete(?deletedKV.1)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateLeafDeleteResult<K, V> = {
    // element was deleted or not found, returning the old value (?value or null)
    #delete : ?V;
    // leaf had the minimum number of keys when deleting, so returns the leaf node's data and the index of the key that will be deleted
    #mergeLeafData : {
      data : Data<K, V>;
      leafDeleteIndex : Nat
    }
  };

  func leafDeleteHelper<K, V>(leafNode : Leaf<K, V>, order : Nat, compare : (K, K) -> Order.Order, deleteKey : K) : IntermediateLeafDeleteResult<K, V> {
    let minKeys = NodeUtil.minKeysFromOrder(order);

    switch (NodeUtil.getKeyIndex<K, V>(leafNode.data, compare, deleteKey)) {
      case (#keyFound(deleteIndex)) {
        if (leafNode.data.count > minKeys) {
          leafNode.data.count -= 1;
          #delete(?ArrayUtil.deleteAndShiftValuesOver<(K, V)>(leafNode.data.kvs, deleteIndex).1)
        } else {
          #mergeLeafData({
            data = leafNode.data;
            leafDeleteIndex = deleteIndex
          })
        }
      };
      case (#notFound(_)) {
        #delete(null)
      }
    }
  };

  // get helper if internal node
  func getFromInternal<K, V>(internalNode : Internal<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V {
    switch (NodeUtil.getKeyIndex<K, V>(internalNode.data, compare, key)) {
      case (#keyFound(index)) {
        getExistingValueFromIndex(internalNode.data, index)
      };
      case (#notFound(index)) {
        switch (internalNode.children[index]) {
          // expects the child to be there, otherwise there's a bug in binary search or the tree is invalid
          case null { assert false; null };
          case (?#leaf(leafNode)) { getFromLeaf(leafNode, compare, key) };
          case (?#internal(internalNode)) {
            getFromInternal(internalNode, compare, key)
          }
        }
      }
    }
  };

  // get function helper if leaf node
  func getFromLeaf<K, V>(leafNode : Leaf<K, V>, compare : (K, K) -> Order.Order, key : K) : ?V {
    switch (NodeUtil.getKeyIndex<K, V>(leafNode.data, compare, key)) {
      case (#keyFound(index)) {
        getExistingValueFromIndex(leafNode.data, index)
      };
      case _ null
    }
  };

  // get function helper that retrieves an existing value in the case that the key is found
  func getExistingValueFromIndex<K, V>(data : Data<K, V>, index : Nat) : ?V {
    switch (data.kvs[index]) {
      case null { null };
      case (?ov) { ?ov.1 }
    }
  };

  // which child the deletionIndex is referring to
  type DeletionSide = { #left; #right };

  func mergeParentWithLeftRightChildLeafNodesAndDelete<K, V>(
    parentKV : ?(K, V),
    leftChild : Leaf<K, V>,
    rightChild : Leaf<K, V>,
    deleteIndex : Nat,
    deletionSide : DeletionSide
  ) : (Leaf<K, V>, (K, V)) {
    let count = leftChild.data.count * 2;
    let (kvs, deletedKV) = ArrayUtil.mergeParentWithChildrenAndDelete<(K, V)>(
      parentKV,
      leftChild.data.count,
      leftChild.data.kvs,
      rightChild.data.kvs,
      deleteIndex,
      deletionSide
    );
    (
      {
        data = {
          kvs;
          var count = count
        }
      },
      deletedKV
    )
  };

  // This type is used to signal to the parent calling context what happened in the level below
  type IntermediateInsertResult<K, V> = {
    // element was inserted or replaced, returning the old value (?value or null)
    #insert : ?V;
    // child was full when inserting, so returns the promoted kv pair and the split left and right child
    #promote : {
      kv : (K, V);
      leftChild : Node<K, V>;
      rightChild : Node<K, V>
    }
  };

  // Helper for inserting into a leaf node
  func leafInsertHelper<K, V>(leafNode : Leaf<K, V>, order : Nat, compare : (K, K) -> Order.Order, key : K, value : V) : (IntermediateInsertResult<K, V>) {
    // Perform binary search to see if the element exists in the node
    switch (NodeUtil.getKeyIndex<K, V>(leafNode.data, compare, key)) {
      case (#keyFound(insertIndex)) {
        let previous = leafNode.data.kvs[insertIndex];
        leafNode.data.kvs[insertIndex] := ?(key, value);
        switch (previous) {
          case (?ov) { #insert(?ov.1) };
          case null { assert false; #insert(null) }; // the binary search already found an element, so this case should never happen
        }
      };
      case (#notFound(insertIndex)) {
        // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
        let maxKeys : Nat = order - 1;
        // If the leaf is full, insert, split the node, and promote the middle element
        if (leafNode.data.count >= maxKeys) {
          let (leftKVs, promotedParentElement, rightKVs) = ArrayUtil.insertOneAtIndexAndSplitArray(
            leafNode.data.kvs,
            (key, value),
            insertIndex
          );

          let leftCount = order / 2;
          let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
            leftCount
          };

          (
            #promote({
              kv = promotedParentElement;
              leftChild = createLeaf<K, V>(leftKVs, leftCount);
              rightChild = createLeaf<K, V>(rightKVs, rightCount)
            })
          )
        }
        // Otherwise, insert at the specified index (shifting elements over if necessary)
        else {
          NodeUtil.insertAtIndexOfNonFullNodeData<K, V>(leafNode.data, ?(key, value), insertIndex);
          #insert(null)
        }
      }
    }
  };

  // Helper for inserting into an internal node
  func internalInsertHelper<K, V>(internalNode : Internal<K, V>, order : Nat, compare : (K, K) -> Order.Order, key : K, value : V) : IntermediateInsertResult<K, V> {
    switch (NodeUtil.getKeyIndex<K, V>(internalNode.data, compare, key)) {
      case (#keyFound(insertIndex)) {
        let previous = internalNode.data.kvs[insertIndex];
        internalNode.data.kvs[insertIndex] := ?(key, value);
        switch (previous) {
          case (?ov) { #insert(?ov.1) };
          case null { assert false; #insert(null) }; // the binary search already found an element, so this case should never happen
        }
      };
      case (#notFound(insertIndex)) {
        let insertResult = switch (internalNode.children[insertIndex]) {
          case null { assert false; #insert(null) };
          case (?#leaf(leafNode)) {
            leafInsertHelper(leafNode, order, compare, key, value)
          };
          case (?#internal(internalChildNode)) {
            internalInsertHelper(internalChildNode, order, compare, key, value)
          }
        };

        switch (insertResult) {
          case (#insert(ov)) { #insert(ov) };
          case (#promote({ kv; leftChild; rightChild })) {
            // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
            let maxKeys : Nat = order - 1;
            // if current internal node is full, need to split the internal node
            if (internalNode.data.count >= maxKeys) {
              // insert and split internal kvs, determine new promotion target kv
              let (leftKVs, promotedParentElement, rightKVs) = ArrayUtil.insertOneAtIndexAndSplitArray(
                internalNode.data.kvs,
                (kv),
                insertIndex
              );

              // calculate the element count in the left KVs and the element count in the right KVs
              let leftCount = order / 2;
              let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
                leftCount
              };

              // split internal children
              let (leftChildren, rightChildren) = NodeUtil.splitChildrenInTwoWithRebalances<K, V>(
                internalNode.children,
                insertIndex,
                leftChild,
                rightChild
              );

              // send the kv to be promoted, as well as the internal children left and right split
              #promote({
                kv = promotedParentElement;
                leftChild = #internal({
                  data = { kvs = leftKVs; var count = leftCount };
                  children = leftChildren
                });
                rightChild = #internal({
                  data = { kvs = rightKVs; var count = rightCount };
                  children = rightChildren
                })
              })
            } else {
              // insert the new kvs into the internal node
              NodeUtil.insertAtIndexOfNonFullNodeData(internalNode.data, ?kv, insertIndex);
              // split and re-insert the single child that needs rebalancing
              NodeUtil.insertRebalancedChild(internalNode.children, insertIndex, leftChild, rightChild);
              #insert(null)
            }
          }
        }
      }
    }
  };

  // // TODO: Think if want to combine this with the leafInsertHelper
  // // Helper for updating an element in a leaf node
  // func leafUpdateHelper<K, V>(leafNode : Leaf<K, V>, order : Nat, compare : (K, K) -> O.Order, key : K, updateFunction : (?V) -> V) : (IntermediateInsertResult<K, V>) {
  //   // Perform binary search to see if the element exists in the node
  //   switch (NU.getKeyIndex<K, V>(leafNode.data, compare, key)) {
  //     case (#keyFound(insertIndex)) {
  //       let previous = leafNode.data.kvs[insertIndex];
  //       switch (previous) {
  //         case (?ov) {
  //           leafNode.data.kvs[insertIndex] := ?(key, updateFunction(?ov.1));
  //           #insert(?ov.1)
  //         };
  //         // the binary search has already found an element, so this case should never happen
  //         case null {
  //           Debug.trap("UNREACHABLE_ERROR: file a bug report! In leafUpdateHelper, when matching on a #keyFound previous value, the previous kv turned out to be null")
  //         }
  //       }
  //     };
  //     case (#notFound(insertIndex)) {
  //       // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
  //       let maxKeys : Nat = order - 1;
  //       // If the leaf is full, insert, split the node, and promote the middle element
  //       if (leafNode.data.count >= maxKeys) {
  //         let (leftKVs, promotedParentElement, rightKVs) = AU.insertOneAtIndexAndSplitArray(
  //           leafNode.data.kvs,
  //           (key, updateFunction(null)),
  //           insertIndex
  //         );

  //         let leftCount = order / 2;
  //         let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
  //           leftCount
  //         };

  //         (
  //           #promote({
  //             kv = promotedParentElement;
  //             leftChild = createLeaf<K, V>(leftKVs, leftCount);
  //             rightChild = createLeaf<K, V>(rightKVs, rightCount)
  //           })
  //         )
  //       }
  //       // Otherwise, insert at the specified index (shifting elements over if necessary)
  //       else {
  //         NU.insertAtIndexOfNonFullNodeData<K, V>(leafNode.data, ?(key, updateFunction(null)), insertIndex);
  //         #insert(null)
  //       }
  //     }
  //   }
  // };

  // // TODO: Think if want to combine this with the internalInsertHelper
  // // Helper for inserting into an internal node
  // func internalUpdateHelper<K, V>(internalNode : Internal<K, V>, order : Nat, compare : (K, K) -> O.Order, key : K, updateFunction : (?V) -> V) : IntermediateInsertResult<K, V> {
  //   switch (NU.getKeyIndex<K, V>(internalNode.data, compare, key)) {
  //     case (#keyFound(insertIndex)) {
  //       let previous = internalNode.data.kvs[insertIndex];
  //       switch (previous) {
  //         case (?ov) {
  //           internalNode.data.kvs[insertIndex] := ?(key, updateFunction(?ov.1));
  //           #insert(?ov.1)
  //         };
  //         // the binary search has already found an element, so this case should never happen
  //         case null {
  //           Debug.trap("UNREACHABLE_ERROR: file a bug report! In internalUpdateHelper, when matching on a #keyFound previous value, the previous kv turned out to be null")
  //         }
  //       }
  //     };
  //     case (#notFound(insertIndex)) {
  //       let updateResult = switch (internalNode.children[insertIndex]) {
  //         case null { assert false; #insert(null) };
  //         case (?#leaf(leafNode)) {
  //           leafUpdateHelper(leafNode, order, compare, key, updateFunction)
  //         };
  //         case (?#internal(internalChildNode)) {
  //           internalUpdateHelper(internalChildNode, order, compare, key, updateFunction)
  //         }
  //       };

  //       switch (updateResult) {
  //         case (#insert(ov)) { #insert(ov) };
  //         case (#promote({ kv; leftChild; rightChild })) {
  //           // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
  //           let maxKeys : Nat = order - 1;
  //           // if current internal node is full, need to split the internal node
  //           if (internalNode.data.count >= maxKeys) {
  //             // insert and split internal kvs, determine new promotion target kv
  //             let (leftKVs, promotedParentElement, rightKVs) = AU.insertOneAtIndexAndSplitArray(
  //               internalNode.data.kvs,
  //               (kv),
  //               insertIndex
  //             );

  //             // calculate the element count in the left KVs and the element count in the right KVs
  //             let leftCount = order / 2;
  //             let rightCount : Nat = if (order % 2 == 0) { leftCount - 1 } else {
  //               leftCount
  //             };

  //             // split internal children
  //             let (leftChildren, rightChildren) = NU.splitChildrenInTwoWithRebalances<K, V>(
  //               internalNode.children,
  //               insertIndex,
  //               leftChild,
  //               rightChild
  //             );

  //             // send the kv to be promoted, as well as the internal children left and right split
  //             #promote({
  //               kv = promotedParentElement;
  //               leftChild = #internal({
  //                 data = { kvs = leftKVs; var count = leftCount };
  //                 children = leftChildren
  //               });
  //               rightChild = #internal({
  //                 data = { kvs = rightKVs; var count = rightCount };
  //                 children = rightChildren
  //               })
  //             })
  //           } else {
  //             // insert the new kvs into the internal node
  //             NU.insertAtIndexOfNonFullNodeData(internalNode.data, ?kv, insertIndex);
  //             // split and re-insert the single child that needs rebalancing
  //             NU.insertRebalancedChild(internalNode.children, insertIndex, leftChild, rightChild);
  //             #insert(null)
  //           }
  //         }
  //       }
  //     }
  //   }
  // };

  func createLeaf<K, V>(kvs : [var ?(K, V)], count : Nat) : Node<K, V> {
    #leaf({
      data = {
        kvs;
        var count
      }
    })
  };

  // /// Opinionated version of generating a textual representation of a BTree. Primarily to be used
  // /// for testing and debugging
  // public func toText<K, V>(t : BTree<K, V>, keyToText : K -> Text, valueToText : V -> Text) : Text {
  //   var textOutput = "BTree={";
  //   textOutput #= "root=" # rootToText<K, V>(t.root, keyToText, valueToText) # "; ";
  //   textOutput #= "size=" # Nat.toText(t.size) # "; ";
  //   textOutput #= "order=" # Nat.toText(t.order) # "; ";
  //   textOutput # "}"
  // };

  // /// Determines if two BTrees are equivalent
  // public func equals<K, V>(
  //   t1 : BTree<K, V>,
  //   t2 : BTree<K, V>,
  //   keyEquals : (K, K) -> Bool,
  //   valueEquals : (V, V) -> Bool
  // ) : Bool {
  //   if (t1.order != t2.order or t1.size != t2.size) return false;

  //   nodeEquals(t1.root, t2.root, keyEquals, valueEquals)
  // };

  // func rootToText<K, V>(node : Node<K, V>, keyToText : K -> Text, valueToText : V -> Text) : Text {
  //   var rootText = "{";
  //   switch (node) {
  //     case (#leaf(leafNode)) {
  //       rootText #= "#leaf=" # leafToText(leafNode, keyToText, valueToText)
  //     };
  //     case (#internal(internalNode)) {
  //       rootText #= "#internal=" # internalToText(internalNode, keyToText, valueToText)
  //     }
  //   };

  //   rootText
  // };

  // func leafToText<K, V>(leaf : Leaf<K, V>, keyToText : K -> Text, valueToText : V -> Text) : Text {
  //   var leafText = "{data=";
  //   leafText #= dataToText(leaf.data, keyToText, valueToText);
  //   leafText # "}"
  // };

  // func internalToText<K, V>(internal : Internal<K, V>, keyToText : K -> Text, valueToText : V -> Text) : Text {
  //   var internalText = "{";
  //   internalText #= "data=" # dataToText(internal.data, keyToText, valueToText) # "; ";
  //   internalText #= "children=[";

  //   var i = 0;
  //   while (i < internal.children.size()) {
  //     switch (internal.children[i]) {
  //       case null { internalText #= "null" };
  //       case (?(#leaf(leafNode))) {
  //         internalText #= "#leaf=" # leafToText(leafNode, keyToText, valueToText)
  //       };
  //       case (?(#internal(internalNode))) {
  //         internalText #= "#internal=" # internalToText(internalNode, keyToText, valueToText)
  //       }
  //     };
  //     internalText #= ", ";
  //     i += 1
  //   };

  //   internalText # "]}"
  // };

  // func dataToText<K, V>(data : Data<K, V>, keyToText : K -> Text, valueToText : V -> Text) : Text {
  //   var dataText = "{kvs=[";
  //   var i = 0;
  //   while (i < data.kvs.size()) {
  //     switch (data.kvs[i]) {
  //       case null { dataText #= "null, " };
  //       case (?(k, v)) {
  //         dataText #= "(key={" # keyToText(k) # "}, value={" # valueToText(v) # "}), "
  //       }
  //     };

  //     i += 1
  //   };

  //   dataText #= "]; count=" # Nat.toText(data.count) # ";}";
  //   dataText
  // };

  // func nodeEquals<K, V>(
  //   n1 : Node<K, V>,
  //   n2 : Node<K, V>,
  //   keyEquals : (K, K) -> Bool,
  //   valueEquals : (V, V) -> Bool
  // ) : Bool {
  //   switch (n1, n2) {
  //     case (#leaf(l1), #leaf(l2)) {
  //       dataEquals(l1.data, l2.data, keyEquals, valueEquals)
  //     };
  //     case (#internal(i1), #internal(i2)) {
  //       dataEquals(i1.data, i2.data, keyEquals, valueEquals) and childrenEquals(i1.children, i2.children, keyEquals, valueEquals)
  //     };
  //     case _ { false }
  //   }
  // };

  // func childrenEquals<K, V>(
  //   c1 : [var ?Node<K, V>],
  //   c2 : [var ?Node<K, V>],
  //   keyEquals : (K, K) -> Bool,
  //   valueEquals : (V, V) -> Bool
  // ) : Bool {
  //   if (c1.size() != c2.size()) { return false };

  //   var i = 0;
  //   while (i < c1.size()) {
  //     switch (c1[i], c2[i]) {
  //       case (null, null) {};
  //       case (?n1, ?n2) {
  //         if (not nodeEquals(n1, n2, keyEquals, valueEquals)) {
  //           return false
  //         }
  //       };
  //       case _ { return false }
  //     };

  //     i += 1
  //   };

  //   true
  // };

  // func dataEquals<K, V>(
  //   d1 : Data<K, V>,
  //   d2 : Data<K, V>,
  //   keyEquals : (K, K) -> Bool,
  //   valueEquals : (V, V) -> Bool
  // ) : Bool {
  //   if (d1.count != d2.count) { return false };
  //   if (d1.kvs.size() != d2.kvs.size()) { return false };

  //   var i = 0;
  //   while (i < d1.kvs.size()) {
  //     switch (d1.kvs[i], d2.kvs[i]) {
  //       case (null, null) {};
  //       case (?(k1, v1), ?(k2, v2)) {
  //         if (
  //           (not keyEquals(k1, k2)) or
  //           (not valueEquals(v1, v2))
  //         ) { return false }
  //       };
  //       case _ { return false }
  //     };

  //     i += 1
  //   };

  //   true
  // };

  // Additional functionality compared to original source.
  func cloneNode<K, V>(node : Node<K, V>) : Node<K, V> {
    switch node {
      case (#leaf _) { node };
      case (#internal { data; children }) {
        let clonedKeyValueSet = VarArray.map<?(K, V), ?(K, V)>(data.kvs, func entry { entry });
        let clonedData = {
          kvs = clonedKeyValueSet;
          var count = data.count
        };
        let clonedChildren = VarArray.map<?Node<K, V>, ?Node<K, V>>(
          children,
          func child {
            switch child {
              case null null;
              case (?childNode) ?cloneNode(childNode)
            }
          }
        );
        # internal({
          data = clonedData;
          children = clonedChildren
        })
      }
    }
  };

  // public func fromIter<K, V>(iter : Iter.Iter<(K, V)>, compare : (K, K) -> Order.Order) : Map<K, V> {
  //   todo()
  // };

  // public func forEach<K, V>(map : Map<K, V>, f : (K, V) -> ()) {
  //   todo()
  // };

  // public func filter<K, V>(map : Map<K, V>, f : (K, V) -> Bool) : Map<K, V> {
  //   todo()
  // };

  // public func map<K, V1, V2>(map : Map<K, V1>, f : (K, V1) -> V2) : Map<K, V2> {
  //   todo()
  // };

  // public func foldLeft<K, V, A>(
  //   map : Map<K, V>,
  //   base : A,
  //   combine : (A, K, V) -> A
  // ) : A {
  //   todo()
  // };

  // public func foldRight<K, V, A>(
  //   map : Map<K, V>,
  //   base : A,
  //   combine : (K, V, A) -> A
  // ) : A {
  //   todo()
  // };

  // public func all<K, V>(map : Map<K, V>, pred : (K, V) -> Bool) : Bool {
  //   todo()
  // };

  // public func any<K, V>(map : Map<K, V>, pred : (K, V) -> Bool) : Bool {
  //   todo()
  // };

  // public func filterMap<K, V1, V2>(map : Map<K, V1>, f : (K, V1) -> ?V2) : Map<K, V2> {
  //   todo()
  // };

  // public func assertValid<K>(map : Map<K, Any>, compare : (K, K) -> Order.Order) : () {
  //   todo()
  // };

  // public func toText<K, V>(map : Map<K, V>, kf : K -> Text, vf : V -> Text) : Text {
  //   todo()
  // };

  // public func compare<K, V>(map1 : Map<K, V>, map2 : Map<K, V>, compareKey : (K, K) -> Order.Order, compareValue : (V, V) -> Order.Order) : Order.Order {
  //   todo()
  // };

  module ArrayUtil {
    /// Inserts an element into a mutable array at a specific index, shifting all other elements over
    ///
    /// Parameters:
    ///
    /// array - the array being inserted into
    /// insertElement - the element being inserted
    /// insertIndex - the index at which the element will be inserted
    /// currentLastElementIndex - the index of last **non-null** element in the array (used to start shifting elements over)
    ///
    /// Note: This assumes that there are nulls at the end of the array and that the array is not full.
    /// If the array is already full, this function will overflow the array size when attempting to
    /// insert and will cause the cansiter to trap
    public func insertAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, currentLastElementIndex : Nat) : () {
      // if inserting at the end of the array, don't need to do any shifting and can just insert and return
      if (insertIndex == currentLastElementIndex + 1) {
        array[insertIndex] := insertElement;
        return
      };

      // otherwise, need to shift all of the elements at the end of the array over one by one until
      // the insert index is hit.
      var j = currentLastElementIndex;
      label l loop {
        array[j + 1] := array[j];
        if (j == insertIndex) {
          array[j] := insertElement;
          break l
        };

        j -= 1
      }
    };

    /// Splits the array into two halves as if the insert has occured, omitting the middle element and returning it so that it can
    /// be promoted to the parent internal node. This is used when inserting an element into an array of key-value data pairs that
    /// is already full.
    ///
    /// Note: Use only when inserting an element into a FULL array & promoting the resulting midpoint element.
    /// This is NOT the same as just splitting this array!
    ///
    /// Parameters:
    ///
    /// array - the array being split
    /// insertElement - the element being inserted
    /// insertIndex - the position/index that the insertElement should be inserted
    public func insertOneAtIndexAndSplitArray<T>(array : [var ?T], insertElement : T, insertIndex : Nat) : ([var ?T], T, [var ?T]) {
      // split at the BTree order / 2
      let splitIndex = (array.size() + 1) / 2;
      // this function assumes the the splitIndex is in the middle of the kvs array - trap otherwise
      if (splitIndex > array.size()) { assert false };

      let leftSplit = if (insertIndex < splitIndex) {
        VarArray.generate<?T>(
          array.size(),
          func(i) {
            // if below the split index
            if (i < splitIndex) {
              // if below the insert index, copy over
              if (i < insertIndex) { array[i] }
              // if less than the insert index, copy over the previous element (since the inserted element has taken up 1 extra slot)
              else if (i > insertIndex) { array[i - 1] }
              // if equal to the insert index add the element to be inserted to the left split
              else { ?insertElement }
            } else { null }
          }
        )
      }
      // index >= splitIndex
      else {
        VarArray.generate<?T>(
          array.size(),
          func(i) {
            // right biased splitting
            if (i < splitIndex) { array[i] } else { null }
          }
        )
      };

      let (rightSplit, middleElement) : ([var ?T], ?T) =
      // if insert > split index, inserted element will be inserted into the right split
      if (insertIndex > splitIndex) {
        let right = VarArray.generate<?T>(
          array.size(),
          func(i) {
            let adjIndex = i + splitIndex + 1; // + 1 accounts for the fact that the split element was part of the original array
            if (adjIndex <= array.size()) {
              if (adjIndex < insertIndex) { array[adjIndex] } else if (adjIndex > insertIndex) {
                array[adjIndex - 1]
              } else { ?insertElement }
            } else { null }
          }
        );
        (right, array[splitIndex])
      }
      // if inserted element was placed in the left split
      else if (insertIndex < splitIndex) {
        let right = VarArray.generate<?T>(
          array.size(),
          func(i) {
            let adjIndex = i + splitIndex;
            if (adjIndex < array.size()) { array[adjIndex] } else { null }
          }
        );
        (right, array[splitIndex - 1])
      }
      // insertIndex == splitIndex
      else {
        let right = VarArray.generate<?T>(
          array.size(),
          func(i) {
            let adjIndex = i + splitIndex;
            if (adjIndex < array.size()) { array[adjIndex] } else { null }
          }
        );
        (right, ?insertElement)
      };

      switch (middleElement) {
        case null {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.ArrayUtil.insertOneAtIndexAndSplitArray, middle element of a BTree node should never be null")
        };
        case (?el) { (leftSplit, el, rightSplit) }
      }
    };

    /// Context of use: This function is used after inserting a child node into the full child of an internal node that is also full.
    /// From the insertion, the full child is rebalanced and split, and then since the internal node is full, when replacing the two
    /// halves of that rebalanced child into the internal node's children this causes a second split. This function takes in the
    /// internal node's children, and the "rebalanced" split child nodes, as well as the index at which the "rebalanced" left and right
    /// child will be inserted and replaces the original child with those two halves
    ///
    /// Note: Use when inserting two successive elements into a FULL array and splitting that array.
    /// This is NOT the same as just splitting this array!
    ///
    /// Assumptions: this function also assumes that the children array is full (no nulls)
    ///
    /// Parameters:
    ///
    /// children - the internal node's children array being split
    /// rebalancedChildIndex - the index used to mark where the rebalanced left and right children will be inserted
    /// leftChildInsert - the rebalanced left child being inserted
    /// rightChildInsert - the rebalanced right child being inserted
    public func splitArrayAndInsertTwo<T>(children : [var ?T], rebalancedChildIndex : Nat, leftChildInsert : T, rightChildInsert : T) : ([var ?T], [var ?T]) {
      let splitIndex = children.size() / 2;

      let leftRebalancedChildren = VarArray.generate<?T>(
        children.size(),
        func(i) {
          // only insert elements up to the split index and fill the rest of the children with nulls
          if (i <= splitIndex) {
            if (i < rebalancedChildIndex) { children[i] }
            // insert the left and right rebalanced child halves if the rebalancedChildIndex comes before the splitIndex
            else if (i == rebalancedChildIndex) {
              ?leftChildInsert
            } else if (i == rebalancedChildIndex + 1) { ?rightChildInsert } else {
              children[i - 1]
            } // i > rebalancedChildIndex
          } else { null }
        }
      );

      let rightRebalanceChildren : [var ?T] =
      // Case 1: if both left and right rebalanced halves were inserted into the left child can just go from the split index onwards
      if (rebalancedChildIndex + 1 <= splitIndex) {
        VarArray.generate<?T>(
          children.size(),
          func(i) {
            let adjIndex = i + splitIndex;
            if (adjIndex < children.size()) { children[adjIndex] } else { null }
          }
        )
      }
      // Case 2: if both left and right rebalanced halves will be inserted into the right child
      else if (rebalancedChildIndex > splitIndex) {
        var rebalanceOffset = 0;
        VarArray.generate<?T>(
          children.size(),
          func(i) {
            let adjIndex = i + splitIndex + 1;
            if (adjIndex == rebalancedChildIndex) { ?leftChildInsert } else if (adjIndex == rebalancedChildIndex + 1) {
              rebalanceOffset := 1; // after inserting both rebalanced children, any elements coming after are from the previous index
              ?rightChildInsert
            } else if (adjIndex <= children.size()) {
              children[adjIndex - rebalanceOffset]
            } else { null }
          }
        )
      }
      // Case 3: if left rebalanced half was in left child, and right rebalanced half will be in right child
      // rebalancedChildIndex == splitIndex
      else {
        VarArray.generate<?T>(
          children.size(),
          func(i) {
            // first element is the right rebalanced half
            if (i == 0) { ?rightChildInsert } else {
              let adjIndex = i + splitIndex;
              if (adjIndex < children.size()) { children[adjIndex] } else {
                null
              }
            }
          }
        )
      };

      (leftRebalancedChildren, rightRebalanceChildren)
    };

    /// Specific to the BTree delete implementation (assumes node ordering such that nulls come at the end of the array)
    ///
    /// Assumptions:
    /// * All nulls come at the end of the array
    /// * Assumes the delete index provided is correct and non null - will trap otherwise
    /// * deleteIndex < array.size()
    ///
    /// Deletes an element from the the array, and then shifts all non-null elements coming after that deleted element by 1
    /// to the left. Returns the key-value that wer deleted
    public func deleteAndShiftValuesOver<T>(array : [var ?T], deleteIndex : Nat) : T {
      var deleted : T = switch (array[deleteIndex]) {
        case null {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.ArrayUtil.deleteAndShiftValuesOver, an invalid/incorrect delete index was passed")
        };
        case (?el) { el }
      };

      array[deleteIndex] := null;

      var i = deleteIndex + 1;
      label l loop {
        if (i >= array.size()) { break l };

        switch (array[i]) {
          case null { break l };
          case (?_) {
            array[i - 1] := array[i]
          }
        };

        i += 1
      };

      array[i - 1] := null;

      deleted
    };

    // replaces two successive elements in the array with a single element and shifts all other elements to the left by 1
    public func replaceTwoWithElementAndShift<T>(array : [var ?T], element : T, replaceIndex : Nat) {
      array[replaceIndex] := ?element;

      var i = replaceIndex + 1;
      let endShiftIndex : Nat = array.size() - 1;
      while (i < endShiftIndex) {
        switch (array[i]) {
          case (?_) { array[i] := array[i + 1] };
          case null { return }
        };

        i += 1
      };

      array[endShiftIndex] := null
    };

    /// BTree specific implementation
    ///
    /// In a single iteration insert at one position of the array while deleting at another position of the array, shifting all
    /// elements as appropriate
    ///
    /// This is used when borrowing a key from an inorder predecessor/successor through the parent node
    public func insertAtPostionAndDeleteAtPosition<T>(array : [var ?T], insertElement : ?T, insertIndex : Nat, deleteIndex : Nat) : T {
      var deleted : T = switch (array[deleteIndex]) {
        case null {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.ArrayUtil.insertAtPositionAndDeleteAtPosition, and incorrect delete index was passed")
        }; // indicated an incorrect delete index was passed - trap
        case (?el) { el }
      };

      // Example of this case:
      //
      //    Insert         Delete
      //      V              V
      //[var ?10, ?20, ?30, ?40, ?50]
      if (insertIndex < deleteIndex) {
        var i = deleteIndex;
        while (i > insertIndex) {
          array[i] := array[i - 1];
          i -= 1
        };

        array[insertIndex] := insertElement
      }
      // Example of this case:
      //
      //    Delete         Insert
      //      V              V
      //[var ?10, ?20, ?30, ?40, ?50]
      else if (insertIndex > deleteIndex) {
        array[deleteIndex] := null;
        var i = deleteIndex + 1;
        label l loop {
          if (i >= array.size()) { assert false; break l }; // TODO: remove? this should not happen since the insertIndex should get hit first?

          if (i == insertIndex) {
            array[i - 1] := array[i];
            array[i] := insertElement;
            break l
          } else {
            array[i - 1] := array[i]
          };

          i += 1
        };

      }
      // insertIndex == deleteIndex, can just do a swap
      else { array[deleteIndex] := insertElement };

      deleted
    };

    // which child the deletionIndex is referring to
    public type DeletionSide = { #left; #right };

    // merges a middle (parent) element with the left and right child arrays while deleting the element from the correct child by the deleteIndex passed
    public func mergeParentWithChildrenAndDelete<T>(
      parentKV : ?T,
      childCount : Nat,
      leftChild : [var ?T],
      rightChild : [var ?T],
      deleteIndex : Nat,
      deletionSide : DeletionSide
    ) : ([var ?T], T) {
      let mergedArray = VarArray.init<?T>(leftChild.size(), null);
      var i = 0;
      switch (deletionSide) {
        case (#left) {
          // BTree implementation expects the deleted element to exist - if null, traps
          let deletedElement = switch (leftChild[deleteIndex]) {
            case (?el) { el };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.ArrayUtil.mergeParentWithChildrenAndDelete, an invalid delete index was passed")
            }
          };

          // copy over left child until deleted element is hit, then copy all elements after the deleted element
          while (i < childCount) {
            if (i < deleteIndex) {
              mergedArray[i] := leftChild[i]
            } else {
              mergedArray[i] := leftChild[i + 1]
            };
            i += 1
          };

          // insert parent kv in the middle
          mergedArray[childCount - 1] := parentKV;

          // copy over the rest of the right child elements
          while (i < childCount * 2) {
            mergedArray[i] := rightChild[i - childCount];
            i += 1
          };

          (mergedArray, deletedElement)
        };
        case (#right) {
          // BTree implementation expects the deleted element to exist - if null, traps
          let deletedElement = switch (rightChild[deleteIndex]) {
            case (?el) { el };
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.ArrayUtil.mergeParentWithChildrenAndDelete: element at deleted index must exist")
            }
          };
          // since deletion side is #right, can safely copy over all elements from the left child
          while (i < childCount) {
            mergedArray[i] := leftChild[i];
            i += 1
          };

          // insert parent kv in the middle
          mergedArray[childCount] := parentKV;
          i += 1;

          var j = 0;
          // copy over right child until deleted element is hit, then copy elements after the deleted element
          while (i < childCount * 2) {
            if (j < deleteIndex) {
              mergedArray[i] := rightChild[j]
            } else {
              mergedArray[i] := rightChild[j + 1]
            };
            i += 1;
            j += 1
          };

          (mergedArray, deletedElement)
        }
      }
    };

  };

  module BinarySearch {
    public type SearchResult = {
      #keyFound : Nat;
      #notFound : Nat
    };

    /// Searches an array for a specific key, returning the index it occurs at if #keyFound, or the child/insert index it may occur at
    /// if #notFound. This is used when determining if a key exists in an internal or leaf node, where a key should be inserted in a
    /// leaf node, or which child of an internal node a key could be in.
    ///
    /// Note: This function expects a mutable, nullable, array of keys in sorted order, where all nulls appear at the end of the array.
    /// This function may trap if a null value appears before any values. It also expects a maxIndex, which is the right-most index (bound)
    /// from which to begin the binary search (the left most bound is expected to be 0)
    ///
    /// Parameters:
    ///
    /// * array - the sorted array that the binary search is performed upon
    /// * compare - the comparator used to perform the search
    /// * searchKey - the key being compared against in the search
    /// * maxIndex - the right-most index (bound) from which to begin the search
    public func binarySearchNode<K, V>(array : [var ?(K, V)], compare : (K, K) -> Order.Order, searchKey : K, maxIndex : Nat) : SearchResult {
      // TODO: get rid of this check?
      // Trap if array is size 0 (should not happen)
      if (array.size() == 0) {
        assert false
      };

      // if all elements in the array are null (i.e. first element is null), return #notFound(0)
      if (maxIndex == 0) {
        return #notFound(0)
      };

      // Initialize search from first to last index
      var left : Nat = 0;
      var right = maxIndex; // maxIndex does not necessarily mean array.size() - 1
      // Search the array
      while (left < right) {
        let middle = (left + right) / 2;
        switch (array[middle]) {
          case null { assert false };
          case (?(key, _)) {
            switch (compare(searchKey, key)) {
              // If the element is present at the middle itself
              case (#equal) { return #keyFound(middle) };
              // If element is greater than mid, it can only be present in left subarray
              case (#greater) { left := middle + 1 };
              // If element is smaller than mid, it can only be present in right subarray
              case (#less) {
                right := if (middle == 0) { 0 } else { middle - 1 }
              }
            }
          }
        }
      };

      if (left == array.size()) {
        return #notFound(left)
      };

      // left == right
      switch (array[left]) {
        // inserting at end of array
        case null { #notFound(left) };
        case (?(key, _)) {
          switch (compare(searchKey, key)) {
            // if left is the key
            case (#equal) { #keyFound(left) };
            // if the key is not found, return notFound and the insert location
            case (#greater) { #notFound(left + 1) };
            case (#less) { #notFound(left) }
          }
        }
      }
    }
  };

  module NodeUtil {
    /// Inserts element at the given index into a non-full leaf node
    public func insertAtIndexOfNonFullNodeData<K, V>(data : Data<K, V>, kvPair : ?(K, V), insertIndex : Nat) : () {
      let currentLastElementIndex : Nat = if (data.count == 0) { 0 } else {
        data.count - 1
      };
      ArrayUtil.insertAtPosition<(K, V)>(data.kvs, kvPair, insertIndex, currentLastElementIndex);

      // increment the count of data in this node since just inserted an element
      data.count += 1
    };

    /// Inserts two rebalanced (split) child halves into a non-full array of children.
    public func insertRebalancedChild<K, V>(children : [var ?Node<K, V>], rebalancedChildIndex : Nat, leftChildInsert : Node<K, V>, rightChildInsert : Node<K, V>) : () {
      // Note: BTree will always have an order >= 4, so this will never have negative Nat overflow
      var j : Nat = children.size() - 2;

      // This is just a sanity check to ensure the children aren't already full (should split promote otherwise)
      // TODO: Remove this check once confident
      if (Option.isSome(children[j + 1])) { assert false };

      // Iterate backwards over the array and shift each element over to the right by one until the rebalancedChildIndex is hit
      while (j > rebalancedChildIndex) {
        children[j + 1] := children[j];
        j -= 1
      };

      // Insert both the left and right rebalanced children (replacing the pre-split child)
      children[j] := ?leftChildInsert;
      children[j + 1] := ?rightChildInsert
    };

    /// Used when splitting the children of an internal node
    ///
    /// Takes in the rebalanced child index, as well as both halves of the rebalanced child and splits the children, inserting the left and right child halves appropriately
    ///
    /// For more context, see the documentation for the splitArrayAndInsertTwo method in ArrayUtils.mo
    public func splitChildrenInTwoWithRebalances<K, V>(
      children : [var ?Node<K, V>],
      rebalancedChildIndex : Nat,
      leftChildInsert : Node<K, V>,
      rightChildInsert : Node<K, V>
    ) : ([var ?Node<K, V>], [var ?Node<K, V>]) {
      ArrayUtil.splitArrayAndInsertTwo<Node<K, V>>(children, rebalancedChildIndex, leftChildInsert, rightChildInsert)
    };

    /// Helper used to get the key index of of a key within a node
    ///
    /// for more, see the BinarySearch.binarySearchNode() documentation
    public func getKeyIndex<K, V>(data : Data<K, V>, compare : (K, K) -> Order.Order, key : K) : BinarySearch.SearchResult {
      BinarySearch.binarySearchNode<K, V>(data.kvs, compare, key, data.count)
    };

    // calculates a BTree Node's minimum allowed keys given the order of the BTree
    public func minKeysFromOrder(order : Nat) : Nat {
      if (order % 2 == 0) { order / 2 - 1 } else { order / 2 }
    };

    // Given a node, get the maximum key value (right most leaf kv)
    public func getMaxKeyValue<K, V>(node : ?Node<K, V>) : (K, V) {
      switch (node) {
        case (?#leaf({ data })) {
          switch (data.kvs[data.count - 1]) {
            case null {
              Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.NodeUtil.getMaxKeyValue, data cannot have more elements than it's count")
            };
            case (?kv) { kv }
          }
        };
        case (?#internal({ data; children })) {
          getMaxKeyValue(children[data.count])
        };
        case null {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.NodeUtil.getMaxKeyValue, the node provided cannot be null")
        }
      }
    };

    type InorderBorrowType = {
      #predecessor;
      #successor
    };

    // attempts to retrieve the in max key of the child leaf node directly to the left if the node will allow it
    // returns the deleted max key if able to retrieve, null if not able
    //
    // mutates the predecessing node's keys
    public func borrowFromLeftLeafChild<K, V>(children : [var ?Node<K, V>], ofChildIndex : Nat) : ?(K, V) {
      let predecessorIndex : Nat = ofChildIndex - 1;
      borrowFromLeafChild(children, predecessorIndex, #predecessor)
    };

    // attempts to retrieve the in max key of the child leaf node directly to the right if the node will allow it
    // returns the deleted max key if able to retrieve, null if not able
    //
    // mutates the predecessing node's keys
    public func borrowFromRightLeafChild<K, V>(children : [var ?Node<K, V>], ofChildIndex : Nat) : ?(K, V) {
      borrowFromLeafChild(children, ofChildIndex + 1, #successor)
    };

    func borrowFromLeafChild<K, V>(children : [var ?Node<K, V>], borrowChildIndex : Nat, childSide : InorderBorrowType) : ?(K, V) {
      let minKeys = minKeysFromOrder(children.size());

      switch (children[borrowChildIndex]) {
        case (?#leaf({ data })) {
          if (data.count > minKeys) {
            // able to borrow a key-value from this child, so decrement the count of kvs
            data.count -= 1; // Since enforce order >= 4, there will always be at least 1 element per node
            switch (childSide) {
              case (#predecessor) {
                let deletedKV = data.kvs[data.count];
                data.kvs[data.count] := null;
                deletedKV
              };
              case (#successor) {
                ?ArrayUtil.deleteAndShiftValuesOver(data.kvs, 0)
              }
            }
          } else { null }
        };
        case _ {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.NodeUtil.borrowFromLeafChild, the node at the borrow child index cannot be null or internal")
        }
      }
    };

    type InternalBorrowResult<K, V> = {
      #borrowed : InternalBorrow<K, V>;
      #notEnoughKeys : Internal<K, V>
    };

    type InternalBorrow<K, V> = {
      deletedSiblingKVPair : ?(K, V);
      child : ?Node<K, V>
    };

    // Attempts to borrow a KV and child from an internal sibling node
    public func borrowFromInternalSibling<K, V>(children : [var ?Node<K, V>], borrowChildIndex : Nat, borrowType : InorderBorrowType) : InternalBorrowResult<K, V> {
      let minKeys = minKeysFromOrder(children.size());

      switch (children[borrowChildIndex]) {
        case (?#internal({ data; children })) {
          if (data.count > minKeys) {
            data.count -= 1;
            switch (borrowType) {
              case (#predecessor) {
                let deletedSiblingKVPair = data.kvs[data.count];
                data.kvs[data.count] := null;
                let child = children[data.count + 1];
                children[data.count + 1] := null;
                #borrowed({
                  deletedSiblingKVPair;
                  child
                })
              };
              case (#successor) {
                #borrowed({
                  deletedSiblingKVPair = ?ArrayUtil.deleteAndShiftValuesOver(data.kvs, 0);
                  child = ?ArrayUtil.deleteAndShiftValuesOver(children, 0)
                })
              }
            }
          } else { #notEnoughKeys({ data; children }) }
        };
        case _ {
          Runtime.trap("UNREACHABLE_ERROR: file a bug report! In Map.NodeUtil.borrowFromInternalSibling from internal sibling, the child at the borrow index cannot be null or a leaf")
        }
      }
    };

    type SiblingSide = { #left; #right };

    // Rotates the borrowed KV and child from sibling side of the internal node to the internal child recipient
    public func rotateBorrowedKVsAndChildFromSibling<K, V>(
      internalNode : Internal<K, V>,
      parentRotateIndex : Nat,
      borrowedSiblingKVPair : ?(K, V),
      borrowedSiblingChild : ?Node<K, V>,
      internalChildRecipient : Internal<K, V>,
      siblingSide : SiblingSide
    ) {
      // if borrowing from the left, the rotated key and child will always be inserted first
      // if borrowing from the right, the rotated key and child will always be inserted last
      let (kvIndex, childIndex) = switch (siblingSide) {
        case (#left) { (0, 0) };
        case (#right) {
          (internalChildRecipient.data.count, internalChildRecipient.data.count + 1)
        }
      };

      // get the parent kv that will be pushed down the the child
      let kvPairToBePushedToChild = internalNode.data.kvs[parentRotateIndex];
      // replace the parent with the sibling kv
      internalNode.data.kvs[parentRotateIndex] := borrowedSiblingKVPair;
      // push the kv and child down into the internalChild
      insertAtIndexOfNonFullNodeData<K, V>(internalChildRecipient.data, kvPairToBePushedToChild, kvIndex);

      ArrayUtil.insertAtPosition<Node<K, V>>(internalChildRecipient.children, borrowedSiblingChild, childIndex, internalChildRecipient.data.count)
    };

    // Merges the kvs and children of two internal nodes, pushing the parent kv in between the right and left halves
    public func mergeChildrenAndPushDownParent<K, V>(leftChild : Internal<K, V>, parentKV : ?(K, V), rightChild : Internal<K, V>) : Internal<K, V> {
      {
        data = mergeData<K, V>(leftChild.data, parentKV, rightChild.data);
        children = mergeChildren(leftChild.children, rightChild.children)
      }
    };

    func mergeData<K, V>(leftData : Data<K, V>, parentKV : ?(K, V), rightData : Data<K, V>) : Data<K, V> {
      assert leftData.count <= minKeysFromOrder(leftData.kvs.size() + 1);
      assert rightData.count <= minKeysFromOrder(rightData.kvs.size() + 1);

      let mergedKVs = VarArray.init<?(K, V)>(leftData.kvs.size(), null);
      var i = 0;
      while (i < leftData.count) {
        mergedKVs[i] := leftData.kvs[i];
        i += 1
      };

      mergedKVs[i] := parentKV;
      i += 1;

      var j = 0;
      while (j < rightData.count) {
        mergedKVs[i] := rightData.kvs[j];
        i += 1;
        j += 1
      };

      {
        kvs = mergedKVs;
        var count = leftData.count + 1 + rightData.count
      }
    };

    func mergeChildren<K, V>(leftChildren : [var ?Node<K, V>], rightChildren : [var ?Node<K, V>]) : [var ?Node<K, V>] {
      let mergedChildren = VarArray.init<?Node<K, V>>(leftChildren.size(), null);
      var i = 0;

      while (Option.isSome(leftChildren[i])) {
        mergedChildren[i] := leftChildren[i];
        i += 1
      };

      var j = 0;
      while (Option.isSome(rightChildren[j])) {
        mergedChildren[i] := rightChildren[j];
        i += 1;
        j += 1
      };

      mergedChildren
    }
  }
}
