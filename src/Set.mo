/// Original: `OrderedSet.mo`

import Order "Order";
import { todo } "Debug";

module {
  type Args<T> = {
    compare : (T, T) -> Order.Order
  };

  public class Set<T>(handle: Args<T>) {

    public func contains(key : T) : Bool = todo();

    // TODO...
  }
}
