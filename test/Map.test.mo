// @testmode wasi

import Suite "mo:matchers/Suite";
import T "mo:matchers/Testable";
import M "mo:matchers/Matchers";

let { run; test; suite } = Suite;

run(
  suite(
    "empty",
    [
      test(
        "dummy",
        0,
        M.equals(T.nat(0))
      )
    ]
  )
);
