import Int "mo:base/Int";
import IntExt "intext";
import Buffer "mo:base/Buffer";

module {
  // 13 = 0b1101 => [true,false,true,ture]
  public func toReverseBin(x : Nat) : [Bool] {
    var buf = Buffer.Buffer<Bool>(256);
    var t = x;
    while (t > 0) {
      buf.add((t % 2) == 1);
      t /= 2;
    };
    buf.toArray()
  };
  public func inv(x : Nat, n : Nat) : Nat {
    let (gcd, rev, _) = IntExt.extGcd(x, n);
    assert(gcd == 1);
    let v = if (rev < 0) rev+n else rev;
    assert(0 <= v and v < n);
    Int.abs(v)
  };
  public func add(x : Nat, y : Nat, n : Nat) : Nat {
    let z = x+y;
    if (z < n) z else z-n;
  };
  public func mul(x : Nat, y : Nat, n : Nat) : Nat = (x * y) % n;
  public func sub(x : Nat, y : Nat, n : Nat) : Nat = if (x >= y) x-y else x+n-y;
  public func div(x : Nat, y : Nat, n : Nat) : Nat = (x * inv(y, n)) % n;
  public func neg(x : Nat, n : Nat) : Nat = if (x == 0) 0 else n - x;
  public func pow(x : Nat, y : Nat, n : Nat) : Nat {
    if (y == 0) return 1;
    let bs = toReverseBin(y);
    let len = bs.size();
    var ret = 1;
    var i = 0;
    while (i < len) {
      let b = bs[len - 1 - i];
      ret := mul(ret, ret, n);
      if (b) ret := mul(ret, x, n);
      i += 1;
    };
    ret
  };
  public func sqr(x : Nat, n : Nat) : Nat = mul(x, x, n);
}