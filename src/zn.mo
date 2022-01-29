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
  public func inv_(x : Nat, n : Nat) : Nat {
    let (gcd, rev, _) = IntExt.extGcd(x, n);
    assert(gcd == 1);
    let v = if (rev < 0) rev+n else rev;
    assert(0 <= v and v < n);
    Int.abs(v)
  };
  public func add_(x : Nat, y : Nat, n : Nat) : Nat {
    let z = x+y;
    if (z < n) z else z-n;
  };
  public func mul_(x : Nat, y : Nat, n : Nat) : Nat = (x * y) % n;
  public func sub_(x : Nat, y : Nat, n : Nat) : Nat = if (x >= y) x-y else x+n-y;
  public func div_(x : Nat, y : Nat, n : Nat) : Nat = (x * inv_(y, n)) % n;
  public func neg_(x : Nat, n : Nat) : Nat = if (x == 0) 0 else n - x;
  public func pow_(x : Nat, y : Nat, n : Nat) : Nat {
    if (y == 0) return 1;
    let bs = toReverseBin(y);
    let len = bs.size();
    var ret = 1;
    var i = 0;
    while (i < len) {
      let b = bs[len - 1 - i];
      ret := mul_(ret, ret, n);
      if (b) ret := mul_(ret, x, n);
      i += 1;
    };
    ret
  };
  public func sqr_(x : Nat, n : Nat) : Nat = mul_(x, x, n);

  public class Zn(n : Nat) {
    public func add(x : Nat, y : Nat) : Nat = add_(x, y, n);
    public func mul(x : Nat, y : Nat) : Nat = mul_(x, y, n);
    public func sub(x : Nat, y : Nat) : Nat = sub_(x, y, n);
    public func div(x : Nat, y : Nat) : Nat = div_(x, y, n);
    public func pow(x : Nat, y : Nat) : Nat = pow_(x, y, n);
    public func neg(x : Nat) : Nat = neg_(x, n);
    public func inv(x : Nat) : Nat = inv_(x, n);
    public func sqr(x : Nat) : Nat = sqr_(x, n);
  };
}