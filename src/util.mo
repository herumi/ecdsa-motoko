import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";

module {
  // [0x12, 0x34] : [Nat] => 0x1234
  public func toNatAsBigEndian(iter : Iter.Iter<Nat8>) : Nat {
    var v = 0;
    label l loop {
      switch (iter.next()) {
        case (null) break l;
        case (?e) v := v * 256 + Nat8.toNat(e);
      };
    };
    return v
  };
  /// 0x1234 => [0x12, 0x34], 0 => [0]
  public func toBigEndian(x : Nat) : [Nat8] {
    if (x == 0) return [0];
    var buf = Buffer.Buffer<Nat8>(64);
    var t = x;
    while (t > 0) {
      buf.add(Nat8.fromNat(t % 256));
      t /= 256;
    };
    let n = buf.size();
    let ith = func(i : Nat) : Nat8 {
      buf.get(n - 1 - i)
    };
    Array.tabulate<Nat8>(n, ith)
  };
  /// (5, 0x1234) => [0x00, 0x00, 0x00, 0x12, 0x34]
  public func toBigEndianPad(len : Nat, x : Nat) : [Nat8] {
    var buf = Buffer.Buffer<Nat8>(len);
    var t = x;
    var i = 0;
    while (i < len) {
      buf.add(Nat8.fromNat(t % 256));
      t /= 256;
      i += 1;
    };
    let ith = func(i : Nat) : Nat8 {
      buf.get(len - 1 - i)
    };
    Array.tabulate<Nat8>(len, ith)
  };
  /// return subarray a[begin:end)
  public func subArray(a : [Nat8], begin : Nat, size : Nat) : [Nat8] {
    let ith = func(i : Nat) : Nat8 = a[begin + i];
    Array.tabulate<Nat8>(size, ith)
  };
}
