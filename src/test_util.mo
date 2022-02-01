import Nat8 "mo:base/Nat8";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import C "../src/curve";

module {
  public func toHex(x : Nat) : Text {
    if (x == 0) return "0";
    var ret = "";
    var t = x;
    while (t > 0) {
      ret := (switch (t % 16) {
        case 0 { "0" };
        case 1 { "1" };
        case 2 { "2" };
        case 3 { "3" };
        case 4 { "4" };
        case 5 { "5" };
        case 6 { "6" };
        case 7 { "7" };
        case 8 { "8" };
        case 9 { "9" };
        case 10 { "a" };
        case 11 { "b" };
        case 12 { "c" };
        case 13 { "d" };
        case 14 { "e" };
        case 15 { "f" };
        case _ { "*" };
      }) # ret;
      t /= 16;
    };
    ret
  };
  public func dump(iter : Iter.Iter<Nat8>) {
    var text = "";
    for (e in iter) {
      let s = toHex(Nat8.toNat(e));
      text := text # (if (s.size() == 1) "0" else "") # s;
    };
    Debug.print(text);
  };
  public func putPoint(a : C.Point) {
    switch (a) {
      case(#zero) {
        Debug.print("0");
      };
      case(#affine(x, y)) {
        Debug.print("(" # toHex(C.Fp.toNat(x)) # ", " # toHex(C.Fp.toNat(y)) # ")");
      };
    };
  };
}
