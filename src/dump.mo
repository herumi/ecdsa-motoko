import Nat8 "mo:base/Nat8";
import Iter "mo:base/Iter";
import Debug "mo:base/Debug";
import Hex "hex";

module {
  public func dump(iter : Iter.Iter<Nat8>) {
    var text = "";
    for (e in iter) {
      let s = Hex.fromNat(Nat8.toNat(e));
      text := text # (if (s.size() == 1) "0" else "") # s;
    };
    Debug.print(text);
  };
}
