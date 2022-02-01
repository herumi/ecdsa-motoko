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
}