import Buffer "mo:base/Buffer";

module {
  // 13 = 0b1101 => [true,false,true,true]
  public func fromNatReversed(x : Nat) : [Bool] {
    var buf = Buffer.Buffer<Bool>(256);
    var t = x;
    while (t > 0) {
      buf.add((t % 2) == 1);
      t /= 2;
    };
    buf.toArray()
  };
}
