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
  // getNAF
  public func toNafWidth(x_ : Int, w : Int) : [Int] {
    var naf = Buffer.Buffer<Int>(256);
    let w = 5;
    let signedMaxW : Int = 2 ** (w - 1);
    let maxW = signedMaxW * 2;
    let negative = x_ < 0;
    var x = if (x_ < 0) -x_ else x_;
    var zeroNum = 0;
    while (x > 0) {
      while ((x % 2) == 0) {
        x /= 2;
        zeroNum += 1;
      };
      do {
        var i = 0;
        while (i < zeroNum) {
          naf.add(0);
          i += 1;
        };
      };
      var v = x % maxW;
      x /= maxW;
      if (v >= signedMaxW) {
        x += 1;
        v -= maxW;
      };
      naf.add(v);
      zeroNum := w - 1;
    };
    if (negative) {
      var i = 0;
      while (i < naf.size()) {
        naf.put(i, -naf.get(i));
        i += 1;
      };
    };
    naf.toArray()
  };
}
