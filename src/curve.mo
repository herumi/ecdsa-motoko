import Field "field";
import Hex "hex";
import Binary "binary";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Buffer "mo:base/Buffer";
import Int "mo:base/Int";
import F "fp";

module {
//  public type FpElt = { #fp : F.F; };
  public type FpElt = F.F;
  public type FrElt = { #fr : Nat; };
  public type Affine = (FpElt, FpElt);
  public type Point = { #zero; #affine : Affine };

  public let params = {
    p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
    r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
    a = F.zero;
    b = (7,0,0,0,0,0,0,0) : F.F;
//  g = (#fp(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798), #fp(0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8));
    gx : F.F = (0x16f81798, 0x59f2815b, 0x2dce28d9, 0x029bfcdb, 0xce870b07, 0x55a06295, 0xf9dcbbac, 0x79be667e);
    gy : F.F = (0xfb10d4b8, 0x9c47d08f, 0xa6855419, 0xfd17b448, 0x0e1108a8, 0x5da4fbfc, 0x26a3c465, 0x483ada77);
	// rHalf_ = (r_ + 1) / 2;
	 rHalf = 0x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a1;
    //rHalf : F.F = (0x681b20a1, 0xdfe92f46, 0x57a4501d, 0x5d576e73, 0xffffffff, 0xffffffff, 0xffffffff, 0x7fffffff);

      // for GLV
      B00 : Int = 0x3086d221a7d46bcde86c90e49284eb15;
      B01 : Int = -0xe4437ed6010e88286f547fa90abfe4c3;
      B10 : Int = 0x114ca50f7a8e2f3f657c1108d9d44cfd8;
//      rw = #fp(55594575648329892869085402983802832744385952214688224221778511981742606582254);
      rw : F.F = (0x719501ee, 0xc1396c28, 0x12f58995, 0x9cf04975, 0xac3434e9, 0x6e64479e, 0x657c0710, 0x7ae96a2b);
      SHIFT256 : Int = 0x10000000000000000000000000000000000000000000000000000000000000000;
      v0 : Int = 64502973549206556628585045361533709077;
      v1 : Int = 303414439467246543595250775667605759172;

  };

  let p_ = params.p;
  let r_ = params.r;
  let a_ = params.a;
  let b_ = params.b;
  // pSqrRoot_ = (p_ + 1) / 4;
  let pSqrRoot_ : Nat = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffbfffff0c;

  public let Fp = {
    fromNat = func (n : Nat) : FpElt = F.fromNat(n % p_);
    toNat = func (x : FpElt) : Nat = F.toNat(x);
//    add = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.add_(x, y, p_));
//    sub = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.sub_(x, y, p_));
//    mul = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.mul_(x, y, p_));
//    sqr = func(#fp(x) : FpElt) : FpElt = #fp(Field.sqr_(x, p_));
//    neg = func(#fp(x) : FpElt) : FpElt = #fp(Field.neg_(x, p_));
//    pow = func(#fp(x) : FpElt, n : Nat) : FpElt = #fp(Field.pow_(x, n, p_));
    add = func(x : FpElt, y : FpElt) : FpElt = F.add(x, y);
    sub = func(x : FpElt, y : FpElt) : FpElt = F.sub(x, y);
    mul = func(x : FpElt, y : FpElt) : FpElt = F.mul(x, y);
    sqr = func(x : FpElt) : FpElt = F.mul(x, x);
    neg = func(x : FpElt) : FpElt = F.neg(x);
    pow = func(x : FpElt, y : Nat) : FpElt {
      if (y == 0) return F.one;
      let bs = Binary.fromNatReversed(y);
      let len = bs.size();
      var ret = F.one;
      var i = 0;
      while (i < len) {
        let b = bs[len - 1 - i];
        ret := F.mul(ret, ret);
        if (b) ret := F.mul(ret, x);
        i += 1;
      };
      ret
    };
/*
    mul = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt {
      let xx = F.toNat(F.mul(F.fromNat(x), F.fromNat(y)));
      let yy = Field.mul_(x, y, p_);
      if (xx != yy) {
        Debug.print("x=" # Nat.toText(x));
        Debug.print("y=" # Nat.toText(y));
        Debug.print("ok=" # Nat.toText(yy));
        Debug.print("ng=" # Nat.toText(xx));
      };
      #fp(xx)
    };
*/
    inv = func(x : FpElt) : FpElt = F.fromNat(Field.inv_(F.toNat(x), p_));
    div = func(x : FpElt, y : FpElt) : FpElt = F.fromNat(Field.div_(F.toNat(x), F.toNat(y), p_));
  };
  public let Fr = {
    fromNat = func (n : Nat) : FrElt = #fr(n % r_);
    toNat = func (#fr(x) : FrElt) : Nat = x;
    add = func(#fr(x) : FrElt, #fr(y) : FrElt) : FrElt = #fr(Field.add_(x, y, r_));
    mul = func(#fr(x) : FrElt, #fr(y) : FrElt) : FrElt = #fr(Field.mul_(x, y, r_));
    sub = func(#fr(x) : FrElt, #fr(y) : FrElt) : FrElt = #fr(Field.sub_(x, y, r_));
    div = func(#fr(x) : FrElt, #fr(y) : FrElt) : FrElt = #fr(Field.div_(x, y, r_));
    pow = func(#fr(x) : FrElt, n : Nat) : FrElt = #fr(Field.pow_(x, n, r_));
    neg = func(#fr(x) : FrElt) : FrElt = #fr(Field.neg_(x, r_));
    inv = func(#fr(x) : FrElt) : FrElt = #fr(Field.inv_(x, r_));
    sqr = func(#fr(x) : FrElt) : FrElt = #fr(Field.sqr_(x, r_));
  };

  // public only for testing
  public func fpSqrRoot(x : FpElt) : ?FpElt {
    let sq = Fp.pow(x, pSqrRoot_);
    if (Fp.sqr(sq) == x) ?sq else null
  };

  // return x^3 + ax + b
  func getYsqrFromX(x : FpElt) : FpElt =
    Fp.add(Fp.mul(Fp.add(Fp.sqr(x), a_), x), b_);

  /// Get y corresponding to x such that y^2 = x^ + ax + b.
  /// Return even y if `even` is true.
  public func getYfromX(x : FpElt, even : Bool) : ?FpElt {
    let y2 = getYsqrFromX(x);
    switch (fpSqrRoot(y2)) {
      case (null) null;
      case (?y) if (even == ((Fp.toNat(y) % 2) == 0)) ?y else ?Fp.neg(y);
    }
  };

  // point functions
  public func isValidAffine((x,y) : Affine) : Bool = Fp.sqr(y) == getYsqrFromX(x);
  public type Jacobi = (FpElt, FpElt, FpElt);
  public let zeroJ = (F.zero, F.zero, F.zero);
  public let G_ = (params.gx, params.gy, F.one);
  public func isZero((_, _, z) : Jacobi) : Bool = z == F.zero;
  public func toJacobi(a : Point) : Jacobi = switch (a) {
    case (#zero) zeroJ;
    case (#affine(x, y)) (x, y, F.one);
  };
  public func normalize((x, y, z) : Jacobi) : Jacobi {
    if (z == F.zero) return (x, y, z);
    let rz = Fp.inv(z);
    let rz2 = Fp.sqr(rz);
    (Fp.mul(x, rz2), Fp.mul(Fp.mul(y, rz2), rz), F.one)
  };
  public func fromJacobi(a : Jacobi) : Point {
    let (x, y, z) = normalize(a);
    if (z == F.zero) return #zero;
    #affine(x, y)
  };
  // y^2 == x(x^2 + a z^4) + b z^6
  public func isValid((x, y, z) : Jacobi) : Bool {
    let x2 = Fp.sqr(x);
    let y2 = Fp.sqr(y);
    let z2 = Fp.sqr(z);
    var z4 = Fp.sqr(z2);
    var t = Fp.mul(z4, a_);
    t := Fp.add(t, x2);
    t := Fp.mul(t, x);
    z4 := Fp.mul(z4, z2);
    z4 := Fp.mul(z4, b_);
    t := Fp.add(t, z4);
    y2 == t
  };
  public func isEqual(P1 : Jacobi, P2 : Jacobi) : Bool {
    let zero1 = isZero(P1);
    let zero2 = isZero(P2);
    if (zero1) return zero2;
    if (zero2) return false;
    let (x1, y1, z1) = P1;
    let (x2, y2, z2) = P2;
    let s1 = Fp.sqr(z1);
    let s2 = Fp.sqr(z2);
    var t1 = Fp.mul(x1, s2);
    var t2 = Fp.mul(x2, s1);
    if (t1 != t2) return false;
    t1 := Fp.mul(y1, s2);
    t2 := Fp.mul(y2, s1);
    t1 := Fp.mul(t1, z2);
    t2 := Fp.mul(t2, z1);
    t1 == t2
  };
  public func neg((x, y, z) : Jacobi) : Jacobi = (x, Fp.neg(y), z);
  public func dbl((x, y, z) : Jacobi) : Jacobi {
    if (z == F.zero) return zeroJ;
    var x2 = Fp.sqr(x);
    var y2 = Fp.sqr(y);
    var xy = Fp.mul(x, y2);
    xy := Fp.add(xy, xy);
    y2 := Fp.sqr(y2);
    xy := Fp.add(xy, xy);
    // assume a_ == 0
    var t = Fp.add(x2, x2);
    x2 := Fp.add(x2, t);
    var rx = Fp.sqr(x2);
    rx := Fp.sub(rx, xy);
    rx := Fp.sub(rx, xy);
    var rz : FpElt = if (z == F.one) y else Fp.mul(y, z);
    rz := Fp.add(rz, rz);
    var ry = Fp.sub(xy, rx);
    ry := Fp.mul(ry, x2);
    y2 := Fp.add(y2, y2);
    y2 := Fp.add(y2, y2);
    y2 := Fp.add(y2, y2);
    ry := Fp.sub(ry, y2);
    (rx, ry, rz)
  };
  public func add((px, py, pz) : Jacobi, (qx, qy, qz) : Jacobi) : Jacobi {
    if (pz == F.zero) return (qx, qy, qz);
    if (qz == F.zero) return (px, py, pz);
    let isPzOne = pz == F.one;
    let isQzOne = qz == F.one;
    var r = if (isPzOne) F.one else Fp.sqr(pz);
    var U1 = F.zero;
    var S1 = F.zero;
    var H = F.zero;
    if (isQzOne) {
      U1 := px;
      if (isPzOne) {
        H := qx;
      } else {
        H := Fp.mul(qx, r);
      };
      H := Fp.sub(H, U1);
      S1 := py;
    } else {
      S1 := Fp.sqr(qz);
      U1 := Fp.mul(px, S1);
      if (isPzOne) {
        H := qx;
      } else {
        H := Fp.mul(qx, r);
      };
      H := Fp.sub(H, U1);
      S1 := Fp.mul(S1, qz);
      S1 := Fp.mul(S1, py);
    };
    if (isPzOne) {
      r := qy;
    } else {
      r := Fp.mul(r, pz);
      r := Fp.mul(r, qy);
    };
    r := Fp.sub(r, S1);
    if (H == F.zero) {
      if (r == F.zero) {
        return dbl((px, py, pz));
      } else {
        return zeroJ;
      };
    };
    var rx = F.zero;
    var ry = F.zero;
    var rz = F.zero;
    if (isPzOne) {
      if (isQzOne) {
        rz := H;
      } else {
        rz := Fp.mul(H, qz);
      };
    } else {
      if (isQzOne) {
        rz := Fp.mul(pz, H);
      } else {
        rz := Fp.mul(pz, qz);
        rz := Fp.mul(rz, H);
      };
    };
    var H3 = Fp.sqr(H);
    ry := Fp.sqr(r);
    U1 := Fp.mul(U1, H3);
    H3 := Fp.mul(H3, H);
    ry := Fp.sub(ry, U1);
    ry := Fp.sub(ry, U1);
    rx := Fp.sub(ry, H3);
    U1 := Fp.sub(U1, rx);
    U1 := Fp.mul(U1, r);
    H3 := Fp.mul(H3, S1);
    ry := Fp.sub(U1, H3);
    (rx, ry, rz)
  };
  public func sub((px, py, pz) : Jacobi, (qx, qy, qz) : Jacobi) : Jacobi = add((px, py, pz), (qx, Fp.neg(qy), qz));
  public func mul_old(a : Jacobi, #fr(x) : FrElt) : Jacobi {
    let bs = Binary.fromNatReversed(x);
    let n = bs.size();
    var ret = zeroJ;
    var i = 0;
    while (i < n) {
      let b = bs[n - 1 - i];
      ret := dbl(ret);
      if (b) ret := add(ret, a);
      i += 1;
    };
    ret
  };

  func mulLambda((x, y, z) : Jacobi) : Jacobi = (Fp.mul(x, params.rw), y, z);
  func split(x_ : Nat) : (Int,Int) {
    let x = x_ : Int;
    let t = (x * params.v0) / params.SHIFT256;
    var b = (x * params.v1) / params.SHIFT256;
    let a = x - (t * params.B00 + b * params.B10);
    b := -(t * params.B01 + b * params.B00);
    (a, b)
  };
  // splitN = 2, w = 5
  public func mul(x : Jacobi, #fr(y) : FrElt) : Jacobi {
    let w = 5;
    let tblSize : Nat = 2 ** (w - 2);
    let u = split(y);
    let naf0 = Binary.toNafWidth(u.0, w);
    let naf1 = Binary.toNafWidth(u.1, w);
    let maxBit = Nat.max(naf0.size(), naf1.size());
    var tbl0 = Buffer.Buffer<Jacobi>(tblSize);
    var tbl1 = Buffer.Buffer<Jacobi>(tblSize);
    tbl0.add(x);
    tbl1.add(mulLambda(x));
    do {
      let P2 = dbl(x);
      var j = 1;
      while (j < tblSize) {
        tbl0.add(add(tbl0.get(j - 1), P2));
        tbl1.add(mulLambda(tbl0.get(j)));
        j += 1;
      };
    };
    var z = zeroJ;
    let addTbl = func(tbl : Buffer.Buffer<Jacobi>, naf : [Int], i : Nat) {
      if (i >= naf.size()) return;
      let n = naf[i];
      if (n > 0) {
        let idx = Int.abs(n - 1) / 2;
        z := add(z, tbl.get(idx));
      } else if (n < 0) {
        let idx = Int.abs(-n - 1) / 2;
        z := add(z, neg(tbl.get(idx)));
      };
    };
    do {
      var i = 0;
      while (i < maxBit) {
        let bit = maxBit - 1 - i : Nat;
        z := dbl(z);
        addTbl(tbl0, naf0, bit);
        addTbl(tbl1, naf1, bit);
        i += 1;
      };
    };
    z;
  };

  public func mul_base(x : FrElt) : Jacobi = mul(G_, x);
  public func putPoint(a : Point) {
    switch (a) {
      case(#zero) {
        Debug.print("0");
      };
      case(#affine(x, y)) {
        Debug.print("(" # Hex.fromNat(Fp.toNat(x)) # ", " # Hex.fromNat(Fp.toNat(y)) # ")");
      };
    };
  };
  public func putJacobi((x, y, z) : Jacobi) {
    Debug.print("(" # Hex.fromNat(Fp.toNat(x)) # ",");
    Debug.print(" " # Hex.fromNat(Fp.toNat(y)) # ",");
    Debug.print(" " # Hex.fromNat(Fp.toNat(z)) # ")");
  };
}
