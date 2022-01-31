import Field "field";
import Util "curve_util";

module {
  public type FpElt = { #fp : Nat; };
  public type FrElt = { #fr : Nat; };
  public type Affine = (FpElt, FpElt);
  public type Point = { #zero; #affine : Affine };

  public let params = {
    p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
    r = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
    a = #fp(0);
    b = #fp(7);
    g = (#fp(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798), #fp(0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8));
	  // rHalf_ = (r_ + 1) / 2;
	  rHalf = 0x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a1;
  };	

  let G_ = #affine(params.g);

  let p_ = params.p;
  let r_ = params.r;
  let a_ = params.a;
  let b_ = params.b;
  // pSqrRoot_ = (p_ + 1) / 4;
  let pSqrRoot_ : Nat = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffbfffff0c;

  public let Fp = {
    fromNat = func (n : Nat) : FpElt = #fp(n % p_);
    toNat = func (#fp(x) : FpElt) : Nat = x;
    add = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.add_(x, y, p_));
    mul = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.mul_(x, y, p_));
    sub = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.sub_(x, y, p_));
    div = func(#fp(x) : FpElt, #fp(y) : FpElt) : FpElt = #fp(Field.div_(x, y, p_));
    pow = func(#fp(x) : FpElt, n : Nat) : FpElt = #fp(Field.pow_(x, n, p_));
    neg = func(#fp(x) : FpElt) : FpElt = #fp(Field.neg_(x, p_));
    inv = func(#fp(x) : FpElt) : FpElt = #fp(Field.inv_(x, p_));
    sqr = func(#fp(x) : FpElt) : FpElt = #fp(Field.sqr_(x, p_));
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
  public func isValid((x,y) : Affine) : Bool = Fp.sqr(y) == getYsqrFromX(x);
  public func isZero(a : Point) : Bool = a == #zero;
  public func isNegOf(a : Point, b : Point) : Bool = a == neg(b);
  public func neg(p : Point) : Point = switch (p) {
    case (#zero) #zero;
    case (#affine(c)) #affine(c.0, Fp.neg(c.1));
  };
  func dbl_affine((x,y) : Affine) : Affine {
    let xx = Fp.mul(x,x);
    let xx3 = Fp.add(Fp.add(xx, xx), xx);
    let nume = Fp.add(xx3, a_);
    let deno = Fp.add(y,y);
    let L = Fp.div(nume, deno);
    let x3 = Fp.sub(Fp.mul(L, L), Fp.add(x,x));
    let y3 = Fp.sub(Fp.mul(L, Fp.sub(x, x3)), y);
    (x3, y3)
  };
  public func dbl(a : Point) : Point = switch (a) {
    case (#zero) #zero;
    case (#affine(c)) #affine(dbl_affine(c));
  };
  public func add(a : Point, b : Point) : Point = switch (a, b) {
    case (#zero, b) return b;
    case (a, #zero) return a;
    case (#affine(ax,ay), #affine(bx,by)) {
      if (ax == bx) {
        // P + (-P) or P + P 
        return if (ay == Fp.neg(by)) #zero else dbl(a);
      } else {
        let L = Fp.div(Fp.sub(ay, by), Fp.sub(ax, bx));
        let x3 = Fp.sub(Fp.mul(L, L), Fp.add(ax, bx));
        let y3 = Fp.sub(Fp.mul(L, Fp.sub(ax, x3)), ay);
        return #affine(x3, y3);
      };
    };
  };
  public func mul(a : Point, #fr(x) : FrElt) : Point {
    let bs = Util.toReverseBin(x);
    let n = bs.size();
    var ret : Point = #zero;
    var i = 0;
    while (i < n) {
      let b = bs[n - 1 - i];
      ret := dbl(ret);
      if (b) ret := add(ret, a);
      i += 1;
    };
    ret
  };
  public func mul_base(e : FrElt) : Point = mul(G_, e);
}