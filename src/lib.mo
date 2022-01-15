/**
 * Module      : fp.mo
 * Description : finite field
 * Copyright   : 2022 Mitsunari Shigeo
 * License     : Apache 2.0 with LLVM Exception
 * Maintainer  : herumi <herumi@nifty.com>
 * Stability   : Stable
 */

import Int "mo:base/Int";
import Nat8 "mo:base/Nat8";
import B "mo:base/Buffer";

module {
  // secp256k1
  // Ec/Fp : y^2 = x^3 + ax + b
  // (gx, gy) in Ec
  // #Ec = r
  public let p_ : Nat = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
  public let a_ : Nat = 0;
  public let b_ : Nat = 7;
  public let r_ : Nat = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
  public let gx_ : Nat = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798;
  public let gy_ : Nat = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8;

  /// return the order of the field where Ec is defined.
  public func p() : Nat { p_ };
  /// return the order of the generator of Ec.
  public func r() : Nat { r_ };

  // 13 = 0b1101 => [true,false,true,ture]
  public func fromNatToReverseBin(x : Nat) : [Bool] {
    var ret : B.Buffer<Bool> = B.Buffer<Bool>(256);
    var t = x;
    while (t > 0) {
      ret.add((t % 2) == 1);
      t := t / 2;
    };
    ret.toArray()
  };

  // return (gcd, x, y) such that gcd = a * x + b * y
  public func extGcd(a : Int, b : Int) : (Int, Int, Int) {
    if (a == 0) return (b, 0, 1);
    let q = b / a;
    let r = b % a;
    let (gcd, x, y) = extGcd(r, a);
    (gcd, y - q * x, x)
  };
  public func addMod(x : Nat, y : Nat, p : Nat) : Nat {
    let v = x + y;
    if (v < p) return v;
    v - p
  };
  public func subMod(x : Nat, y : Nat, p : Nat) : Nat {
    if (x >= y) return x - y;
    x + p - y
  };
  public func mulMod(x : Nat, y : Nat, p : Nat) : Nat {
    (x * y) % p
  };
  // return rev such that x * rev mod p = 1 if success else 0
  public func invMod(x : Nat, p : Nat) : Nat {
    let (gcd, rev, dummy) = extGcd(x, p);
    assert(gcd == 1);
    var v = rev;
    if (rev < 0) v := rev + p;
    assert(0 <= v and v < p);
    Int.abs(v)
  };
  public func divMod(x : Nat, y : Nat, p : Nat) : Nat {
    mulMod(x, invMod(y, p), p)
  };
  public func negMod(x : Nat, p : Nat) : Nat {
    if (x == 0) return 0;
    p - x
  };

  public class Fp() {
    private var v_ : Nat = 0;
    public func val() : Nat { v_ };
    public func set(v : Nat) {
      v_ := v % p_;
    };
    // set v without modulo
    public func setNoCheck(v : Nat) {
      v_ := v;
    };
    public func isZero() : Bool {
      v_ == 0
    };
    public func equal(rhs : Fp) : Bool {
      v_ == rhs.val()
    };
    public func add(rhs : Fp) : Fp {
      let ret = Fp();
      ret.setNoCheck(addMod(v_, rhs.val(), p_));
      ret
    };
    public func sub(rhs : Fp) : Fp {
      let ret = Fp();
      ret.setNoCheck(subMod(v_, rhs.val(), p_));
      ret
    };
    public func mul(rhs : Fp) : Fp {
      let ret = Fp();
      ret.setNoCheck(mulMod(v_, rhs.val(), p_));
      ret
    };
    public func inv() : Fp {
      let ret = Fp();
      ret.setNoCheck(invMod(v_, p_));
      ret
    };
    public func div(rhs : Fp) : Fp {
      mul(rhs.inv())
    };
    public func neg() : Fp {
      let ret = Fp();
      ret.setNoCheck(negMod(v_, p_));
      ret
    };
  };
  public func newFp(x : Nat) : Fp {
    let ret = Fp();
    ret.set(x);
    ret
  };
  public func newFp_nomod(x : Nat) : Fp {
    let ret = Fp();
    ret.setNoCheck(x);
    ret
  };
  // mod fp functions
  public func fp_add(x : Nat, y : Nat) : Nat {
    addMod(x, y, p_)
  };
  public func fp_sub(x : Nat, y : Nat) : Nat {
    subMod(x, y, p_)
  };
  public func fp_mul(x : Nat, y : Nat) : Nat {
    mulMod(x, y, p_)
  };
  public func fp_div(x : Nat, y : Nat) : Nat {
    divMod(x, y, p_)
  };
  public func fp_neg(x : Nat) : Nat {
    negMod(x, p_)
  };
  public func fp_inv(x : Nat) : Nat {
    invMod(x, p_)
  };
  // mod fr functions
  public func fr_add(x : Nat, y : Nat) : Nat {
    addMod(x, y, r_)
  };
  public func fr_sub(x : Nat, y : Nat) : Nat {
    subMod(x, y, r_)
  };
  public func fr_mul(x : Nat, y : Nat) : Nat {
    mulMod(x, y, r_)
  };
  public func fr_div(x : Nat, y : Nat) : Nat {
    divMod(x, y, r_)
  };
  public func fr_neg(x : Nat) : Nat {
    negMod(x, r_)
  };
  public func fr_inv(x : Nat) : Nat {
    invMod(x, r_)
  };

  func _isValid(x : Nat, y : Nat) : Bool {
    // return y^2 == (x^2 + a)x + b
    let lhs = fp_mul(y, y);
    let rhs = fp_add(fp_mul(fp_add(fp_mul(x, x), a_), x), b_);
    lhs == rhs
  };
  public class Ec() {
    private var x_ : Nat  = 0;
    private var y_ : Nat  = 0;
    private var isZero_ : Bool = true;
    public func val() : (Nat, Nat) { (x_, y_) };
    public func x() : Nat { x_ };
    public func y() : Nat { y_ };
    public func set(x : Nat, y : Nat) : Bool {
      if (not _isValid(x, y)) return false;
      x_ := x;
      y_ := y;
      isZero_ := false;
      true
    };
    public func setNoCheck(x : Nat, y : Nat) {
      x_ := x;
      y_ := y;
      isZero_ := false;
    };
    public func isZero() : Bool { isZero_ };
    public func isValid() : Bool {
      if (isZero_) return true;
      _isValid(x_, y_)
    };
    public func neg() : Ec {
      if (isZero()) return Ec();
      newEcNoCheck(x_, fp_neg(y_))
    };
	// QQQ : how can I return *this?
	public func copy() : Ec {
		if (isZero()) return Ec();
		newEcNoCheck(x_, y_)
	};
    public func add(rhs : Ec) : Ec {
      if (isZero()) return rhs;
      // QQQ : how can I return *this?
      if (rhs.isZero()) return copy();
      var nume = 0;
      var deno = 0;
      let x2 = rhs.x();
      let y2 = rhs.y();
      if (x_ == x2) {
        // P + (-P) = 0
        if (y_ == fp_neg(y2)) return Ec();
        // dbl
        let xx = fp_mul(x_, x_);
        let xx3 = fp_add(fp_add(xx, xx), xx);
        nume := fp_add(xx3, a_);
        deno := fp_add(y_, y_);
      } else {
        nume := fp_sub(y_, y2);
        deno := fp_sub(x_, x2);
      };
      let L = fp_div(nume, deno);
      let x3 = fp_sub(fp_mul(L, L), fp_add(x_, x2));
      let y3 = fp_sub(fp_mul(L, fp_sub(x_, x3)), y_);
      newEcNoCheck(x3, y3)
    };
    public func dbl() : Ec {
      if (isZero()) return Ec();
      var nume = 0;
      var deno = 0;
      // P + (-P) = 0
      if (y_ == 0) return Ec();
      let xx = fp_mul(x_, x_);
      let xx3 = fp_add(fp_add(xx, xx), xx);
      nume := fp_add(xx3, a_);
      deno := fp_add(y_, y_);
      let L = fp_div(nume, deno);
      let x3 = fp_sub(fp_mul(L, L), fp_add(x_, x_));
      let y3 = fp_sub(fp_mul(L, fp_sub(x_, x3)), y_);
      newEcNoCheck(x3, y3)
    };
    public func equal(rhs : Ec) : Bool {
      if (isZero()) return rhs.isZero();
      if (rhs.isZero()) return false;
      // both are not zero
      x_ == rhs.x() and y_ == rhs.y()
    };
    public func mul(x : Nat) : Ec {
      if (x == 0) return Ec();
      let bs = fromNatToReverseBin(x);
      let self = copy();
      var ret = Ec();
      for (b in bs.vals()) {
        ret := dbl();
        if (b) ret := ret.add(self);
      };
      ret
    };
  };
  public func newEc(x : Nat, y : Nat) : Ec {
    let P = Ec();
    if (P.set(x, y)) return P;
    assert(false);
    Ec()
  };
  public func newEcNoCheck(x : Nat, y : Nat) : Ec {
    let P = Ec();
    P.setNoCheck(x, y);
    P
  };
  public func newEc_P() : Ec {
    newEcNoCheck(gx_, gy_)
  };
  // [0x12, 0x34] : [Nat] => 0x1234
  public func toBigEndianNat(b : [Nat8]) : Nat {
    var v : Nat = 0;
    for (e in b.vals()) {
      v := v * 256 + Nat8.toNat(e);
    };
    v
  };
  /// get secret key from [Nat8]
  public func getSecretKey(rand : [Nat8]) : ?Nat {
    let sec = toBigEndianNat(rand) % r_;
    if (sec == 0) return null;
    ?sec
  };
  /// get public key from sec
  public func getPublicKey(sec : Nat) : ?(Nat, Nat) {
    let P = newEc_P();
    let Q = P.mul(sec);
    if (Q.isZero()) return null;
    ?(Q.x(), Q.y())
  };
  /// sign hashed by sec and rand
  public func signHashed(sec : Nat, hashed : [Nat8], rand : [Nat8]) : ?(Nat, Nat) {
    if (sec == 0 or sec >= r_) return null;
    let k = toBigEndianNat(rand) % r_;
    if (k == 0) return null;
    let P = newEc_P();
    let Q = P.mul(k);
    if (Q.isZero()) return null;
    let r = Q.x() % r_;
    if (r == 0) return null;
    let z = toBigEndianNat(hashed) % r_;
    // s = (r * sec + z) / k
    let s = fr_div(fr_add(fr_mul(r, sec), z), k);
    ?(r, s)
  };
  // verify hashed and sign by pub
  public func verifyHashed(pub : (Nat, Nat), hashed : [Nat8], sig : (Nat, Nat)) : Bool {
    let (r, s) = sig;
    if (r == 0 or r >= r_) return false;
    if (s == 0 or s >= r_) return false;
    let z = toBigEndianNat(hashed) % r_;
    let w = fr_inv(s);
    let u1 = fr_mul(z, w);
    let u2 = fr_mul(r, w);
    let P = newEc_P();
    let (x, y) = pub;
    let Q = newEcNoCheck(x, y);
    if (not Q.isValid()) return false;
    let R = P.mul(u1).add(Q.mul(u2));
    if (R.isZero()) return false;
    (R.x() % r_) == r
  };
};
