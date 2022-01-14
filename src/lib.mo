/**
 * Module      : fp.mo
 * Description : finite field
 * Copyright   : 2022 Mitsunari Shigeo
 * License     : Apache 2.0 with LLVM Exception
 * Maintainer  : herumi <herumi@nifty.com>
 * Stability   : Stable
 */

import Int "mo:base/Int";
import B "mo:base/Buffer";

module {
  // secp256k1
  // Ec/Fp : y^2 = x^3 + ax + b
  // (gx, gy) in Ec
  // #Ec = r
  public let p_:Nat = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
  public let a_:Nat = 0;
  public let b_:Nat = 7;
  public let r_:Nat = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
  public let gx_:Nat = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798;
  public let gy_:Nat = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8;

  /// return the order of the field where Ec is defined.
  public func get_p() : Nat { p_ };
  /// return the order of the generator of Ec.
  public func get_r() : Nat { r_ };

  // 13 = 0b1101 => [true,false,true,ture]
  public func Nat_to_reverse_bin(x : Nat) : [Bool] {
    var ret:B.Buffer<Bool> = B.Buffer<Bool>(256);
    var t = x;
    while (t > 0) {
      ret.add((t % 2) == 1);
      t := t / 2;
    };
    ret.toArray()
  };

  // return (gcd, x, y) such that gcd = a * x + b * y
  public func ext_gcd(a:Int, b:Int) : (Int, Int, Int) {
    if (a == 0) return (b, 0, 1);
    let q = b / a;
    let r = b % a;
    let (gcd, x, y) = ext_gcd(r, a);
    (gcd, y - q * x, x)
  };
  public func add_mod(x:Nat, y:Nat, p:Nat) : Nat {
    let v = x + y;
    if (v < p) return v;
    v - p
  };
  public func sub_mod(x:Nat, y:Nat, p:Nat) : Nat {
    if (x >= y) return x - y;
    x + p - y
  };
  public func mul_mod(x:Nat, y:Nat, p:Nat) : Nat {
    (x * y) % p
  };
  // return rev such that x * rev mod p = 1 if success else 0
  public func inv_mod(x:Nat, p:Nat) : Nat {
    let (gcd, rev, dummy) = ext_gcd(x, p);
    assert(gcd == 1);
    var v = rev;
    if (rev < 0) v := rev + p;
    assert(0 <= v and v < p);
    Int.abs(v)
  };
  public func div_mod(x:Nat, y:Nat, p:Nat) : Nat {
    mul_mod(x, inv_mod(y, p), p)
  };
  public func neg_mod(x:Nat, p:Nat) : Nat {
    if (x == 0) return 0;
    p - x
  };

  public class Fp() {
    private var v_ : Nat = 0;
    public func get(): Nat { v_ };
    public func set(v:Nat) {
      v_ := v % p_;
    };
    // set v without modulo
    public func set_nomod(v:Nat) {
      v_ := v;
    };
    public func is_zero() : Bool {
      v_ == 0
    };
    public func add(rhs:Fp) : Fp {
      let ret = Fp();
      ret.set_nomod(add_mod(v_, rhs.get(), p_));
      ret
    };
    public func sub(rhs:Fp) : Fp {
      let ret = Fp();
      ret.set_nomod(sub_mod(v_, rhs.get(), p_));
      ret
    };
    public func mul(rhs:Fp) : Fp {
      let ret = Fp();
      ret.set_nomod(mul_mod(v_, rhs.get(), p_));
      ret
    };
    public func inv() : Fp {
      let ret = Fp();
      ret.set_nomod(inv_mod(v_, p_));
      ret
    };
    public func div(rhs:Fp) : Fp {
      mul(rhs.inv())
    };
    public func neg() : Fp {
      let ret = Fp();
      ret.set_nomod(neg_mod(v_, p_));
      ret
    };
  };
  public func newFp(x:Nat) : Fp {
    let ret = Fp();
    ret.set(x);
    ret
  };
  public func newFp_nomod(x:Nat) : Fp {
    let ret = Fp();
    ret.set_nomod(x);
    ret
  };
  public func fp_add(x:Nat, y:Nat) : Nat {
    add_mod(x, y, p_)
  };
  public func fp_sub(x:Nat, y:Nat) : Nat {
    sub_mod(x, y, p_)
  };
  public func fp_mul(x:Nat, y:Nat) : Nat {
    mul_mod(x, y, p_)
  };
  public func fp_div(x:Nat, y:Nat) : Nat {
    div_mod(x, y, p_)
  };
  public func fp_neg(x:Nat) : Nat {
    neg_mod(x, p_)
  };
  public func fp_inv(x:Nat) : Nat {
    inv_mod(x, p_)
  };
  func _is_valid(x:Nat, y:Nat) : Bool {
    // return y^2 == (x^2 + a)x + b
    let lhs = fp_mul(y, y);
    let rhs = fp_add(fp_mul(fp_add(fp_mul(x, x), a_), x), b_);
    lhs == rhs
  };
  public class Ec() {
    private var x_:Nat  = 0;
    private var y_:Nat  = 0;
    private var isZero_:Bool = true;
    public func get() : (Nat, Nat) { (x_, y_) };
    public func get_x() : Nat { x_ };
    public func get_y() : Nat { y_ };
    public func set(x:Nat, y:Nat) : Bool {
      if (not _is_valid(x, y)) return false;
      x_ := x;
      y_ := y;
      isZero_ := false;
      true
    };
    public func set_nocheck(x:Nat, y:Nat) {
      x_ := x;
      y_ := y;
      isZero_ := false;
    };
    public func is_zero() : Bool { isZero_ };
    public func is_valid() : Bool {
      if (isZero_) return true;
      _is_valid(x_, y_)
    };
    public func neg() : Ec {
      if (is_zero()) return Ec();
      newEc_nocheck(x_, fp_neg(y_))
    };
	// QQQ : how can I return *this?
	public func copy() : Ec {
		if (is_zero()) return Ec();
		newEc_nocheck(x_, y_)
	};
    public func add(rhs : Ec) : Ec {
      if (is_zero()) return rhs;
      // QQQ : how can I return *this?
      if (rhs.is_zero()) return copy();
      var nume = 0;
      var deno = 0;
      let x2 = rhs.get_x();
      let y2 = rhs.get_y();
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
      newEc_nocheck(x3, y3)
    };
    public func dbl() : Ec {
      if (is_zero()) return Ec();
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
      newEc_nocheck(x3, y3)
    };
    public func equal(rhs : Ec) : Bool {
      if (is_zero()) return rhs.is_zero();
      if (rhs.is_zero()) return false;
      // both are not zero
      x_ == rhs.get_x() and y_ == rhs.get_y()
    };
    public func mul(x : Nat) : Ec {
      if (x == 0) return Ec();
      let bs = Nat_to_reverse_bin(x);
      let self = copy();
      var ret = Ec();
      for (b in bs.vals()) {
        ret := dbl();
        if (b) ret := ret.add(self);
      };
      ret
    };
  };
  public func newEc(x:Nat, y:Nat) : Ec {
    let P = Ec();
    if (P.set(x, y)) return P;
    assert(false);
    Ec()
  };
  public func newEc_nocheck(x:Nat, y:Nat) : Ec {
    let P = Ec();
    P.set_nocheck(x, y);
    P
  };
};
