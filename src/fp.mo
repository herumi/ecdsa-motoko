/**
 * Module      : fp.mo
 * Description : finite field
 * Copyright   : 2022 Mitsunari Shigeo
 * License     : Apache 2.0 with LLVM Exception
 * Maintainer  : herumi <herumi@nifty.com>
 * Stability   : Stable
 */

import Int "mo:base/Int";

module {
  let p_ : Nat = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;

  /// return the order of the field where Ec is defined.
  public func p() : Nat = p_;

  // return (gcd, x, y) such that gcd = a * x + b * y
  public func extGcd(a : Int, b : Int) : (Int, Int, Int) {
    if (a == 0) return (b, 0, 1);
    let (q, r) = (b/a, b%a);
    let (gcd, x, y) = extGcd(r, a);
    return (gcd, y - q * x, x)
  };
  // return rev such that x * rev mod p = 1 if success else 0
  public func invMod(x : Nat, p : Nat) : Nat {
    let (gcd, rev, _) = extGcd(x, p);
    assert(gcd == 1);
    let v = if (rev < 0) rev+p else rev;
    assert(0 <= v and v < p);
    Int.abs(v)
  };
  func addMod(x : Nat, y : Nat, p : Nat) : Nat {
    let z = x+y;
    if (z < p) z else z-p;
  };
  func mulMod(x : Nat, y : Nat, p : Nat) : Nat = (x * y) % p;
  func subMod(x : Nat, y : Nat, p : Nat) : Nat = if (x >= y) x-y else x+p-y;
  func divMod(x : Nat, y : Nat, p : Nat) : Nat = (x * invMod(y, p)) % p;
  func negMod(x : Nat, p : Nat) : Nat = if (x == 0) 0 else p - x;

  public class Fp() {
    var v_ : Nat = 0;
    public func val() : Nat = v_;
    public func set(v : Nat) {
      v_ := v % p_;
    };
    // set v without modulo
    public func setNoCheck(v : Nat) {
      v_ := v;
    };
    public func isZero() : Bool = v_ == 0;
    public func equal(rhs : Fp) : Bool = v_ == rhs.val();
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
  }; // class Fp
  public func newFp(x : Nat) : Fp {
    let ret = Fp();
    ret.set(x);
    ret
  };
  public func newFpNoCheck(x : Nat) : Fp {
    let ret = Fp();
    ret.setNoCheck(x);
    ret
  };
};
