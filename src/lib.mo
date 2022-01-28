/**
 * Module      : lib.mo
 * Description : ECDSA-SHA-256
 * Copyright   : 2022 Mitsunari Shigeo
 * License     : Apache 2.0 with LLVM Exception
 * Maintainer  : herumi <herumi@nifty.com>
 * Stability   : Stable
 */

import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import SHA2 "mo:sha2";
import Zn "zn";

module {
  // secp256k1
  // Ec/Fp : y^2 = x^3 + ax + b
  // (gx, gy) in Ec
  // #Ec = r
  let p_ : Nat = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
  let a_ : Nat = 0;
  let b_ : Nat = 7;
  let r_ : Nat = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
  let gx_ : Nat = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798;
  let gy_ : Nat = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8;

  // pSqrRoot_ = (p_ + 1) / 4;
  let pSqrRoot_ : Nat = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffbfffff0c;

  // rHalf_ = (r_ + 1) / 2;
  let rHalf_ : Nat = 0x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a1;

  /// return the order of the field where Ec is defined.
  public func p() : Nat = p_;
  /// return the order of the generator of Ec.
  public func r() : Nat = r_;

  /// return the generator of Ec.
  public func generator() : (Nat, Nat) = (gx_, gy_);

  public func sha2(iter : Iter.Iter<Nat8>) : Blob {
    SHA2.fromIter(#sha256, iter)
  };

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

  // Static version of 
  //   let fp = Zn.Zn(p_);
  // The line above is not static. We can use the type Zn.Zn, but have to 
  // write out the constructor again with fixed p_ to make it static.
  public let fp : Zn.Zn = {
    add = func (x : Nat, y : Nat) : Nat = Zn.add_(x, y, p_);
    mul = func (x : Nat, y : Nat) : Nat = Zn.mul_(x, y, p_);
    sub = func (x : Nat, y : Nat) : Nat = Zn.sub_(x, y, p_);
    div = func (x : Nat, y : Nat) : Nat = Zn.div_(x, y, p_);
    pow = func (x : Nat, y : Nat) : Nat = Zn.pow_(x, y, p_);
    neg = func (x : Nat) : Nat = Zn.neg_(x, p_);
    inv = func (x : Nat) : Nat = Zn.inv_(x, p_);
    sqr = func (x : Nat) : Nat = Zn.sqr_(x, p_);
  };

  // Static version of 
  //   let fp = Zn.Zn(r_);
  // The line above is not static. We can use the type Zn.Zn, but have to 
  // write out the constructor again with fixed r_ to make it static.
  public let fr : Zn.Zn = {
    add = func (x : Nat, y : Nat) : Nat = Zn.add_(x, y, r_);
    mul = func (x : Nat, y : Nat) : Nat = Zn.mul_(x, y, r_);
    sub = func (x : Nat, y : Nat) : Nat = Zn.sub_(x, y, r_);
    div = func (x : Nat, y : Nat) : Nat = Zn.div_(x, y, r_);
    pow = func (x : Nat, y : Nat) : Nat = Zn.pow_(x, y, r_);
    neg = func (x : Nat) : Nat = Zn.neg_(x, r_);
    inv = func (x : Nat) : Nat = Zn.inv_(x, r_);
    sqr = func (x : Nat) : Nat = Zn.sqr_(x, r_);
  };

  // more mod fp functions
  public func fpSqrRoot(x : Nat) : ?Nat {
    let sq = fp.pow(x, pSqrRoot_);
    if (fp.sqr(sq) == x) ?sq else null
  };

  // return x^3 + ax + b
  func getYsqrFromX(x : Nat) : Nat {
    fp.add(fp.mul(fp.add(fp.sqr(x), a_), x), b_)
  };
  func _isValid(x : Nat, y : Nat) : Bool {
    fp.sqr(y) == getYsqrFromX(x)
  };

  public class Ec() {
    var x_ : Nat = 0;
    var y_ : Nat = 0;
    var isZero_ : Bool = true;
    public func affine() : (Nat, Nat) = (x_, y_);
    public func x() : Nat = x_;
    public func y() : Nat = y_;
    public func set(x : Nat, y : Nat) : Bool {
      if (not _isValid(x, y)) return false;
      x_ := x;
      y_ := y;
      isZero_ := false;
      return true
    };
    public func setNoCheck(x : Nat, y : Nat) {
      x_ := x;
      y_ := y;
      isZero_ := false;
    };
    public func isZero() : Bool = isZero_;
    public func isValid() : Bool = isZero_ or _isValid(x_, y_);
    public func neg() : Ec = if (isZero()) Ec() else newEcNoCheck(x_, fp.neg(y_));
    // QQQ : how can I return *this?
    public func copy() : Ec = if (isZero()) Ec() else newEcNoCheck(x_, y_);
    public func add(rhs : Ec) : Ec {
      if (isZero()) return rhs;
      if (rhs.isZero()) return copy();
      var nume = 0;
      var deno = 0;
      let x2 = rhs.x();
      let y2 = rhs.y();
      if (x_ == x2) {
        // P + (-P) = 0
        if (y_ == fp.neg(y2)) return Ec();
        // dbl
        let xx = fp.sqr(x_);
        let xx3 = fp.add(fp.add(xx, xx), xx);
        nume := fp.add(xx3, a_);
        deno := fp.add(y_, y_);
      } else {
        nume := fp.sub(y_, y2);
        deno := fp.sub(x_, x2);
      };
      let L = fp.div(nume, deno);
      let x3 = fp.sub(fp.sqr(L), fp.add(x_, x2));
      let y3 = fp.sub(fp.mul(L, fp.sub(x_, x3)), y_);
      return newEcNoCheck(x3, y3)
    };
    public func dbl() : Ec {
      if (isZero()) return Ec();
      var nume = 0;
      var deno = 0;
      // P + (-P) = 0
      if (y_ == 0) return Ec();
      let xx = fp.sqr(x_);
      let xx3 = fp.add(fp.add(xx, xx), xx);
      nume := fp.add(xx3, a_);
      deno := fp.add(y_, y_);
      let L = fp.div(nume, deno);
      let x3 = fp.sub(fp.sqr(L), fp.add(x_, x_));
      let y3 = fp.sub(fp.mul(L, fp.sub(x_, x3)), y_);
      return newEcNoCheck(x3, y3)
    };
    public func equal(rhs : Ec) : Bool {
      if (isZero()) return rhs.isZero();
      if (rhs.isZero()) return false;
      // both are not zero
      return x_ == rhs.x() and y_ == rhs.y()
    };
    public func mul(x : Nat) : Ec {
      if (x == 0) return Ec();
      let bs = toReverseBin(x);
      let self = copy();
      var ret = Ec();
      let n = bs.size();
      var i = 0;
      while (i < n) {
        let b = bs[n - 1 - i];
        ret := ret.dbl();
        if (b) {
          ret := ret.add(self);
        };
        i += 1;
      };
      return ret
    };
    public func put() {
      if (isZero()) {
        Debug.print("0");
      } else {
        Debug.print("x=" # toHex(x_));
        Debug.print("y=" # toHex(y_));
      };
    };
  };
  public func newEc(x : Nat, y : Nat) : ?Ec {
    let P = Ec();
    if (P.set(x, y)) ?P else null
  };
  public func newEcNoCheck(x : Nat, y : Nat) : Ec {
    let P = Ec();
    P.setNoCheck(x, y);
    return P
  };
  /// return the generator of Ec
  public func newEcGenerator() : Ec = newEcNoCheck(gx_, gy_);
  // [0x12, 0x34] : [Nat] => 0x1234
  public func toNatAsBigEndian(iter : Iter.Iter<Nat8>) : Nat {
    var v = 0;
    label l loop {
      switch (iter.next()) {
        case (null) break l;
        case (?e) v := v * 256 + Nat8.toNat(e);
      };
    };
    return v
  };
  /// 0x1234 => [0x12, 0x34], 0 => [0]
  public func toBigEndian(x : Nat) : [Nat8] {
    if (x == 0) return [0];
    var buf = Buffer.Buffer<Nat8>(64);
    var t = x;
    while (t > 0) {
      buf.add(Nat8.fromNat(t % 256));
      t /= 256;
    };
    let n = buf.size();
    let ith = func(i : Nat) : Nat8 {
      buf.get(n - 1 - i)
    };
    Array.tabulate<Nat8>(n, ith)
  };
  /// (5, 0x1234) => [0x00, 0x00, 0x00, 0x12, 0x34]
  public func toBigEndianPad(len : Nat, x : Nat) : [Nat8] {
    var buf = Buffer.Buffer<Nat8>(len);
    var t = x;
    var i = 0;
    while (i < len) {
      buf.add(Nat8.fromNat(t % 256));
      t /= 256;
      i += 1;
    };
    let ith = func(i : Nat) : Nat8 {
      buf.get(len - 1 - i)
    };
    Array.tabulate<Nat8>(len, ith)
  };
  /// Get secret key from rand.
  /// rand : Nat8 values
  /// return secret key in [1, r_-1]
  public func getSecretKey(rand : Iter.Iter<Nat8>) : ?Nat {
    let sec = toNatAsBigEndian(rand) % r_;
    if (sec == 0) null else ?sec
  };
  /// Get public key from sec.
  /// public key (x, y) is a point of elliptic curve
  public func getPublicKey(sec : Nat) : ?(Nat, Nat) {
    let P = newEcGenerator();
    let Q = P.mul(sec);
    if (Q.isZero()) null else ?(Q.x(), Q.y())
  };
  /// Sign hashed by sec and rand return lower S signature (r, s) such that s < rHalf_
  /// hashed : 32-byte SHA-256 value of a message.
  /// rand : 32-byte random value.
  public func signHashed(sec : Nat, hashed : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(Nat, Nat) {
    if (sec == 0 or sec >= r_) return null;
    let k = toNatAsBigEndian(rand) % r_;
    if (k == 0) return null;
    let P = newEcGenerator();
    let Q = P.mul(k);
    if (Q.isZero()) return null;
    let r = Q.x() % r_;
    if (r == 0) return null;
    let z = toNatAsBigEndian(hashed) % r_;
    // s = (r * sec + z) / k
    var s = fr.div(fr.add(fr.mul(r, sec), z), k);
    if (s >= rHalf_) s := fr.neg(s);
    ?(r, s)
  };
  /// convert a signature to lower S signature
  public func normalizeSignature((r, s) : (Nat, Nat)) : (Nat, Nat) {
    if (s < rHalf_) (r, s) else (r, fr.neg(s))
  };
  /// verify a tuple of pub, hashed, and lowerS sig
  public func verifyHashed(pub : (Nat, Nat), hashed : Iter.Iter<Nat8>, sig : (Nat, Nat)) : Bool {
    let (r, s) = sig;
    if (r == 0 or r >= r_) return false;
    if (s == 0 or s >= rHalf_) return false;
    let z = toNatAsBigEndian(hashed) % r_;
    let w = fr.inv(s);
    let u1 = fr.mul(z, w);
    let u2 = fr.mul(r, w);
    let P = newEcGenerator();
    let (x, y) = pub;
    let Q = newEcNoCheck(x, y);
    if (not Q.isValid()) return false;
    let R = P.mul(u1).add(Q.mul(u2));
    if (R.isZero()) return false;
    return (R.x() % r_) == r
  };
  /// Sign a message by sec and rand with SHA-256
  public func sign(sec : Nat, msg : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(Nat, Nat) {
    signHashed(sec, sha2(msg).vals(), rand)
  };
  // verify a tuple of pub, msg, and sig
  public func verify(pub : (Nat, Nat), msg : Iter.Iter<Nat8>, sig : (Nat, Nat)) : Bool {
    verifyHashed(pub, sha2(msg).vals(), sig)
  };
  /// return 0x04 + bigEndian(x) + bigEndian(y)
  public func serializeUncompressed(pub : (Nat, Nat)) : Blob {
    let prefix = 0x04 : Nat8;
    let n = 32;
    let x = toBigEndianPad(n, pub.0);
    let y = toBigEndianPad(n, pub.1);
    let ith = func(i : Nat) : Nat8 {
      if (i == 0) {
        prefix
      } else if (i <= n) {
        x[i - 1]
      } else {
        y[i - 1 - n]
      }
    };
    let ar = Array.tabulate<Nat8>(1+n*2, ith);
    Blob.fromArray(ar)
  };
  /// return 0x02 + bigEndian(x) if y is even
  /// return 0x03 + bigEndian(x) if y is odd
  public func serializeCompressed(pub : (Nat, Nat)) : Blob {
    let prefix : Nat8 = if ((pub.1 % 2) == 0) 0x02 else 0x03;
    let n = 32;
    let x = toBigEndianPad(n, pub.0);
    let ith = func(i : Nat) : Nat8 {
      if (i == 0) {
        prefix
      } else {
        x[i - 1]
      }
    };
    let ar = Array.tabulate<Nat8>(1+n, ith);
    Blob.fromArray(ar)
  };
  /// Get y corresponding to x such that y^2 = x^ + ax + b.
  /// Return even y if `even` is true.
  public func getYfromX(x : Nat, even : Bool) : ?Nat {
    let y2 = getYsqrFromX(x);
    switch (fpSqrRoot(y2)) {
      case (null) { return null };
      case (?y) {
        return if (even == ((y % 2) == 0)) ?y else ?fp.neg(y)
      };
    }
  };
  /// Deserialize a compressed public key.
  public func deserializeCompressed(b : Blob) : ?(Nat, Nat) {
    let n = 32;
    if (b.size() != n + 1) return null;
    let iter = b.vals();
    var even = true;
    switch (iter.next()) {
      case (?0x02) { even := true; };
      case (?0x03) { even := false; };
      case _ { return null; };
    };
    let x = toNatAsBigEndian(iter);
    if (x >= p_) return null;
    switch (getYfromX(x, even)) {
      case (null) return null;
      case (?y) return ?(x, y);
    };
  };
  /// serialize to DER format
  /// https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/ch04.html
  public func serializeToDer((r, s) : (Nat, Nat)) : Blob {
    var buf = Buffer.Buffer<Nat8>(80);
    buf.add(0x30); // top marker
    buf.add(0); // modify later
    let append = func(x : Nat) {
      buf.add(0x02); // marker
      let a = toBigEndian(x);
      let adj = if (a[0] >= 0x80) 1 else 0;
      buf.add(Nat8.fromNat(a.size() + adj));
      if (adj == 1) buf.add(0x00);
      for (e in a.vals()) {
        buf.add(e);
      };
    };
    append(r);
    append(s);
    let va = buf.toVarArray();
    va[1] := Nat8.fromNat(va.size()) - 2;
    Blob.fromArrayMut(va)
  };
  /// deserialize DER to signature
  public func deserializeDer(b : Blob) : ?(Nat, Nat) {
    let a = Blob.toArray(b);
    if (a[0] != 0x30) return null;
    if (a.size() != Nat8.toNat(a[1]) + 2) return null;
    let read = func(a : [Nat8], begin : Nat) : ?(Nat, Nat) {
      if (a[begin] != 0x02) return null;
      let n = Nat8.toNat(a[begin + 1]);
      if (a.size() < begin + 1 + n) return null;
      var v = 0;
      var i = 0;
      while (i < n) {
        v := v * 256 + Nat8.toNat(a[begin + 2 + i]);
        i += 1;
      };
      ?(n + 2, v)
    };
    switch (read(a, 2)) {
      case (null) { null };
      case (?(read1, r)) {
        switch (read(a, 2 + read1)) {
          case (null) { null };
          case (?(read2, s)) {
            if (a.size() != 2 + read1 + read2) return null;
            ?(r, s)
          };
        };
      };
    };
  };
};
