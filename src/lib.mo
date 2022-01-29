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
import Field "field";

module {
  // secp256k1
  // Ec/Fp : y^2 = x^3 + ax + b
  // (gx, gy) in Ec
  // #Ec = r
  let p_ : Nat = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;
  let a_ = #fp(0);
  let b_ = #fp(7);
  let r_ : Nat = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141;
  let gx_ = #fp(0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798);
  let gy_ = #fp(0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8);

  // pSqrRoot_ = (p_ + 1) / 4;
  let pSqrRoot_ : Nat = 0x3fffffffffffffffffffffffffffffffffffffffffffffffffffffffbfffff0c;

  // rHalf_ = (r_ + 1) / 2;
  let rHalf_ : Nat = 0x7fffffffffffffffffffffffffffffff5d576e7357a4501ddfe92f46681b20a1;

  /// return the order of the field where Ec is defined.
  public func p() : Nat = p_;
  /// return the order of the generator of Ec.
  public func r() : Nat = r_;

  /// return the generator of Ec.
  public func generator() : (FpElt, FpElt) = (gx_, gy_);

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

  public type FpElt = { #fp : Nat; };
  public type FrElt = { #fr : Nat; };
  public let Fp = {
    fromNat = func (n : Nat) : FpElt {
      #fp(n % p_);
    };
    toNat = func (x : FpElt) : Nat {
      let #fp(x_) = x;
      x_
    };
    add = func(x : FpElt, y : FpElt) : FpElt {
      let #fp(x_) = x;
      let #fp(y_) = y;
      #fp(Field.add_(x_, y_, p_));
    };
    mul = func(x : FpElt, y : FpElt) : FpElt {
      let #fp(x_) = x;
      let #fp(y_) = y;
      #fp(Field.mul_(x_, y_, p_));
    };
    sub = func(x : FpElt, y : FpElt) : FpElt {
      let #fp(x_) = x;
      let #fp(y_) = y;
      #fp(Field.sub_(x_, y_, p_));
    };
    div = func(x : FpElt, y : FpElt) : FpElt {
      let #fp(x_) = x;
      let #fp(y_) = y;
      #fp(Field.div_(x_, y_, p_));
    };
    pow = func(x : FpElt, n : Nat) : FpElt {
      let #fp(x_) = x;
      #fp(Field.pow_(x_, n, p_));
    };
    neg = func(x : FpElt) : FpElt {
      let #fp(x_) = x;
      #fp(Field.neg_(x_, p_));
    };
    inv = func(x : FpElt) : FpElt {
      let #fp(x_) = x;
      #fp(Field.inv_(x_, p_));
    };
    sqr = func(x : FpElt) : FpElt {
      let #fp(x_) = x;
      #fp(Field.sqr_(x_, p_));
    };
  };
  public let Fr = {
    fromNat = func (n : Nat) : FrElt {
      #fr(n % r_);
    };
    toNat = func (x : FrElt) : Nat {
      let #fr(x_) = x;
      x_
    };
    add = func(x : FrElt, y : FrElt) : FrElt {
      let #fr(x_) = x;
      let #fr(y_) = y;
      #fr(Field.add_(x_, y_, r_));
    };
    mul = func(x : FrElt, y : FrElt) : FrElt {
      let #fr(x_) = x;
      let #fr(y_) = y;
      #fr(Field.mul_(x_, y_, r_));
    };
    sub = func(x : FrElt, y : FrElt) : FrElt {
      let #fr(x_) = x;
      let #fr(y_) = y;
      #fr(Field.sub_(x_, y_, r_));
    };
    div = func(x : FrElt, y : FrElt) : FrElt {
      let #fr(x_) = x;
      let #fr(y_) = y;
      #fr(Field.div_(x_, y_, r_));
    };
    pow = func(x : FrElt, n : Nat) : FrElt {
      let #fr(x_) = x;
      #fr(Field.pow_(x_, n, r_));
    };
    neg = func(x : FrElt) : FrElt {
      let #fr(x_) = x;
      #fr(Field.neg_(x_, r_));
    };
    inv = func(x : FrElt) : FrElt {
      let #fr(x_) = x;
      #fr(Field.inv_(x_, r_));
    };
    sqr = func(x : FrElt) : FrElt {
      let #fr(x_) = x;
      #fr(Field.sqr_(x_, r_));
    };
  };

  // more mod fp functions
  public func fpSqrRoot(x : FpElt) : ?FpElt {
    let sq = Fp.pow(x, pSqrRoot_);
    if (Fp.sqr(sq) == x) ?sq else null
  };

  // return x^3 + ax + b
  func getYsqrFromX(x : FpElt) : FpElt {
    Fp.add(Fp.mul(Fp.add(Fp.sqr(x), a_), x), b_)
  };
  func _isValid(x : FpElt, y : FpElt) : Bool {
    Fp.sqr(y) == getYsqrFromX(x) 
  };

  public class Ec() {
    var x_ = #fp(0);
    var y_ = #fp(0);
    var isZero_ : Bool = true;
    public func affine() : (FpElt, FpElt) = (x_, y_);
    public func x() : FpElt = x_;
    public func y() : FpElt = y_;
    public func set(x : FpElt, y : FpElt) : Bool {
      if (not _isValid(x, y)) return false;
      x_ := x;
      y_ := y;
      isZero_ := false;
      return true
    };
    public func setNoCheck(x : FpElt, y : FpElt) {
      x_ := x;
      y_ := y;
      isZero_ := false;
    };
    public func isZero() : Bool = isZero_;
    public func isValid() : Bool = isZero_ or _isValid(x_, y_);
    public func neg() : Ec = if (isZero()) Ec() else newEcNoCheck(x_, Fp.neg(y_));
    // QQQ : how can I return *this?
    public func copy() : Ec = if (isZero()) Ec() else newEcNoCheck(x_, y_);
    public func add(rhs : Ec) : Ec {
      if (isZero()) return rhs;
      if (rhs.isZero()) return copy();
      var nume = #fp(0);
      var deno = #fp(0);
      let x2 = rhs.x();
      let y2 = rhs.y();
      if (x_ == x2) {
        // P + (-P) = 0
        if (y_ == Fp.neg(y2)) return Ec();
        // dbl
        let xx = Fp.sqr(x_);
        let xx3 = Fp.add(Fp.add(xx, xx), xx);
        nume := Fp.add(xx3, a_);
        deno := Fp.add(y_, y_);
      } else {
        nume := Fp.sub(y_, y2);
        deno := Fp.sub(x_, x2);
      };
      let L = Fp.div(nume, deno);
      let x3 = Fp.sub(Fp.sqr(L), Fp.add(x_, x2));
      let y3 = Fp.sub(Fp.mul(L, Fp.sub(x_, x3)), y_);
      return newEcNoCheck(x3, y3)
    };
    public func dbl() : Ec {
      if (isZero()) return Ec();
      var nume = #fp(0);
      var deno = #fp(0);
      // P + (-P) = 0
      if (y_ == #fp(0)) return Ec();
      let xx = Fp.sqr(x_);
      let xx3 = Fp.add(Fp.add(xx, xx), xx);
      nume := Fp.add(xx3, a_);
      deno := Fp.add(y_, y_);
      let L = Fp.div(nume, deno);
      let x3 = Fp.sub(Fp.sqr(L), Fp.add(x_, x_));
      let y3 = Fp.sub(Fp.mul(L, Fp.sub(x_, x3)), y_);
      return newEcNoCheck(x3, y3)
    };
    public func equal(rhs : Ec) : Bool {
      if (isZero()) return rhs.isZero();
      if (rhs.isZero()) return false;
      // both are not zero
      return x_ == rhs.x() and y_ == rhs.y()
    };
    public func mul(x : FrElt) : Ec {
      if (x == #fr(0)) return Ec();
      let bs = toReverseBin(Fr.toNat(x));
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
        Debug.print("x=" # toHex(Fp.toNat(x_)));
        Debug.print("y=" # toHex(Fp.toNat(y_)));
      };
    };
  };
  public func newEc(x : FpElt, y : FpElt) : ?Ec {
    let P = Ec();
    if (P.set(x, y)) ?P else null
  };
  public func newEcNoCheck(x : FpElt, y : FpElt) : Ec {
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
  public func getSecretKey(rand : Iter.Iter<Nat8>) : ?FrElt {
    let sec = #fr(toNatAsBigEndian(rand) % r_);
    if (sec == #fr(0)) null else ?sec
  };
  /// Get public key from sec.
  /// public key (x, y) is a point of elliptic curve
  public func getPublicKey(sec : FrElt) : ?(FpElt, FpElt) {
    let P = newEcGenerator();
    let Q = P.mul(sec);
    if (Q.isZero()) null else ?(Q.x(), Q.y())
  };
  /// Sign hashed by sec and rand return lower S signature (r, s) such that s < rHalf_
  /// hashed : 32-byte SHA-256 value of a message.
  /// rand : 32-byte random value.
  public func signHashed(sec : FrElt, hashed : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(FrElt, FrElt) {
    if (sec == #fr(0)) return null; // 0 is an invalid secret key
    let k = Fr.fromNat(toNatAsBigEndian(rand));
    if (k == #fr(0)) return null; // 0 is an invalid k value
    let P = newEcGenerator();
    let Q = P.mul(k);
    if (Q.isZero()) return null; // TODO: isn't this check redundant?
    let r = Fr.fromNat(Fp.toNat(Q.x()));
    if (r == #fr(0)) return null; // 0 is an invalid r value
    let z = Fr.fromNat(toNatAsBigEndian(hashed));
    // s = (r * sec + z) / k
    let s = Fr.div(Fr.add(Fr.mul(r, sec), z), k);
    ?normalizeSignature(r,s)
  };
  /// convert a signature to lower S signature
  public func normalizeSignature((r, s) : (FrElt, FrElt)) : (FrElt, FrElt) {
    if (Fr.toNat(s) < rHalf_) (r, s) else (r, Fr.neg(s))
  };
  /// verify a tuple of pub, hashed, and lowerS sig
  public func verifyHashed(pub : (FpElt, FpElt), hashed : Iter.Iter<Nat8>, (r,s) : (FrElt, FrElt)) : Bool {
    if (r == #fr(0)) return false;
    if (s == #fr(0) or Fr.toNat(s) >= rHalf_) return false;
    let z = Fr.fromNat(toNatAsBigEndian(hashed));
    let w = Fr.inv(s);
    let u1 = Fr.mul(z, w);
    let u2 = Fr.mul(r, w);
    let P = newEcGenerator();
    let (x, y) = pub;
    let Q = newEcNoCheck(x, y);
    if (not Q.isValid()) return false;
    let R = P.mul(u1).add(Q.mul(u2));
    if (R.isZero()) return false;
    return Fr.fromNat(Fp.toNat(R.x())) == r
  };
  /// Sign a message by sec and rand with SHA-256
  public func sign(sec : FrElt, msg : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(FrElt, FrElt) {
    signHashed(sec, sha2(msg).vals(), rand)
  };
  // verify a tuple of pub, msg, and sig
  public func verify(pub : (FpElt, FpElt), msg : Iter.Iter<Nat8>, sig : (FrElt, FrElt)) : Bool {
    verifyHashed(pub, sha2(msg).vals(), sig)
  };
  /// return 0x04 + bigEndian(x) + bigEndian(y)
  public func serializeUncompressed(pub : (FpElt, FpElt)) : Blob {
    let prefix = 0x04 : Nat8;
    let n = 32;
    let x = toBigEndianPad(n, Fp.toNat(pub.0));
    let y = toBigEndianPad(n, Fp.toNat(pub.1));
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
  public func serializeCompressed(pub : (FpElt, FpElt)) : Blob {
    let prefix : Nat8 = if ((Fp.toNat(pub.1) % 2) == 0) 0x02 else 0x03;
    let n = 32;
    let x = toBigEndianPad(n, Fp.toNat(pub.0));
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
  public func getYfromX(x : FpElt, even : Bool) : ?FpElt {
    let y2 = getYsqrFromX(x);
    switch (fpSqrRoot(y2)) {
      case (null) { return null };
      case (?y) {
        return if (even == ((Fp.toNat(y) % 2) == 0)) ?y else ?Fp.neg(y)
      };
    }
  };
  /// Deserialize a compressed public key.
  public func deserializeCompressed(b : Blob) : ?(FpElt, FpElt) {
    let n = 32;
    if (b.size() != n + 1) return null;
    let iter = b.vals();
    var even = true;
    switch (iter.next()) {
      case (?0x02) { even := true; };
      case (?0x03) { even := false; };
      case _ { return null; };
    };
    let x_ = toNatAsBigEndian(iter);
    if (x_ >= p_) return null;
    let x = #fp(x_);
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
