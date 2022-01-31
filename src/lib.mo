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
import SHA2 "mo:sha2";
import Curve "curve";
import Util "lib_util";
import Prelude "mo:base/Prelude";

module {
  let p_ = Curve.params.p;
  let rHalf_ = Curve.params.rHalf;

  public func sha2(iter : Iter.Iter<Nat8>) : Blob {
    SHA2.fromIter(#sha256, iter)
  };

  public type FpElt = Curve.FpElt;
  public type FrElt = Curve.FrElt;
  public let Fp = Curve.Fp;
  public let Fr = Curve.Fr;

  public type Affine = Curve.Affine;
  public type Point = Curve.Point;

  /// Get secret key from rand.
  /// rand : Nat8 values
  /// return secret key in [1, r_-1]
  public func getSecretKey(rand : Iter.Iter<Nat8>) : ?FrElt {
    let sec = Fr.fromNat(Util.toNatAsBigEndian(rand));
    if (sec == #fr(0)) null else ?sec
  };
  /// Get public key from sec.
  /// public key (x, y) is an affine point of elliptic curve
  public func getPublicKey(sec : FrElt) : Affine {
    switch (Curve.mul_base(sec)) {
      case (#zero) Prelude.unreachable();
      case (#affine(c)) c;
    }
  };
  /// Sign hashed by sec and rand return lower S signature (r, s) such that s < rHalf_
  /// hashed : 32-byte SHA-256 value of a message.
  /// rand : 32-byte random value.
  public func signHashed(sec : FrElt, hashed : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(FrElt, FrElt) {
    if (sec == #fr(0)) return null; // 0 is an invalid secret key
    let k = Fr.fromNat(Util.toNatAsBigEndian(rand));
    if (k == #fr(0)) return null; // 0 is an invalid k value
    let Q = Curve.mul_base(k);
    let x : FpElt = switch (Q) {
      case (#zero) Prelude.unreachable(); // should not happen because k is non-zero
      case (#affine(x, _)) x;
    };
    let r = Fr.fromNat(Fp.toNat(x));
    if (r == #fr(0)) return null; // 0 is an invalid r value
    let z = Fr.fromNat(Util.toNatAsBigEndian(hashed));
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
    let z = Fr.fromNat(Util.toNatAsBigEndian(hashed));
    let w = Fr.inv(s);
    let u1 = Fr.mul(z, w);
    let u2 = Fr.mul(r, w);
    if (not Curve.isValid(pub)) return false;
    let Q = #affine(pub);
    let R = Curve.add(Curve.mul_base(u1),Curve.mul(Q,u2));
    switch (R) {
      case (#zero) false;
      case (#affine(x,_)) Fr.fromNat(Fp.toNat(x)) == r
    }
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
  public func serializePublicKeyUncompressed(pub : (FpElt, FpElt)) : Blob {
    let prefix = 0x04 : Nat8;
    let n = 32;
    let x = Util.toBigEndianPad(n, Fp.toNat(pub.0));
    let y = Util.toBigEndianPad(n, Fp.toNat(pub.1));
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
  public func serializePublicKeyCompressed(pub : (FpElt, FpElt)) : Blob {
    let prefix : Nat8 = if ((Fp.toNat(pub.1) % 2) == 0) 0x02 else 0x03;
    let n = 32;
    let x = Util.toBigEndianPad(n, Fp.toNat(pub.0));
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
  /// Deserialize an uncompressed public key
  public func deserializePublicKeyUncompressed(b : Blob) : ?(FpElt, FpElt) {
    if(b.size() != 65) return null;
    let a = Blob.toArray(b);
    if (a[0] != 0x04) return null;
    class range(a : [Nat8], begin : Nat, size : Nat) {
      var i = 0;
      public func next() : ?Nat8 {
        if (i == size) return null;
        let ret = ?a[begin + i];
        i += 1;
        ret
      };
    };
    let n = 32;
    let x = Util.toNatAsBigEndian(range(a, 1, n));
    let y = Util.toNatAsBigEndian(range(a, 1+n, n));
    ?(#fp(x), #fp(y));
  };
  /// Deserialize a compressed public key.
  public func deserializePublicKeyCompressed(b : Blob) : ?(FpElt, FpElt) {
    let n = 32;
    if (b.size() != n + 1) return null;
    let iter = b.vals();
    var even = true;
    switch (iter.next()) {
      case (?0x02) { even := true; };
      case (?0x03) { even := false; };
      case _ { return null; };
    };
    let x_ = Util.toNatAsBigEndian(iter);
    if (x_ >= p_) return null;
    let x = #fp(x_);
    switch (Curve.getYfromX(x, even)) {
      case (null) return null;
      case (?y) return ?(x, y);
    };
  };
  /// serialize to DER format
  /// https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/ch04.html
  public func serializeSignatureDer((r, s) : (Nat, Nat)) : Blob {
    var buf = Buffer.Buffer<Nat8>(80);
    buf.add(0x30); // top marker
    buf.add(0); // modify later
    let append = func(x : Nat) {
      buf.add(0x02); // marker
      let a = Util.toBigEndian(x);
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
  public func deserializeSignatureDer(b : Blob) : ?(Nat, Nat) {
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
