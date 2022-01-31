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
  public func sha2(iter : Iter.Iter<Nat8>) : Blob {
    SHA2.fromIter(#sha256, iter)
  };

  public type PublicKey = Curve.Affine;
  public type SecretKey = { #non_zero : Curve.FrElt; };
  public type Signature = (Curve.FrElt, Curve.FrElt);

  let Fp = Curve.Fp;
  let Fr = Curve.Fr;

  func getExponent(rand : Iter.Iter<Nat8>) : Curve.FrElt =
    Fr.fromNat(Util.toNatAsBigEndian(rand));

  /// Get secret key from rand.
  public func getSecretKey(rand : Iter.Iter<Nat8>) : ?SecretKey {
    let s = getExponent(rand);
    if (s == #fr(0)) null else ?#non_zero(s)
  };
  /// Get public key from sec.
  /// public key (x, y) is an affine point of elliptic curve
  public func getPublicKey(#non_zero(s) : SecretKey) : PublicKey {
    if (s == #fr(0)) Prelude.unreachable(); // type error
    switch (Curve.mul_base(s)) {
      case (#zero) Prelude.unreachable(); // because s is non-zero
      case (#affine(c)) c;
    }
  };
  /// Sign hashed by sec and rand return lower S signature (r, s) such that s < rHalf
  /// hashed : 32-byte SHA-256 value of a message.
  /// rand : 32-byte random value.
  public func signHashed(#non_zero(sec) : SecretKey, hashed : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?Signature {
    if (sec == #fr(0)) Prelude.unreachable(); // type error
    let k = getExponent(rand);
    let x = switch (Curve.mul_base(k)) {
      case (#zero) return null; // k was 0, bad luck with rand
      case (#affine(x, _)) x;
    };
    let r = Fr.fromNat(Fp.toNat(x));
    if (r == #fr(0)) return null; // x was 0 mod r, bad luck with rand
    let z = getExponent(hashed);
    // s = (r * sec + z) / k
    let s = Fr.div(Fr.add(Fr.mul(r, sec), z), k);
    ?normalizeSignature(r,s)
  };
  /// convert a signature to lower S signature
  public func normalizeSignature((r, s) : Signature) : Signature {
    if (Fr.toNat(s) < Curve.params.rHalf) (r, s) else (r, Fr.neg(s))
  };
  /// verify a tuple of pub, hashed, and lowerS sig
  public func verifyHashed(pub : PublicKey, hashed : Iter.Iter<Nat8>, (r,s) : Signature) : Bool {
    if (not Curve.isValid(pub)) return false;
    if (r == #fr(0)) return false;
    if (s == #fr(0)) return false;
    if (Fr.toNat(s) >= Curve.params.rHalf) return false;
    let z = getExponent(hashed);
    let w = Fr.inv(s);
    let u1 = Fr.mul(z, w);
    let u2 = Fr.mul(r, w);
    let R = Curve.add(Curve.mul_base(u1),Curve.mul(#affine(pub),u2));
    return switch (R) {
      case (#zero) false;
      case (#affine(x,_)) Fr.fromNat(Fp.toNat(x)) == r
    };
  };
  /// Sign a message by sec and rand with SHA-256
  public func sign(sec : SecretKey, msg : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?Signature {
    signHashed(sec, sha2(msg).vals(), rand)
  };
  // verify a tuple of pub, msg, and sig
  public func verify(pub : PublicKey, msg : Iter.Iter<Nat8>, sig : Signature) : Bool {
    verifyHashed(pub, sha2(msg).vals(), sig)
  };
  /// return 0x04 + bigEndian(x) + bigEndian(y)
  public func serializePublicKeyUncompressed((x,y) : PublicKey) : Blob {
    let prefix = 0x04 : Nat8;
    let n = 32;
    let x_bytes = Util.toBigEndianPad(n, Fp.toNat(x));
    let y_bytes = Util.toBigEndianPad(n, Fp.toNat(y));
    let ith = func(i : Nat) : Nat8 {
      if (i == 0) {
        prefix
      } else if (i <= n) {
        x_bytes[i - 1]
      } else {
        y_bytes[i - 1 - n]
      }
    };
    let ar = Array.tabulate<Nat8>(1+n*2, ith);
    Blob.fromArray(ar)
  };
  /// return 0x02 + bigEndian(x) if y is even
  /// return 0x03 + bigEndian(x) if y is odd
  public func serializePublicKeyCompressed((x,y) : PublicKey) : Blob {
    let prefix : Nat8 = if ((Fp.toNat(y) % 2) == 0) 0x02 else 0x03;
    let n = 32;
    let x_bytes = Util.toBigEndianPad(n, Fp.toNat(x));
    let ith = func(i : Nat) : Nat8 {
      if (i == 0) {
        prefix
      } else {
        x_bytes[i - 1]
      }
    };
    let ar = Array.tabulate<Nat8>(1+n, ith);
    Blob.fromArray(ar)
  };
  /// Deserialize an uncompressed public key
  public func deserializePublicKeyUncompressed(b : Blob) : ?PublicKey {
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
    if (x >= Curve.params.p) return null;
    if (y >= Curve.params.p) return null;
    let pub = (#fp(x), #fp(y));
    if (not Curve.isValid(pub)) return null;
    return ?pub;
  };
  /// Deserialize a compressed public key.
  public func deserializePublicKeyCompressed(b : Blob) : ?PublicKey {
    let n = 32;
    if (b.size() != n + 1) return null;
    let iter = b.vals();
    let even = switch (iter.next()) {
      case (?0x02) true;
      case (?0x03) false;
      case _ return null;
    };
    let x_ = Util.toNatAsBigEndian(iter);
    if (x_ >= Curve.params.p) return null;
    let x = #fp(x_);
    return switch (Curve.getYfromX(x, even)) {
      case (null) null;
      case (?y) ?(x, y);
    };
  };
  /// serialize to DER format
  /// https://www.oreilly.com/library/view/programming-bitcoin/9781492031482/ch04.html
  public func serializeSignatureDer(sig : Signature) : Blob {
    let buf = Buffer.Buffer<Nat8>(80);
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
    let (#fr(r), #fr(s)) = sig;
    append(r);
    append(s);
    let va = buf.toVarArray();
    va[1] := Nat8.fromNat(va.size()) - 2;
    Blob.fromArrayMut(va)
  };
  /// deserialize DER to signature
  public func deserializeSignatureDer(b : Blob) : ?Signature {
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
    return switch (read(a, 2)) {
      case (null) null;
      case (?(read1, r)) {
        switch (read(a, 2 + read1)) {
          case (null) null;
          case (?(read2, s)) {
            if (a.size() != 2 + read1 + read2) return null;
            ?(#fr(r), #fr(s))
          };
        };
      };
    };
  };
};
