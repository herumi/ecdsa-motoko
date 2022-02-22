// generated by test/gen.py
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hex "../src/hex";
import Order "mo:base/Order";

module {

public type F = (Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64);
public type Fdbl = (Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64, Nat64);
let p : F = (0xfffffc2f, 0xfffffffe, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff);
public func toNat(x : F) : Nat {
  var v = Nat64.toNat(x.7);
  v := v * 0x100000000 + Nat64.toNat(x.6);
  v := v * 0x100000000 + Nat64.toNat(x.5);
  v := v * 0x100000000 + Nat64.toNat(x.4);
  v := v * 0x100000000 + Nat64.toNat(x.3);
  v := v * 0x100000000 + Nat64.toNat(x.2);
  v := v * 0x100000000 + Nat64.toNat(x.1);
  v := v * 0x100000000 + Nat64.toNat(x.0);
  v
};
public func DtoNat(x : Fdbl) : Nat {
  var v = Nat64.toNat(x.15);
  v := v * 0x100000000 + Nat64.toNat(x.14);
  v := v * 0x100000000 + Nat64.toNat(x.13);
  v := v * 0x100000000 + Nat64.toNat(x.12);
  v := v * 0x100000000 + Nat64.toNat(x.11);
  v := v * 0x100000000 + Nat64.toNat(x.10);
  v := v * 0x100000000 + Nat64.toNat(x.9);
  v := v * 0x100000000 + Nat64.toNat(x.8);
  v := v * 0x100000000 + Nat64.toNat(x.7);
  v := v * 0x100000000 + Nat64.toNat(x.6);
  v := v * 0x100000000 + Nat64.toNat(x.5);
  v := v * 0x100000000 + Nat64.toNat(x.4);
  v := v * 0x100000000 + Nat64.toNat(x.3);
  v := v * 0x100000000 + Nat64.toNat(x.2);
  v := v * 0x100000000 + Nat64.toNat(x.1);
  v := v * 0x100000000 + Nat64.toNat(x.0);
  v
};
public func fromNat(x : Nat) : F {
  var v = x;
  let v0 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v1 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v2 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v3 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v4 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v5 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v6 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  let v7 = Nat64.fromNat(v % 0x100000000);
  v := v / 0x100000000;
  (v0,v1,v2,v3,v4,v5,v6,v7)
};
public func toStr(x : F) : Text {
  var s ="("# Hex.fromNat(Nat64.toNat(x.0));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.1));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.2));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.3));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.4));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.5));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.6));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.7));
  s#")"
};
public func cmp(x : F, y : F) : Order.Order {
  if (x.7 < y.7) return #less;
  if (x.7 > y.7) return #greater;
  if (x.6 < y.6) return #less;
  if (x.6 > y.6) return #greater;
  if (x.5 < y.5) return #less;
  if (x.5 > y.5) return #greater;
  if (x.4 < y.4) return #less;
  if (x.4 > y.4) return #greater;
  if (x.3 < y.3) return #less;
  if (x.3 > y.3) return #greater;
  if (x.2 < y.2) return #less;
  if (x.2 > y.2) return #greater;
  if (x.1 < y.1) return #less;
  if (x.1 > y.1) return #greater;
  if (x.0 < y.0) return #less;
  if (x.0 > y.0) return #greater;
  #equal
};
public func subPre(x : F, y : F) : (F, Nat64) {
  let t0 = x.0 -% y.0;
  let z0 = t0 & 0xffffffff;
  let c0 = t0 >> 63;
  let t1 = x.1 -% y.1 -% c0;
  let z1 = t1 & 0xffffffff;
  let c1 = t1 >> 63;
  let t2 = x.2 -% y.2 -% c1;
  let z2 = t2 & 0xffffffff;
  let c2 = t2 >> 63;
  let t3 = x.3 -% y.3 -% c2;
  let z3 = t3 & 0xffffffff;
  let c3 = t3 >> 63;
  let t4 = x.4 -% y.4 -% c3;
  let z4 = t4 & 0xffffffff;
  let c4 = t4 >> 63;
  let t5 = x.5 -% y.5 -% c4;
  let z5 = t5 & 0xffffffff;
  let c5 = t5 >> 63;
  let t6 = x.6 -% y.6 -% c5;
  let z6 = t6 & 0xffffffff;
  let c6 = t6 >> 63;
  let t7 = x.7 -% y.7 -% c6;
  let z7 = t7 & 0xffffffff;
  let c7 = t7 >> 63;
  ((z0,z1,z2,z3,z4,z5,z6,z7), c7)
};
// ret.7 may has (1<<32) as CF
public func addPre(x : F, y : F) : F {
  let t0 = x.0 +% y.0;
  let z0 = t0 & 0xffffffff;
  let c0 = t0 >> 32;
  let t1 = x.1 +% y.1 +% c0;
  let z1 = t1 & 0xffffffff;
  let c1 = t1 >> 32;
  let t2 = x.2 +% y.2 +% c1;
  let z2 = t2 & 0xffffffff;
  let c2 = t2 >> 32;
  let t3 = x.3 +% y.3 +% c2;
  let z3 = t3 & 0xffffffff;
  let c3 = t3 >> 32;
  let t4 = x.4 +% y.4 +% c3;
  let z4 = t4 & 0xffffffff;
  let c4 = t4 >> 32;
  let t5 = x.5 +% y.5 +% c4;
  let z5 = t5 & 0xffffffff;
  let c5 = t5 >> 32;
  let t6 = x.6 +% y.6 +% c5;
  let z6 = t6 & 0xffffffff;
  let c6 = t6 >> 32;
  let z7 = x.7 +% y.7 +% c6;
  (z0,z1,z2,z3,z4,z5,z6,z7)
};
public func mulPre(x : F, y : F) : Fdbl {
  var t : Nat64 = 0;
  var L : Nat64 = 0;
  var H : Nat64 = 0;
  t := x.0 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z0 = L;
  L := H; H := 0;
  t := x.0 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z1 = L;
  L := H; H := 0;
  t := x.0 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z2 = L;
  L := H; H := 0;
  t := x.0 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z3 = L;
  L := H; H := 0;
  t := x.0 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z4 = L;
  L := H; H := 0;
  t := x.0 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z5 = L;
  L := H; H := 0;
  t := x.0 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z6 = L;
  L := H; H := 0;
  t := x.0 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.1 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.0;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z7 = L;
  L := H; H := 0;
  t := x.1 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z8 = L;
  L := H; H := 0;
  t := x.2 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.3 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z9 = L;
  L := H; H := 0;
  t := x.3 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.4 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.3;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z10 = L;
  L := H; H := 0;
  t := x.4 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.5 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.4;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z11 = L;
  L := H; H := 0;
  t := x.5 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.6 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.5;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z12 = L;
  L := H; H := 0;
  t := x.6 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.7 *% y.6;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z13 = L;
  L := H; H := 0;
  t := x.7 *% y.7;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z14 = L;
  L := H; H := 0;
  let z15 = L;
  (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15)
};
public func normalizeFpDbl(x : Fdbl) : Fdbl {
  let z0 = x.0 & 0xffffffff;
  var t = z0 >> 32;
  t := t +% x.1;
  let z1 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.2;
  let z2 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.3;
  let z3 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.4;
  let z4 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.5;
  let z5 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.6;
  let z6 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.7;
  let z7 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.8;
  let z8 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.9;
  let z9 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.10;
  let z10 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.11;
  let z11 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.12;
  let z12 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.13;
  let z13 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.14;
  let z14 = t & 0xffffffff;
  t := t >> 32;
  t := t +% x.15;
  let z15 = t & 0xffffffff;
  t := t >> 32;
  (z0,z1,z2,z3,z4,z5,z6,z7,z8,z9,z10,z11,z12,z13,z14,z15)
};
// z7 may be >= 2^32
public func mulUnit(x : F, y : Nat64) : F {
  var t : Nat64 = x.0 *% y;
  let z0 = t & 0xffffffff;
  t := x.1 *% y +% (t >> 32);
  let z1 = t & 0xffffffff;
  t := x.2 *% y +% (t >> 32);
  let z2 = t & 0xffffffff;
  t := x.3 *% y +% (t >> 32);
  let z3 = t & 0xffffffff;
  t := x.4 *% y +% (t >> 32);
  let z4 = t & 0xffffffff;
  t := x.5 *% y +% (t >> 32);
  let z5 = t & 0xffffffff;
  t := x.6 *% y +% (t >> 32);
  let z6 = t & 0xffffffff;
  t := x.7 *% y +% (t >> 32);
  let z7 = t;
  (z0,z1,z2,z3,z4,z5,z6,z7)
};
public func addMulUnit((x, xH) : (F, Nat64), y : F, z : Nat64) : (F, Nat64) {
  var a = y.0 *% z;
  var b : Nat64 = 0;
  var t = x.0 +% (a & 0xffffffff);
  let w0 = t & 0xffffffff;
  b := y.1 *% z;
  t := (t >> 32) +% x.1 +% (b & 0xffffffff) +% (a >> 32);
  let w1 = t & 0xffffffff;
  a := y.2 *% z;
  t := (t >> 32) +% x.2 +% (a & 0xffffffff) +% (b >> 32);
  let w2 = t & 0xffffffff;
  b := y.3 *% z;
  t := (t >> 32) +% x.3 +% (b & 0xffffffff) +% (a >> 32);
  let w3 = t & 0xffffffff;
  a := y.4 *% z;
  t := (t >> 32) +% x.4 +% (a & 0xffffffff) +% (b >> 32);
  let w4 = t & 0xffffffff;
  b := y.5 *% z;
  t := (t >> 32) +% x.5 +% (b & 0xffffffff) +% (a >> 32);
  let w5 = t & 0xffffffff;
  a := y.6 *% z;
  t := (t >> 32) +% x.6 +% (a & 0xffffffff) +% (b >> 32);
  let w6 = t & 0xffffffff;
  b := y.7 *% z;
  t := (t >> 32) +% x.7 +% (b & 0xffffffff) +% (a >> 32);
  let w7 = t & 0xffffffff;
  ((w0,w1,w2,w3,w4,w5,w6,w7), xH +% (t >> 32) +% (b >> 32))
};
public func modp(x : Fdbl) : F {
  let a : Nat64 = 0x3d1;
  let H = (x.8, x.9, x.10, x.11, x.12, x.13, x.14, x.15);
  // ((H << 32) + L) + H * a
  let (Lt, Ht) = addMulUnit(((x.0, x.1+%x.8, x.2+%x.9, x.3+%x.10, x.4+%x.11, x.5+%x.12, x.6+%x.13, x.7+%x.14), x.15), H, a);
  let HtL = Ht & 0xffffffff;
  let HtH = Ht >> 32;
  let HtLa = HtL *% a;
  let HtHa = HtH *% a;
  var t = Lt.0 +% HtLa;
  let z0 = t & 0xffffffff;
  t := HtL +% Lt.1 +% HtHa +% (t >> 32);
  let z1 = t & 0xffffffff;
  t := HtH +% Lt.2 +% (t >> 32);
  let z2 = t & 0xffffffff;
  t := Lt.3 +% (t >> 32);
  let z3 = t & 0xffffffff;
  t := Lt.4 +% (t >> 32);
  let z4 = t & 0xffffffff;
  t := Lt.5 +% (t >> 32);
  let z5 = t & 0xffffffff;
  t := Lt.6 +% (t >> 32);
  let z6 = t & 0xffffffff;
  let z7 = Lt.7 +% (t >> 32);
  (z0,z1,z2,z3,z4,z5,z6,z7)
};
public func add(x : F, y : F) : F {
  let z = addPre(x,y);
  if (cmp(z, p) != #less) subPre(z, p).0 else z
};
  public func sub(x : F, y : F) : F {
  let (z, CF) = subPre(x,y);
  if (CF == 0) z else addPre(z, p)
};
  public func mul(x : F, y : F) : F {
    modp(mulPre(x, y))
};

};
