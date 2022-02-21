// generated by test/gen.py
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hex "../src/hex";
import Order "mo:base/Order";

module {

public type F = (Nat64, Nat64, Nat64);
public type Fdbl = (Nat64, Nat64, Nat64, Nat64, Nat64, Nat64);
let p : F = (0xfffffc2f, 0xfffffffe, 0xffffffff);
public func toNat(x : F) : Nat {
  var v = Nat64.toNat(x.2);
  v := v * 0x100000000 + Nat64.toNat(x.1);
  v := v * 0x100000000 + Nat64.toNat(x.0);
  v
};
public func DtoNat(x : Fdbl) : Nat {
  var v = Nat64.toNat(x.5);
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
  (v0,v1,v2)
};
public func toStr(x : F) : Text {
  var s ="("# Hex.fromNat(Nat64.toNat(x.0));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.1));
  s := s #"," # Hex.fromNat(Nat64.toNat(x.2));
  s#")"
};
public func cmp(x : F, y : F) : Order.Order {
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
  ((z0,z1,z2), c2)
};
// ret.2 may has (1<<32) as CF
public func addPre(x : F, y : F) : F {
  let t0 = x.0 +% y.0;
  let z0 = t0 & 0xffffffff;
  let c0 = t0 >> 32;
  let t1 = x.1 +% y.1 +% c0;
  let z1 = t1 & 0xffffffff;
  let c1 = t1 >> 32;
  let z2 = x.2 +% y.2 +% c1;
  (z0,z1,z2)
};
public func add(x : F, y : F) : F {
  let z = addPre(x,y);
  if (cmp(z, p) != #less) subPre(z, p).0 else z
};
public func sub(x : F, y : F) : F {
  let (z, CF) = subPre(x,y);
  if (CF == 0) z else addPre(z, p)
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
  t := x.1 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  t := x.2 *% y.1;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z3 = L;
  L := H; H := 0;
  t := x.2 *% y.2;
  L := L +% (t & 0xffffffff);
  H := H +% (t >> 32);
  let z4 = L;
  L := H; H := 0;
  let z5 = L;
  (z0,z1,z2,z3,z4,z5)
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
  (z0,z1,z2,z3,z4,z5)
};
};
