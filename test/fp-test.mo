import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hex "../src/hex";
import Order "mo:base/Order";
import Fp "../src/fp";


let a : Fp.F = (5,2,3,9,9,0xffffffff,0x77777777,0x55555555);
let b : Fp.F = (1,2,3,4,5,6,7,8);
let c = Fp.sub(a, b);
let d = Fp.add(c, b);
Debug.print("a=" # Nat.toText(Fp.toNat(a)));
Debug.print("b=" # Nat.toText(Fp.toNat(b)));
Debug.print("c=" # Fp.toStr(c));
Debug.print("d=" # Fp.toStr(d));
Debug.print("d=" # Nat.toText(Fp.toNat(d)));
Debug.print("e=" # Fp.toStr(Fp.fromNat(Fp.toNat(d))));
let x = Fp.mulPre(a,b);
Debug.print("x=" # Nat.toText(Fp.DtoNat(x)));
let x0 = Fp.normalizeFpDbl(x);
Debug.print("x=" # Nat.toText(Fp.DtoNat(x0)));
Debug.print("x0=" # Nat.toText(Nat64.toNat(x.1)));
Debug.print("x0=" # Nat.toText(Nat64.toNat(x0.1)));

