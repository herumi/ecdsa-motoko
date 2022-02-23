// check the speed of mulMod
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import F "../src/fp";

let p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f;

let C = 100000;
// 0.47 sec
func mulMod(x : Nat, y : Nat) : Nat {
  (x * y) % p
};

let M = 0x10000000000000000000000000000000000000000000000000000000000000000;
let a = 0x1000003d1;

let gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798;
let gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8;

// 0.77 sec
func mulMod2(x : Nat, y : Nat) : Nat {
  var t = x * y;
  var H = t / M;
  var L = t - H * M : Nat;
  t := H * a + L;
  H := t / M;
  L := t - H * M : Nat;
  t := H * a + L;
  if (t > M) t - M + 1 else t
};

func test1() {
  var i : Nat = 0;
  var x = gx;
  let y = gy;
  while (i < 100000) {
    let x1 = mulMod(x, y);
    x := x1;
    i += 1;
  };
  Debug.print("test1 x=" # Nat.toText(x));
};

func test2() {
  var i : Nat = 0;
  var x = F.fromNat(gx);
  let y = F.fromNat(gy);
  while (i < 100000) {
    let x1 = F.mul(x, y);
    x := x1;
    i += 1;
  };
  Debug.print("test1 x=" # Nat.toText(F.toNat(x)));
};

//test1();
test2();

