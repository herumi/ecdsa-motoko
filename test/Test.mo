import M "../src";
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";

let p = M.p();

func optionFunc(v:Nat) : ?Nat {
  if (v == 0) return null;
  ?v
};

func Nat_to_reverse_bin_test() {
  let tbl = [
    (0, []:[Bool]),
    (1, [true]),
    (2, [false, true]),
  ];
  for(i in tbl.keys()) {
    let (v, a) = tbl[i];
    let b = M.fromNatToReverseBin(v);
    assert(b == a);
  };
  switch (optionFunc(5)) {
    case(null) { assert(false); };
    case(?v) { assert(v == 5); };
  };
};

func toBigEndianNatTest() {
  let tbl = [
    ([0x12, 0x34]:[Nat8], 0x1234),
  ];
  for (i in tbl.keys()) {
    let (b, v) = tbl[i];
    assert(M.toBigEndianNat(b) == v);
  };
};

func cstr_test() {
  assert(M.Fp().val() == 0);
  var x = M.Fp();
  assert(x.isZero());
  var v1 = p + 123;
  x.setNoCheck(v1);
  assert(not x.isZero());
  assert(x.val() == v1);
  x.set(v1);
  assert(x.val() == v1 % p);
};

func arith_test() {
  let m1 = 50000;
  let m2 = 60000;
  var x1 = M.newFp(m1);
  var x2 = M.newFp(m2);
  assert(x1.add(x2).val() == (m1 + m2) % p);
  assert(x1.sub(x2).val() == (m1 + p - m2) % p);
  assert(x2.sub(x1).val() == (m2 - m1) % p);
  assert(M.Fp().neg().isZero());
  var x3 = M.newFp(m1).neg();
  assert(x3.val() == p - m1);
  x1.set(m1);
  x2.set(m2);
  x3 := x1.mul(x2);
  assert(x3.val() == (m1 * m2) % p);
};

func gcd_test() {
  let (gcd1, gcd2, gcd3) = M.extGcd(100, 37);
  assert(gcd1 == 1);
  assert(gcd2 == 10);
  assert(gcd3 == -27);
  let (a, b, c) = M.extGcd(0, 37);
  assert(a == 37);
  assert(b == 0);
  assert(c == 1);
};

func inv_test() {
  let inv123 = M.invMod(123, 65537);
  assert(inv123 == 14919);
  let x2 = M.newFp(123).inv();
  var i = 1;
  while (i < 20) {
    let x1 = M.newFp(i);
    assert(x1.mul(x1.inv()).val() == 1);
    assert(x2.div(x1).mul(x1).val() == x2.val());
    i += 1;
  };
};

func ec1_test() {
  let P = M.Ec();
  assert(P.isZero());
  assert(P.neg().isZero());

  assert(P.add(P).isZero());

  assert(P.set(M.gx_, M.gy_));
  assert(not P.isZero());
  let Q = P.neg();
  assert(not Q.isZero());
  assert(P.x() == Q.x());
  assert(P.y() == M.fpNeg(Q.y()));
  assert(P.add(Q).isZero());
};

func ec2_test() {
  let P = M.Ec();
  let P2 = P.add(P);
  let P3 = P2.add(P);
  let P4 = P3.add(P);
  let P5 = P4.add(P);
  assert(P.add(P.neg()).isZero());
  assert(P.dbl().equal(P2));
  assert(P.mul(2).equal(P2));
  assert(P.mul(3).equal(P3));
  assert(P.mul(4).equal(P4));
  assert(P.mul(5).equal(P5));
  let Q = P.mul(M.r() - 1);
  assert(Q.equal(P.neg()));
  assert(Q.add(P).isZero());
  assert(P.mul(M.r()).isZero());
};

func ecdsa_test() {
  let secRand : [Nat8] = [ 0x83, 0xec, 0xb3, 0x98, 0x4a, 0x4f, 0x9f, 0xf0, 0x3e, 0x84, 0xd5, 0xf9, 0xc0, 0xd7, 0xf8, 0x88, 0xa8, 0x18, 0x33, 0x64, 0x30, 0x47, 0xac, 0xc5, 0x8e, 0xb6, 0x43, 0x1e, 0x01, 0xd9, 0xba, 0xc8 ];

  let pubx = 0x653bd02ba1367e5d4cd695b6f857d1cd90d4d8d42bc155d85377b7d2d0ed2e71;
  let puby = 0x04e8f5da403ab78decec1f19e2396739ea544e2b14159beb5091b30b418b813a;
  let SignRand : [Nat8] = [ 0x8a, 0xfa, 0x4a, 0x16, 0x2b, 0x7b, 0xad, 0x6c, 0x92, 0xff, 0x14, 0xf3, 0xa8, 0xbf, 0x4d, 0xb0, 0xf3, 0xc3, 0x9e, 0x90, 0xc0, 0x6f, 0x93, 0x78, 0x61, 0xf8, 0x23, 0xd2, 0x99, 0x5c, 0x74, 0xf0 ];
  // sha256('hello')
  let hashed : [Nat8] = [ 0x2c, 0xf2, 0x4d, 0xba, 0x5f, 0xb0, 0xa3, 0x0e, 0x26, 0xe8, 0x3b, 0x2a, 0xc5, 0xb9, 0xe2, 0x9e, 0x1b, 0x16, 0x1e, 0x5c, 0x1f, 0xa7, 0x42, 0x5e, 0x73, 0x04, 0x33, 0x62, 0x93, 0x8b, 0x98, 0x24 ];

  let sec = switch (M.getSecretKey(secRand)) {
    case(null) { 0 };
    case(?v) { v };
  };
  assert(sec == 0x83ecb3984a4f9ff03e84d5f9c0d7f888a81833643047acc58eb6431e01d9bac8);
  let (x, y) = switch (M.getPublicKey(sec)) {
    case(null) { (0, 0) };
    case(?v) { v };
  };
  Debug.print("x=" # Nat.toText(x));
//  assert(x == pubx);
//  assert(y == puby);

};

toBigEndianNatTest();
Nat_to_reverse_bin_test();
cstr_test();
arith_test();
gcd_test();
inv_test();
ec1_test();
ec2_test();
ecdsa_test();
