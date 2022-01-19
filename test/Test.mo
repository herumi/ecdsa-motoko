import M "../src";
import Nat "mo:base/Nat";
import FP "../src/fp";

let p = M.p();

func optionFunc(v:Nat) : ?Nat {
  if (v == 0) return null;
  ?v
};

func toReverseBinTest() {
  let tbl = [
    (0, []:[Bool]),
    (1, [true]),
    (2, [false, true]),
  ];
  for(i in tbl.keys()) {
    let (v, a) = tbl[i];
    let b = M.toReverseBin(v);
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

func cstrTest() {
  assert(FP.Fp().val() == 0);
  var x = FP.Fp();
  assert(x.isZero());
  var v1 = p + 123;
  x.setNoCheck(v1);
  assert(not x.isZero());
  assert(x.val() == v1);
  x.set(v1);
  assert(x.val() == v1 % p);
};

func arithTest() {
  let m1 = 50000;
  let m2 = 60000;
  var x1 = FP.newFp(m1);
  var x2 = FP.newFp(m2);
  assert(x1.add(x2).val() == (m1 + m2) % p);
  assert(x1.sub(x2).val() == (m1 + p - m2 : Nat) % p);
  assert(x2.sub(x1).val() == (m2 - m1 : Nat) % p);
  assert(FP.Fp().neg().isZero());
  var x3 = FP.newFp(m1).neg();
  assert(x3.val() == (p - m1 : Nat));
  x1.set(m1);
  x2.set(m2);
  x3 := x1.mul(x2);
  assert(x3.val() == (m1 * m2) % p);
};

func invTest() {
  let inv123 = FP.invMod(123, 65537);
  assert(inv123 == 14919);
  let x2 = FP.newFp(123).inv();
  var i = 1;
  while (i < 20) {
    let x1 = FP.newFp(i);
    assert(x1.mul(x1.inv()).val() == 1);
    assert(x2.div(x1).mul(x1).val() == x2.val());
    i += 1;
  };
};

func gcdTest() {
  let (gcd1, gcd2, gcd3) = M.extGcd(100, 37);
  assert(gcd1 == 1);
  assert(gcd2 == 10);
  assert(gcd3 == -27);
  let (a, b, c) = M.extGcd(0, 37);
  assert(a == 37);
  assert(b == 0);
  assert(c == 1);
};

func ec1Test() {
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

func ec2Teset() {
  let okP = (0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798, 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8);
  let okP2 = (0xc6047f9441ed7d6d3045406e95c07cd85c778e4b8cef3ca7abac09b95c709ee5, 0x1ae168fea63dc339a3c58419466ceaeef7f632653266d0e1236431a950cfe52a);
  let okP3 = (0xf9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9, 0x388f7b0f632de8140fe337e62a37f3566500a99934c2231b6cb9fd7584b8e672);

  let P = M.newEcGenerator();
  assert(P.x() == okP.0);
  assert(P.y() == okP.1);
  let P2 = P.add(P);
  assert(P2.x() == okP2.0);
  assert(P2.y() == okP2.1);
  let P3 = P2.add(P);
  assert(P3.x() == okP3.0);
  assert(P3.y() == okP3.1);
  let P4 = P3.add(P);
  let P5 = P4.add(P);
  assert(P.add(P.neg()).isZero());
  assert(P.dbl().equal(P2));
  assert(P.mul(1).equal(P));
  assert(P.mul(2).equal(P2));
  assert(P.mul(3).equal(P3));
  assert(P.mul(4).equal(P4));
  assert(P.mul(5).equal(P5));
  let Q = P.mul(M.r() - 1);
  assert(Q.equal(P.neg()));
  assert(Q.add(P).isZero());
  assert(P.mul(M.r()).isZero());
};

func getPair(v : ?(Nat, Nat)) : (Nat, Nat) {
  // QQQ : How can I write better?
  switch (v) {
    case(null) { (0, 0) };
    case(?x) { x };
  }
};

func ecdsaTest() {
  let secRand : [Nat8] = [ 0x83, 0xec, 0xb3, 0x98, 0x4a, 0x4f, 0x9f, 0xf0, 0x3e, 0x84, 0xd5, 0xf9, 0xc0, 0xd7, 0xf8, 0x88, 0xa8, 0x18, 0x33, 0x64, 0x30, 0x47, 0xac, 0xc5, 0x8e, 0xb6, 0x43, 0x1e, 0x01, 0xd9, 0xba, 0xc8 ];

  let signRand : [Nat8] = [ 0x8a, 0xfa, 0x4a, 0x16, 0x2b, 0x7b, 0xad, 0x6c, 0x92, 0xff, 0x14, 0xf3, 0xa8, 0xbf, 0x4d, 0xb0, 0xf3, 0xc3, 0x9e, 0x90, 0xc0, 0x6f, 0x93, 0x78, 0x61, 0xf8, 0x23, 0xd2, 0x99, 0x5c, 0x74, 0xf0 ];
  let hello : [Nat8] = [ 0x68, 0x65, 0x6c, 0x6c, 0x6f ];
  // sha256('hello')
  let hashed : [Nat8] = [ 0x2c, 0xf2, 0x4d, 0xba, 0x5f, 0xb0, 0xa3, 0x0e, 0x26, 0xe8, 0x3b, 0x2a, 0xc5, 0xb9, 0xe2, 0x9e, 0x1b, 0x16, 0x1e, 0x5c, 0x1f, 0xa7, 0x42, 0x5e, 0x73, 0x04, 0x33, 0x62, 0x93, 0x8b, 0x98, 0x24 ];
  assert(M.test_sha2(hello) == hashed);

  var sec = switch (M.getSecretKey(secRand)) {
    case(null) { 0 };
    case(?v) { v };
  };
  assert(sec == 0x83ecb3984a4f9ff03e84d5f9c0d7f888a81833643047acc58eb6431e01d9bac8);
  var pub = getPair(M.getPublicKey(sec));
  assert(pub == (0x653bd02ba1367e5d4cd695b6f857d1cd90d4d8d42bc155d85377b7d2d0ed2e71, 0x04e8f5da403ab78decec1f19e2396739ea544e2b14159beb5091b30b418b813a));
  var sig = getPair(M.signHashed(sec, hashed, signRand));
  assert(M.verifyHashed(pub, hashed, sig));
  assert(not M.verifyHashed((pub.0, pub.1 + 1), hashed, sig));
  assert(not M.verifyHashed((pub.0, pub.1 + 1), hashed, sig));
  assert(not M.verifyHashed(pub, [0x1, 0x2], sig));
  sig := (0xa598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5, 0xde5d79a2ba44e311d04fdca263639283965780bce9169822be9cc81756e95a24);
  assert(M.verifyHashed(pub, hashed, sig));

  // generated values by Python:ecdsa
  sec := 0xb1aa6282b14e5ffbf6d12f783612f804e6a20d1a9734ffbb6c9923c670ee8da2;
  pub := getPair(M.getPublicKey(sec));
  assert(pub == (0x0a09ff142d94bc3f56c5c81b75ea3b06b082c5263fbb5bd88c619fc6393dda3d, 0xa53e0e930892cdb7799eea8fd45b9fff377d838f4106454289ae8a080b111f8d));
  sig := (0x50839a97404c24ec39455b996e4888477fd61bcf0ffb960c7ffa3bef10450191, 0x9671b8315bb5c1611d422d49cbbe7e80c6b463215bfad1c16ca73172155bf31a);
  assert(M.verifyHashed(pub, hashed, sig));
};

toBigEndianNatTest();
toReverseBinTest();

// test fp.mo
cstrTest();
arithTest();
invTest();

// test lib.mo
gcdTest();
ec1Test();
ec2Teset();
ecdsaTest();
