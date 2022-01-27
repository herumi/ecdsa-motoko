import M "../src";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Blob "mo:base/Blob";
import Iter "mo:base/Iter";

let p = M.p();

func consumeIter(iter : Iter.Iter<Nat>, expect : [Nat]) {
  assert(iter.next() == ?expect[0]);
  assert(iter.next() == ?expect[1]);
};

func iterTest() {
  let a = [1, 2, 3, 4, 5];
  let b = a.vals();
  let c = b;
  let d = a.vals();
  consumeIter(b, [1, 2]);
  consumeIter(c, [3, 4]);
  consumeIter(d, [1, 2]);
};

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

func toNatAsBigEndianTest() {
  let tbl = [
    ([] : [Nat8], 0x0),
    ([0x12] : [Nat8], 0x12),
    ([0x12, 0x34] : [Nat8], 0x1234),
    ([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0] : [Nat8], 0x123456789abcdef0),
    ([0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 0x12] : [Nat8], 0x123456789abcdef012),
  ];
  for (i in tbl.keys()) {
    let (b, v) = tbl[i];
    assert(M.toNatAsBigEndian(b.vals()) == v);
    assert(M.fromNatToBigEndian(b.size(), v) == b);
  };
  assert(M.fromNatToBigEndian(1, 0) == ([0x00] : [Nat8]));
  assert(M.fromNatToBigEndian(5, 0x1234) == ([0x00, 0x00, 0x00, 0x12, 0x34] : [Nat8]));
};

func arithTest() {
  let m1 = 5 * 2 ** 128;
  let m2 = 6 * 2 ** 128;
  var x1 = m1 % p;
  var x2 = m2 % p;
  assert(M.fpAdd(x1, x2) == (m1 + m2) % p);
  assert(M.fpSub(x1, x2) == (m1 + p - m2 : Nat) % p);
  assert(M.fpSub(x2, x1) == (m2 - m1 : Nat) % p);
  assert(M.fpNeg(0) == 0);
  assert(M.fpNeg(x1) == (p - m1 : Nat));
  assert(M.fpMul(x1, x2) == (m1 * m2) % p);

  var i = 0;
  x2 := 1;
  while (i < 30) {
    assert(x2 == M.fpPow(x1, i));
    x2 := M.fpMul(x2, x1);
    i += 1;
  };
};

func invTest() {
  let inv123 = M.invMod(123, 65537);
  assert(inv123 == 14919);
  let x2 = M.fpInv(123);
  var x1 = 1;
  while (x1 < 20) {
    assert(M.fpMul(x1, M.fpInv(x1)) == 1);
    assert(M.fpMul(M.fpDiv(x2, x1), x1) == x2);
    x1 += 1;
  };
};

func sqrRootTest() {
  var i = 0;
  while (i < 30) {
//    Debug.print("i=" # M.toHex(i));
    switch (M.fpSqrRoot(i)) {
      case (null) { };
      case (?sq) {
//        Debug.print("sq=" # M.toHex(sq));
        assert(M.fpSqr(sq) == i);
      };
    };
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

  let (x, y) = M.generator();
  assert(P.set(x, y));
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

func ecdsaTest() {
  let secRand : [Nat8] = [ 0x83, 0xec, 0xb3, 0x98, 0x4a, 0x4f, 0x9f, 0xf0, 0x3e, 0x84, 0xd5, 0xf9, 0xc0, 0xd7, 0xf8, 0x88, 0xa8, 0x18, 0x33, 0x64, 0x30, 0x47, 0xac, 0xc5, 0x8e, 0xb6, 0x43, 0x1e, 0x01, 0xd9, 0xba, 0xc8 ];

  let signRand : [Nat8] = [ 0x8a, 0xfa, 0x4a, 0x16, 0x2b, 0x7b, 0xad, 0x6c, 0x92, 0xff, 0x14, 0xf3, 0xa8, 0xbf, 0x4d, 0xb0, 0xf3, 0xc3, 0x9e, 0x90, 0xc0, 0x6f, 0x93, 0x78, 0x61, 0xf8, 0x23, 0xd2, 0x99, 0x5c, 0x74, 0xf0 ];
  let hello : [Nat8] = [ 0x68, 0x65, 0x6c, 0x6c, 0x6f ];
  // sha256('hello')
  let hashed : [Nat8] = [ 0x2c, 0xf2, 0x4d, 0xba, 0x5f, 0xb0, 0xa3, 0x0e, 0x26, 0xe8, 0x3b, 0x2a, 0xc5, 0xb9, 0xe2, 0x9e, 0x1b, 0x16, 0x1e, 0x5c, 0x1f, 0xa7, 0x42, 0x5e, 0x73, 0x04, 0x33, 0x62, 0x93, 0x8b, 0x98, 0x24 ];
  assert(M.test_sha2(hello) == hashed);

  var sec = switch (M.getSecretKey(secRand.vals())) {
    case(null) { 0 };
    case(?v) { v };
  };
  assert(sec == 0x83ecb3984a4f9ff03e84d5f9c0d7f888a81833643047acc58eb6431e01d9bac8);
  var pub = Option.get(M.getPublicKey(sec), (0, 0));
  assert(pub == (0x653bd02ba1367e5d4cd695b6f857d1cd90d4d8d42bc155d85377b7d2d0ed2e71, 0x04e8f5da403ab78decec1f19e2396739ea544e2b14159beb5091b30b418b813a));
  var sig = Option.get(M.signHashed(sec, hashed.vals(), signRand.vals()), (0, 0));
  assert(M.verifyHashed(pub, hashed.vals(), sig));
  assert(not M.verifyHashed((pub.0, pub.1 + 1), hashed.vals(), sig));
  assert(not M.verifyHashed((pub.0, pub.1 + 1), hashed.vals(), sig));
  assert(not M.verifyHashed(pub, ([0x1, 0x2] : [Nat8]).vals(), sig));
  sig := (0xa598a8030da6d86c6bc7f2f5144ea549d28211ea58faa70ebf4c1e665c1fe9b5, 0xde5d79a2ba44e311d04fdca263639283965780bce9169822be9cc81756e95a24);
  assert(M.verifyHashed(pub, hashed.vals(), sig));

  // generated values by Python:ecdsa
  sec := 0xb1aa6282b14e5ffbf6d12f783612f804e6a20d1a9734ffbb6c9923c670ee8da2;
  pub := Option.get(M.getPublicKey(sec), (0, 0));
  assert(pub == (0x0a09ff142d94bc3f56c5c81b75ea3b06b082c5263fbb5bd88c619fc6393dda3d, 0xa53e0e930892cdb7799eea8fd45b9fff377d838f4106454289ae8a080b111f8d));
  sig := (0x50839a97404c24ec39455b996e4888477fd61bcf0ffb960c7ffa3bef10450191, 0x9671b8315bb5c1611d422d49cbbe7e80c6b463215bfad1c16ca73172155bf31a);
  assert(M.verifyHashed(pub, hashed.vals(), sig));
};

func serializeTest() {
  let expected = Blob.fromArray([0x04,0xa,0x9,0xff,0x14,0x2d,0x94,0xbc,0x3f,0x56,0xc5,0xc8,0x1b,0x75,0xea,0x3b,0x6,0xb0,0x82,0xc5,0x26,0x3f,0xbb,0x5b,0xd8,0x8c,0x61,0x9f,0xc6,0x39,0x3d,0xda,0x3d,0xa5,0x3e,0xe,0x93,0x8,0x92,0xcd,0xb7,0x79,0x9e,0xea,0x8f,0xd4,0x5b,0x9f,0xff,0x37,0x7d,0x83,0x8f,0x41,0x6,0x45,0x42,0x89,0xae,0x8a,0x8,0xb,0x11,0x1f,0x8d]);
  let pub = (0x0a09ff142d94bc3f56c5c81b75ea3b06b082c5263fbb5bd88c619fc6393dda3d, 0xa53e0e930892cdb7799eea8fd45b9fff377d838f4106454289ae8a080b111f8d);
  var v = M.serializeUncompressed(pub);
  assert(v == expected);
  v := M.serializeCompressed(pub);
  let (x, y) = Option.get(M.deserializeCompressed(v), (0, 0));
  assert((x, y) == pub);
  let pub2 = (x, M.fpNeg(y));
  v := M.serializeCompressed(pub2);
  let pub3 = Option.get(M.deserializeCompressed(v), (0, 0));
  assert(pub3 == pub2);
};

toNatAsBigEndianTest();
toReverseBinTest();
iterTest();

// test lib.mo
arithTest();
invTest();
sqrRootTest();
gcdTest();
ec1Test();
ec2Teset();
ecdsaTest();
serializeTest();
