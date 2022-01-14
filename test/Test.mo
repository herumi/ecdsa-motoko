import Int "mo:base/Int";
import M "../src";

let p = M.get_p();

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
    let b = M.Nat_to_reverse_bin(v);
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
  assert(M.Fp().get() == 0);
  var x = M.Fp();
  assert(x.is_zero());
  var v1 = p + 123;
  x.set_nocheck(v1);
  assert(not x.is_zero());
  assert(x.get() == v1);
  x.set(v1);
  assert(x.get() == v1 % p);
};

func arith_test() {
  let m1 = 50000;
  let m2 = 60000;
  var x1 = M.newFp(m1);
  var x2 = M.newFp(m2);
  assert(x1.add(x2).get() == (m1 + m2) % p);
  assert(x1.sub(x2).get() == (m1 + p - m2) % p);
  assert(x2.sub(x1).get() == (m2 - m1) % p);
  assert(M.Fp().neg().is_zero());
  var x3 = M.newFp(m1).neg();
  assert(x3.get() == p - m1);
  x1.set(m1);
  x2.set(m2);
  x3 := x1.mul(x2);
  assert(x3.get() == (m1 * m2) % p);
};

func gcd_test() {
  let (gcd1, gcd2, gcd3) = M.ext_gcd(100, 37);
  assert(gcd1 == 1);
  assert(gcd2 == 10);
  assert(gcd3 == -27);
  let (a, b, c) = M.ext_gcd(0, 37);
  assert(a == 37);
  assert(b == 0);
  assert(c == 1);
};

func inv_test() {
  let inv123 = M.inv_mod(123, 65537);
  assert(inv123 == 14919);
  let x2 = M.newFp(123).inv();
  var i = 1;
  while (i < 20) {
    let x1 = M.newFp(i);
    assert(x1.mul(x1.inv()).get() == 1);
    assert(x2.div(x1).mul(x1).get() == x2.get());
    i += 1;
  };
};

func ec1_test() {
  let P = M.Ec();
  assert(P.is_zero());
  assert(P.neg().is_zero());

  assert(P.add(P).is_zero());

  assert(P.set(M.gx_, M.gy_));
  assert(not P.is_zero());
  let Q = P.neg();
  assert(not Q.is_zero());
  assert(P.get_x() == Q.get_x());
  assert(P.get_y() == M.fp_neg(Q.get_y()));
  assert(P.add(Q).is_zero());
};

func ec2_test() {
  let P = M.Ec();
  let P2 = P.add(P);
  let P3 = P2.add(P);
  let P4 = P3.add(P);
  let P5 = P4.add(P);
  assert(P.add(P.neg()).is_zero());
  assert(P.dbl().equal(P2));
  assert(P.mul(2).equal(P2));
  assert(P.mul(3).equal(P3));
  assert(P.mul(4).equal(P4));
  assert(P.mul(5).equal(P5));
  let Q = P.mul(M.get_r() - 1);
  assert(Q.equal(P.neg()));
  assert(Q.add(P).is_zero());
  assert(P.mul(M.get_r()).is_zero());
};

toBigEndianNatTest();
Nat_to_reverse_bin_test();
cstr_test();
arith_test();
gcd_test();
inv_test();
ec1_test();
ec2_test();
