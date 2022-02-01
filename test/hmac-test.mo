import Hmac "../src/hmac";
import M "../src";
import Util "../src/util";
import Debug "mo:base/Debug";
import Blob "mo:base/Blob";

let tbl = [
  (
    0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b,
    0x4869205468657265,
    0xb0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7,
  ),
  (
    0x4a656665,
    0x7768617420646f2079612077616e7420666f72206e6f7468696e673f,
    0x5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843,
  ),
  (
    0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
    0xdddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd,
    0x773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe,
  ),
  (
    0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
    0x54657374205573696e67204c6172676572205468616e20426c6f636b2d53697a65204b6579202d2048617368204b6579204669727374,
    0x60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54,
  ),
  (
    0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
    0x5468697320697320612074657374207573696e672061206c6172676572207468616e20626c6f636b2d73697a65206b657920616e642061206c6172676572207468616e20626c6f636b2d73697a6520646174612e20546865206b6579206e6565647320746f20626520686173686564206265666f7265206265696e6720757365642062792074686520484d414320616c676f726974686d2e,
    0x9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2,
  ),
  (
    0x01010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101,
    0x12,
    0x9fc5fd7acf75bf2125220240293bd8221d72a25ffb5bfb397ee1a2a00df7a1ad,
  ),
  (
    0x0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101,
    0x12,
    0x4a8ac5b5f14061a2ed19ea9ac716b3c2c27343ac4dc52e42fabb9b1ab019d335,
  ),
  (
    0x010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101,
    0x12,
    0xe4ab292c53a535617d5d5a80a74de6e5e2516faba8708ac536a5bb79c7c8e989,
  ),
];

for (e in tbl.vals()) {
  let key = Util.toBigEndian(e.0);
  let msg = Util.toBigEndian(e.1);
  let md = Util.toBigEndian(e.2);
  let ret = Hmac.hmac256(key, msg.vals());
  assert(md == Blob.toArray(ret));
};

do {
  let md = Util.toBigEndian(0xb613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad);
  let ret = Hmac.hmac256([], [].vals());
  assert(md == Blob.toArray(ret));
};
