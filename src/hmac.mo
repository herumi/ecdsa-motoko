/**
 * Module      : hmac.mo
 * Description : HMAC-SHA-256
 * Copyright   : 2022 Mitsunari Shigeo
 * License     : Apache 2.0 with LLVM Exception
 * Maintainer  : herumi <herumi@nifty.com>
 * Stability   : Stable
 */

import Iter "mo:base/Iter";
import Nat8 "mo:base/Nat8";
import Buffer "mo:base/Buffer";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import SHA2 "mo:sha2";
import M "../src";

module {
  public func hmac256(key : [Nat8], msg : Iter.Iter<Nat8>) : Blob {
    let ipad : Nat8 = 0x36;
    let opad : Nat8 = 0x5c;
    var k : [var Nat8] = Array.init<Nat8>(64, 0);
    var keySize = key.size();
    if (keySize > 64) {
      let md = Blob.toArray(SHA2.fromIter(#sha256, key.vals()));
      var i = 0;
      keySize := 32;
      while (i < keySize) {
        k[i] := md[i];
        i += 1;
      };
    } else {
      var i = 0;
      while (i < keySize) {
        k[i] := key[i];
        i += 1;
      };
    };
    var i = 0;
    while (i < 64) {
      k[i] ^= ipad;
      i += 1;
    };
    class k_and_msg(k : [var Nat8], msg : Iter.Iter<Nat8>) {
      var i = 0;
      public func next() : ?Nat8 {
        if (i < 64) {
          let ret = ?k[i];
          i += 1;
          ret
        } else {
          msg.next()
        };
      };
    };
    let hmac = Blob.toArray(SHA2.fromIter(#sha256, k_and_msg(k, msg)));
    i := 0;
    while (i < 64) {
      k[i] ^= ipad ^ opad;
      i += 1;
    };
    let ith = func(i : Nat) : Nat8 {
      if (i < 64) k[i] else hmac[i - 64];
    };
    let cat = Array.tabulate<Nat8>(96, ith);
    SHA2.fromIter(#sha256, cat.vals())
  };
};
