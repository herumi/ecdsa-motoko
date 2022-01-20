import M "../src";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";

func benchGen(n : Nat32) {
  let sec = 0x83ecb3984a4f9ff03e84d5f9c0d7f888a81833643047acc58eb6431e01d9bac8;
  var i:Nat32 = 0;
  while (i < n) {
    let pub = M.getPublicKey(sec);
    i := i + 1;
  };
};

Debug.print("start");
benchGen(50);
Debug.print("end");
