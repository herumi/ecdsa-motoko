[![.github/workflows/ci.yml](https://github.com/herumi/ecdsa-motoko/actions/workflows/ci.yml/badge.svg)](https://github.com/herumi/ecdsa-motoko/actions/workflows/ci.yml)

# ECDSA for Motoko

Under construction

# API

- `getSecretKey(rand : Iter.Iter<Nat8>) : ?Nat`
  - return a secret key from random data `rand`.
- `getPublicKey(sec : Nat) : ?(Nat, Nat)`
  - return a public key `(x, y)` from a secret key `sec`.
- `signHashed(sec : Nat, hashed : Iter.Iter<Nat8>, rand : Iter.Iter<Nat8>) : ?(Nat, Nat)`
  - return a signature `(r, s)` from a secret key `sec`, a hashed value `hashed`, and a random data `rand`.
- `verifyHashed(pub : (Nat, Nat), hashed : Iter.Iter<Nat8>, sig : (Nat, Nat)) : Bool`
  - return true if a tuple of a public key `pub` and a hashed data `hashed`, and a signature `sig` is valid.
- `serializeUncompressed(pub : (Nat, Nat)) : [Nat8]`
  - return an uncompressed serialized data such as `0x04 + x + y` of a public key `pub`.
- `serializeCompressed(pub : (Nat, Nat)) : [Nat8] `
  - return a compressed serialized data such as `0x02(or 0x03) + x` of a public key `pub`.

# License

Apache 2.0 with LLVM Exception

# Author

MITSUNARI Shigeo(herumi@nifty.com)

# Sponsors welcome
[GitHub Sponsor](https://github.com/sponsors/herumi)
