N = 8

def pack(v, n = N):
	s = ''
	for i in range(n):
		if i == 0:
			s = f'({v}0'
		else:
			s += f',{v}{i}'
	return s + ")"

def toStr():
	print('public func toStr(x : F) : Text {')
	for i in range(N):
		head = 's := s #"," #' if i > 0 else 'var s ="("#'
		print(f'  {head} Hex.fromNat(Nat64.toNat(x.{i}));') 
	print('  s#")"')
	print('};')

def toNat(name,n=N):
	print(f'public func {name}(x : {"F" if n==N else "Fdbl"}) : Nat {{')
	print(f'  var v = Nat64.toNat(x.{n-1});')
	for i in range(1, n):
		print(f'  v := v * 0x100000000 + Nat64.toNat(x.{n-1-i});')
	print('  v')
	print('};')

def fromNat():
	print('public func fromNat(x : Nat) : F {')
	print(f'  var v = x;')
	for i in range(0, N):
		print(f'  let v{i} = Nat64.fromNat(v % 0x100000000);')
		print(f'  v := v / 0x100000000;')
	print(f'  {pack("v")}')
	print('};')

def cmp():
	print('public func cmp(x : F, y : F) : Order.Order {')
	for i in range(N):
		print(f'  if (x.{N-1-i} < y.{N-1-i}) return #less;')
		print(f'  if (x.{N-1-i} > y.{N-1-i}) return #greater;')
	print('  #equal')
	print('};')

# return x+y
def addPre():
	print(f'// ret.{N-1} may has (1<<32) as CF')
	print('public func addPre(x : F, y : F) : F {')
	for i in range(N):
		s = '' if i == 0 else f' +% c{i-1}'
		if i < N-1:
			print(f'  let t{i} = x.{i} +% y.{i}{s};')
			print(f'  let z{i} = t{i} & 0xffffffff;')
			print(f'  let c{i} = t{i} >> 32;')
		else:
			print(f'  let z{i} = x.{i} +% y.{i}{s};')
	print(f'  {pack("z")}')
	print('};')

# return x-y
# assume x>=y
def subPre():
	print('public func subPre(x : F, y : F) : (F, Nat64) {')
	for i in range(N):
		s = '' if i == 0 else f' -% c{i-1}'
		print(f'  let t{i} = x.{i} -% y.{i}{s};')
		print(f'  let z{i} = t{i} & 0xffffffff;')
		print(f'  let c{i} = t{i} >> 63;')
	print(f'  ({pack("z")}, c{N-1})')
	print('};')

# return x*y
def mulPre():
	print('public func mulPre(x : F, y : F) : Fdbl {')
	print('  var t : Nat64 = 0;')
	print('  var L : Nat64 = 0;')
	print('  var H : Nat64 = 0;')
	for i in range(N*2-1):
		for j in range(N+1):
			if j < N and 0 <= i-j < N:
				print(f'  t := x.{j} *% y.{i-j};')
				print(f'  L := L +% (t & 0xffffffff);')
				print(f'  H := H +% (t >> 32);')
		print(f'  let z{i} = L;')
		print(f'  L := H; H := 0;')
	print(f'  let z{N*2-1} = L;')
	print(f'  {pack("z", N*2)}')
	print('};')

# return x*y
def mulUnit():
	print(f'// z{N-1} may be >= 2^32')
	print('public func mulUnit(x : F, y : Nat64) : F {')
	print('  var t : Nat64 = x.0 *% y;')
	print('  let z0 = t & 0xffffffff;')
	for i in range(1,N):
		print(f'  t := x.{i} *% y +% (t >> 32);')
		if i < N-1:
			print(f'  let z{i} = t & 0xffffffff;')
		else:
			print(f'  let z{i} = t;')
	print(f'  {pack("z")}')
	print('};')

# return x+y*z
def addMulUnit():
	print('public func addMulUnit((x, xH) : (F, Nat64), y : F, z : Nat64) : (F, Nat64) {')
	print('  var a = y.0 *% z;')
	print('  var b : Nat64 = 0;')
	print('  var t = x.0 +% (a & 0xffffffff);')
	print('  let w0 = t & 0xffffffff;')
	for i in range(1,N):
		if (i % 2) == 1:
			print(f'  b := y.{i} *% z;')
			print(f'  t := (t >> 32) +% x.{i} +% (b & 0xffffffff) +% (a >> 32);')
		else:
			print(f'  a := y.{i} *% z;')
			print(f'  t := (t >> 32) +% x.{i} +% (a & 0xffffffff) +% (b >> 32);')
		print(f'  let w{i} = t & 0xffffffff;')
	print(f'  ({pack("w")}, xH +% (t >> 32) +% (b >> 32))')
	print('};')

def normalizeFpDbl():
	print('public func normalizeFpDbl(x : Fdbl) : Fdbl {')
	print(f'  let z0 = x.0 & 0xffffffff;')
	print(f'  var t = z0 >> 32;')
	for i in range(1,N*2):
		print(f'  t := t +% x.{i};')
		print(f'  let z{i} = t & 0xffffffff;')
		print(f'  t := t >> 32;')
	print(f'  {pack("z",N*2)}')
	print('};')

# return mod p 
"""
p =0xffffffff_ffffffff_ffffffff_ffffffff_ffffffff_ffffffff_fffffffe_fffffc2f
N = 1<<32
a = 0x3d1
maxX = (p-1)^2=[H:L]
H =0xffffffff_ffffffff_ffffffff_ffffffff_ffffffff_ffffffff_fffffffd_fffff85c
L =0x1_000007a4_000e9844
t =H*(a+N)+L=(H * N + L) + H * a
  =0x1_000003d0_ffffffff_ffffffff_ffffffff_ffffffff_ffffffff_fffffffe_fffff85d_fff16f60
Ht=0x1_000003d0
Lt=0xffffffff_ffffffff_ffffffff_ffffffff_ffffffff_fffffffe_fffff85d_fff16f60
Ht*(a+N)+Lt = (Ht * N + Lt) + Ht * a
  =0xffffffff_ffffffff_ffffffff_ffffffff_ffffffff_ffffffff_fffffffe_fffffc30
"""
def modp():
	print("""public func modp(x : Fdbl) : F {
  let a : Nat64 = 0x3d1;
  let H = (x.8, x.9, x.10, x.11, x.12, x.13, x.14, x.15);
  // ((H << 32) + L) + H * a
  let (Lt, Ht) = addMulUnit(((x.0, x.1+%x.8, x.2+%x.9, x.3+%x.10, x.4+%x.11, x.5+%x.12, x.6+%x.13, x.7+%x.14), x.15), H, a);
  let HtL = Ht & 0xffffffff;
  let HtH = Ht >> 32;
  let HtLa = HtL *% a;
  let HtHa = HtH *% a;
  var t = Lt.0 +% HtLa;
  let z0 = t & 0xffffffff;
  t := HtL +% Lt.1 +% HtHa +% (t >> 32);
  let z1 = t & 0xffffffff;
  t := HtH +% Lt.2 +% (t >> 32);
  let z2 = t & 0xffffffff;
  t := Lt.3 +% (t >> 32);
  let z3 = t & 0xffffffff;
  t := Lt.4 +% (t >> 32);
  let z4 = t & 0xffffffff;
  t := Lt.5 +% (t >> 32);
  let z5 = t & 0xffffffff;
  t := Lt.6 +% (t >> 32);
  let z6 = t & 0xffffffff;
  let z7 = Lt.7 +% (t >> 32);
  (z0,z1,z2,z3,z4,z5,z6,z7)
};""")

header="""// generated by test/gen.py
import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hex "../src/hex";
import Order "mo:base/Order";

module {
"""

def printPrime():
	p = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
	s = 'let p : F = ('
	for i in range(N):
		v = p & 0xffffffff
		p >>= 32
		if i > 0:
			s += ', '
		s += hex(v)
	s += ');'
	print(s)

def printType(name, n):
	s = f'public type {name} = ('
	for i in range(n):
		if i > 0:
			s += ', '
		s += 'Nat64'
	s += ');'
	print(s)

def misc():
	print("""public func add(x : F, y : F) : F {
  let z = addPre(x,y);
  if (cmp(z, p) != #less) subPre(z, p).0 else z
};
  public func sub(x : F, y : F) : F {
  let (z, CF) = subPre(x,y);
  if (CF == 0) z else addPre(z, p)
};
  public func mul(x : F, y : F) : F {
    modp(mulPre(x, y))
};
""")


print(header)
printType('F', N)
printType('Fdbl', N*2)
printPrime()
toNat('toNat', N)
toNat('DtoNat', N*2)
fromNat()
toStr()
cmp()
subPre()
addPre()
mulPre()
normalizeFpDbl()
mulUnit()
addMulUnit()
modp()
misc()

print('};')
