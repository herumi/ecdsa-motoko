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

# return normalized x*y
def mulUnit():
	print('public func mulUnit(x : F, y : Nat64) : (F, Nat64) {')
	print('  var t : Nat64 = x.0 *% y;')
	print('  let z0 = t & 0xffffffff;')
	for i in range(1,N):
		print(f'  t := x.{i} *% y +% (t >> 32);')
		print(f'  let z{i} = t & 0xffffffff;')
	print('  t >>= 32;')
	print(f'  ({pack("z",N)},t)')
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

# return (x+y)%p
def add():
	print("""public func add(x : F, y : F) : F {
  let z = addPre(x,y);
  if (cmp(z, p) != #less) subPre(z, p).0 else z
};""")

# return (x-y)%p
def sub():
	print("""public func sub(x : F, y : F) : F {
  let (z, CF) = subPre(x,y);
  if (CF == 0) z else addPre(z, p)
};""")

# return mod p 
def modp():
	print("""public func modp(x : Fdbl) : F {
  let a : Nat64 = 0x1000003d1;
  let L = (x.0, x.1, x.2, x.3, x.4, x.5, x.6, x.7);
  let H = (x.8, x.9, x.10, x.11, x.12, x.13, x.14, x.15);
  let (t0, t1) = mulUnit(H, a);
  let t = addPre(t0, L);
  
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
add()
sub()
mulPre()
normalizeFpDbl()
mulUnit()

print('};')
