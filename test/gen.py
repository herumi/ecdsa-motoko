N = 3

def pack(v):
	s = ''
	for i in range(N):
		if i == 0:
			s = f'({v}0'
		else:
			s += f',{v}{i}'
	return s + ")"

def toStr():
	print('func toStr(x : F) : Text {')
	for i in range(N):
		head = 's := s #"," #' if i > 0 else 'var s ="("#'
		print(f'  {head} Hex.fromNat(Nat64.toNat(x.{i}));') 
	print('  s#")"')
	print('};')

def toNat():
	print('func toNat(x : F) : Nat {')
	print(f'  var v = Nat64.toNat(x.{N-1});')
	for i in range(1, N):
		print(f'  v := v * 0x100000000 + Nat64.toNat(x.{N-1-i});')
	print('  v')
	print('};')

def fromNat():
	print('func fromNat(x : Nat) : F {')
	print(f'  var v = x;')
	for i in range(0, N):
		print(f'  let v{i} = Nat64.fromNat(v % 0x100000000);')
		print(f'  v := v / 0x100000000;')
	print(f'  {pack("v")}')
	print('};')

def cmp():
	print('func cmp(x : F, y : F) : Order.Order {')
	for i in range(N):
		print(f'  if (x.{N-1-i} < y.{N-1-i}) return #less;')
		print(f'  if (x.{N-1-i} > y.{N-1-i}) return #greater;')
	print('  #equal')
	print('};')

# return x+y
def addPre():
	print('func addPre(x : F, y : F) : F {')
	for i in range(N):
		s = '' if i == 0 else f' +% c{i-1}'
		if i < N-1:
			print(f'  let t{i} = x.{i} +% y.{i}{s};')
			print(f'  let z{i} = t{i} & 0xffffffff;')
			print(f'  let c{i} = (t{i} >> 32)&1;')
		else:
			print(f'  let z{i} = x.{i} +% y.{i}{s};')
	print(f'  {pack("z")}')
	print('};')

# return x-y
# assume x>=y
def subPre():
	print('func subPre(x : F, y : F) : F {')
	for i in range(N):
		s = '' if i == 0 else f' -% c{i-1}'
		if i < N-1:
			print(f'  let t{i} = x.{i} -% y.{i}{s};')
			print(f'  let z{i} = t{i} & 0xffffffff;')
			print(f'  let c{i} = (t{i} >> 32)&1;')
		else:
			print(f'  let z{i} = x.{i} -% y.{i}{s};')
	print(f'  {pack("z")}')
	print('};')

# return (x+y)%p
def add():
	code="""func add(x : F, y : F) : F {
  let z = addPre(x,y);
  if (cmp(z, p) != #less) subPre(z, p) else z
};"""
	print(code)

header="""import Debug "mo:base/Debug";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hex "../src/hex";
import Order "mo:base/Order";
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

def printType():
	s = 'type F = ('
	for i in range(N):
		if i > 0:
			s += ', '
		s += 'Nat64'
	s += ');'
	print(s)


print(header)
printType()
printPrime()
toNat()
fromNat()
toStr()
cmp()
subPre()
addPre()
add()


print("""
let a : F = (5,2,3);
let b : F = (4,5,6);
let c = subPre(a, b);
let d = addPre(c, b);
Debug.print("c=" # toStr(c));
Debug.print("d=" # toStr(d));
Debug.print("d=" # Nat.toText(toNat(d)));
Debug.print("e=" # toStr(fromNat(toNat(d))));
""")
