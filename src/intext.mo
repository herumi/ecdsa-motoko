module {
  // return (d, lambda, mu) with d = lambda * a + mu * b
  public func extGcd(a : Int, b : Int) : (Int, Int, Int) {
    if (a == 0)
      (b, 0, 1)
    else {
      // b = qa + r
      let (q, r) = (b/a, b%a);
      // d = xr + ya
      let (d, x, y) = extGcd(r, a);
      // (y-qx)a + xb = ya - qxa + x(qa + r) = ya + xr = d
      (d, y - q * x, x)
    }
  };

  // non-recursive version of the same function
  // turns out to use more cycles
  public func extGcd_nr(a : Int, b : Int) : (Int, Int, Int) {
    // the last two residues and values of lambda, mu
    // when r.1 == 0 then r.0 is the gcd
    var r : (Int, Int) = (b, a);
    var lambda : (Int, Int) = (0,1);
    var mu : (Int, Int) = (1,0);

    while (r.1 != 0) {
	    // r.0 = q_ * r.1 + r_
	    let (q_, r_) = (r.0/r.1, r.0%r.1);    	

	    lambda := (lambda.1, lambda.0 - q_ * lambda.1);
	    mu := (mu.1, mu.0 - q_ * mu.1);
	    r := (r.1, r_);
    };
    (r.0, lambda.0, mu.0)
  };
}