private BigInteger x; // = {xi,xi+1,...,xi+r,ci}
private BigInteger a;
private BigInteger b;
public float Random()
{
     int n = x.length; //calcular x
     BigInteger v = a.multiply(x[0]); //a*x[i-r]
     v = v.add(x[n - 1]); //a*xi-r + xi
     xn = v.mod(b); // (a*xi-r + xi) % b
     BigInteger cn = v.divide(b); // (a*xi-r + xi) / b (divisió entera, part de sota)
     for (int i = 0; i < n - 2; i++) x[i] = x[i + 1]; //avancem al següent i
     x[n - 2] = xn; //seguent xi= (a*xi-r + xi) % b
     x[n - 1] = cn; //seguent ci= (a*xi-r + xi) / b
     return (float)(xn.floatValue() / b.floatValue()); //  (ultim xi)/b  (divisio amb coma flotant)
}