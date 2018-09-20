unsigned long xorshiftrand(void) { 
   unsigned long t; 
   t = x ^ (x << a);
   x = y; 
   y = z; 
   z = w;
   return w = (w ^ (w >> b)) ^ (t ^ (t >> c)); 
}