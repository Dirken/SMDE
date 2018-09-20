public class Composite {
   private congruencial c1;
   private congruencial c2;
   private congruencial c3;
   private double [] vector;
   private int index;
   /**
   * Construct
   * c1,c2,c3: congruential variables.
   * vector: array filled with the values corresponding to c1, it will be used as reference to decide future random values of the compound.
   */
   public Composite() {
       index = 0;
       c1 = new congruencial(2147483647,376891,19183,437693);
       c2 = new congruencial(2147483647,548489,67867979,467353);
       c3 = new congruencial(2147483647,562753,611549,611953);
       vector = new double[3000];
       for(int i=0;i<vector.length;i++){
           vector[i]=c1.next();
       }
   }
   /**
   * Return the next generated number
   */
   public float next(){
       if(vector[index]>0.5){
           return c2.next();
       }else{
           return c3.next();
       }
       index = (index + 1)%vector.length;
   }
   /**
    * @return the c1
    */
   public congruencial getC1() {
       return c1;
   }
   /**
    * @return the c2
    */
   public congruencial getC2() {
       return c2;
   }
   /**
    * @return the c3
    */
   public congruencial getC3() {
       return c3;
   }
   class congruencial{
       private int m;
       private int a;
       private int c;
       private int seed;
       private double prev=0;
       /**
       *m: modulo
       *a: multiplier
       *c: increment
       *seed: seed or initial value.
       *{Pre: (0 < m) && (a < m) && (c < m) && (seed < m). 
       *(c and m should be relatively primes) &&
       *( If q is a prime number and divisor of m, then q also divdes a-1) &&
       *(If m is multiple of 4, a-1 a-1 is multiple of 4,  
       *a can't be multiple of 4)} 
       */	
      public congruencial(int m, int a, int c, int seed){
          this.m = m;
          this.a = a;
          this.c = c;
          this.seed = seed;
          this.prev = seed;
      }
     /**
     * Generate next number
     */	
      public float next(){
          prev = (a * prev + c )%m;
          return  (float)prev/m;
      }
   }
   public static void main(String [] args){
       Composite composite = new Composite();
       for(int i=0;i<3000;i++){
           System.out.println(composite.next());
       }
   }
 }