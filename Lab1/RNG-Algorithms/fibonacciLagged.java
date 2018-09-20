//Initialize the vector antics in order that no empty position exists.
   //So began a few values with different values between them. The results genrand returns are calculated from the number 3000.
  
   class fiblagged { 
     
     public static int n=3000;
     public static double antics[] = new double[30000];
   
     public static void ini() { 
             for(int i=0;i<6000;i++)
             antics[i]=i;
     }
     
     public static double genrand() {
         double a,b;
         //Parametritzacio
         double j = 4;
         double k = 400;
         double m = 2147483647;
        
         a=antics[n-(int)j];
         b=antics[n-(int)k];
         
         antics[n]=((a*b)%m);
         
         n++;
         return antics[n-1]/m;
     }

      /* This main () removes the first 50 numbers generated */
   
    public static void main(String[] args) {
       ini();
       for(int i=0;i<50;i++) {
           System.out.println(genrand());
       }
    }
   
   }