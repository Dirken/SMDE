public static double nextRandom()
{
   int i,k;
   int aux;
   double s,pot;
   s = 0;
   pot = (Math.pow(2,l));
   if((q - cont + 1) >= l)
   {
      for(k = 1; k <= l; k++)
       {
           vaux[l-k] = (Integer)vbits.get(cont);
           cont ++;
       }
   }
   else
   {
       aux = 0;
       while((posact-cont+1) <= l)
       {   for(i=1; i <=l; i++)
           {
               aux = aux + ((Integer)vbits.get(posact-i));
           }
           vbits.add(aux%2);
           posact++;
       }
       for(k = 1; k <= l; k++)
       {
           vaux[l-k] = (Integer)vbits.get(cont);
           cont ++;
       }
  }
   for(i = 1; i <= l; i++)
   {
       s = s + vaux[l-i]* (Math.pow(2,l-i));
   }
 return (s/(pot));
}

public class GNA {
  /**
  * Creates a new instance of GNA
  */
  private static int r;           //value of r such file numbers for generating pseudo-random
  private static int q;           // q value for generating such file numbers of pseudo-random
  private static ArrayList vbits; // value of the q-bit to generate such file numbers of the pseudo-random
  private static int l;           // l value for generating such file numbers of pseudo-random
  private static int posact;      // bit that has to be generated;
  private static int cont;        // positions
  Vaux private static int [],     // value of the last l bits generados
}