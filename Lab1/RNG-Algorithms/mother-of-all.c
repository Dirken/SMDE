#include <string.h>
#include <memory.h>
static short mother1[10];
static short mother2[10];
static short mStart=1;

#define m16Long 65536L
#define m16Mask 0xFFFF
#define m15Mask 0x7FFF
#define m31Mask 0x7FFFFFFF
#define m32Double  4294967295.0

double Mother(unsigned long *pSeed,double *res)
{
   unsigned long number,number1,number2;
   short n,*p;
   unsigned short sNumber;
   
   if (mStart) {
           
       sNumber=*pSeed&m16Mask;
       number=*pSeed&m31Mask;
       p=mother1;
       
       for (n=18;n--;) {
                   
           number=30903*sNumber+(number>>16);   
           *p++=sNumber=number&m16Mask;
           
           if (n==9)
               
               p=mother2;
           }
           
           mother1[0]&=m15Mask;
           mother2[0]&=m15Mask;
           mStart=0;
   
   }
   
   memcpy((char*)mother1+2,(char*)mother1+1,8*sizeof(short));
   memcpy((char*)mother2+2,(char*)mother2+1,8*sizeof(short));
   
   number1=mother1[0];
   number2=mother2[0];
   
   number1+= 1941 * mother1[2] + 1860 * mother1[3] + 
             1812 * mother1[4] + 1776 * mother1[5] + 
             1492 * mother1[6] + 1215 * mother1[7] + 
             1066 * mother1[8] + 12013 * mother1[9];
   number2 += 1111 * mother2[2] + 2222 * mother2[3] + 
              3333 * mother2[4] + 4444 * mother2[5] + 
              5555 * mother2[6] + 6666 * mother2[7] + 
              7777 * mother2[8] + 9272 * mother2[9];
   
   mother1[0]=number1/m16Long;
   mother2[0]=number2/m16Long;
   
   mother1[1]=m16Mask&number1;
   mother2[1]=m16Mask&number2;
   
   *pSeed=(((long)mother1[1])<<16)+(long)mother2[1];
   *res = ((double)*pSeed)/m32Double;
}