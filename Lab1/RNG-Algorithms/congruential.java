/* A program in Java for linear congruential generator
 * Authors : Odin Lainé, Nicolas Leroux - UPC Barcelona - FIB
 * The main function prints a list of a random number following
 * the linear congruential general theory.
 * In String[] args we expect that:
 * - args[0] is the amount of generated number
 * - args[1] is the multiplier
 * - args[2] is the increment
 * - args[3] is the seed (or the initial number) 
 * Actually, the revelant variable is the seed and for example the modulo is
 * fixed at (2^31)-1
 */

package gna;

import java.io.*;


public class Main {

    /**
     * generator Park Miller Standard Minimal
     */
    public static void main(String[] args) throws IOException {
        //next value
        double x2 =0;
        
        //first value
        double x1 = Double.valueOf(args[3]);
        
        //output value in the [0,1] interval
        double output = 0;
        
        //definition of a output file in a Windows distribution and 
        //initialization
        BufferedWriter sortie = new BufferedWriter(new FileWriter("C:" +
                "\\Documents and Settings\\ptit chou\\Bureau\\test.txt", true));
        sortie.write(args[0]);

        for ( int loop = Integer.valueOf(args[0]);loop>0;loop--){
            x2 = (Double.valueOf(args[1])*x1 + Double.valueOf(args[2])) %(Math.pow(2,31)- 1);
            output = ((double) x2 / (Math.pow(2, 31) - 1));
            sortie.write(String.valueOf(output));
            sortie.newLine();
            x1 = x2;
        }
        sortie.close();      
    }
} 