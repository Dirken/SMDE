public static void main(String[] args) {
    Scanner scan=new Scanner(System.in);
    int cantidad_números= scan.nextInt();
    int número_de_decimales = scan.nextInt();
    int semilla = scan.nextInt();
    RNG generador = new RNG();
    int q=generador.calcular_q(cantidad_números, numero_de_decimales, semilla);
    generador.imprimeNum(q, cantidad_números, numero_de_decimales);
}
public RNG(){


	public int calcular_q(int lon, int nd, int se){
       lon = lon*nd*se;
       int S;
       lon=(lon-1)/2+1;
       for (S=lon;;S++){
           if (primo(S)){
               if (primo(2*S+1)){
                   if (S%20 == 3 || S%20 == 9 || S%20 == 11)
                   return 2*S+1; 
               }
           }
       }
	}

	public boolean primo(int x){
	   for(int i=2; i<=Math.sqrt(x); i++){
	       if (x%i == 0) return false;
	   }
	   return true;
	}

	public void imprimeNum(int q, int lon, int nd){
	    lon = lon*nd;
	    java.math.BigDecimal qf = new java.math.BigDecimal(q);
	    java.math.BigDecimal r=new java.math.BigDecimal(1);
	    java.math.BigDecimal resultat = r.divide(qf, lon, BigDecimal.ROUND_FLOOR);
		try {
		   BufferedWriter out = new BufferedWriter(new FileWriter("resultado.txt"));
		   out.write(""+lon); //escribir la longitud primero
		   out.newLine();
		   char[] vecChar = new char[lon];        
		   java.math.BigDecimal m = new java.math.BigDecimal("10");
		   java.math.BigDecimal sub;
		   String enter;
		   for (int i=0; i<lon; i++){
		       resultat = resultat.multiply(m);
		       enter = Integer.toString(resultat.intValue());
		       sub = new java.math.BigDecimal(enter);
		       resultat = resultat.subtract(sub);
		       vecChar[i] = enter.charAt(0);
		   }
		   System.out.println(vecChar);
		    for(int j=0; j<lon; ){
		        System.out.print("0.");
		        out.write("0.");
		        for (int k=0; k<nd; k++){
		            System.out.print(vecChar[j]);
		            out.write(vecChar[j]);
		            j++;
		        }
		        System.out.println();
		        out.newLine();
		    }
		} catch (IOException e) {
			System.out.println("Error al escribir el fichero " +e.getMessage());
		}
	}
}

public static void main(String[] args) {
   Scanner scan=new Scanner(System.in);
   int cantidad_números= scan.nextInt();
   int número_de_decimales = scan.nextInt();
   int semilla = scan.nextInt();
   RNG generador = new RNG();
   int q=generador.calcular_q(cantidad_números, numero_de_decimales, semilla);
   generador.imprimeNum(q, cantidad_números, numero_de_decimales);
}