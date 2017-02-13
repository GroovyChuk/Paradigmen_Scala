object Main {
      def main(args: Array[String]) {
        val primzahlen = Lst.range(2,100).filter(isPrime)
        println("Die ersten 10 Primzahlen:\n" + primzahlen.takeAsList(10))        
        println("\nSumme (foldLeft): " + primzahlen.foldLeft(0)(_+_))
        var s = 0
        for (x <- primzahlen) {
          s += x
        }
        println("Summe (prozedural): " + s)
        
        println("\n4 Werte:")
        for (i <- 0 until 4) {
          printf("%d: = %3d%n", i, primzahlen(i))
        } 


        val products = for { // bei "{" braucht man kein ";"
          x <- Lst.range(3,9,2)
          y <- Lst.range(3,9,2)
          if x*y < 30
        } yield (x,y)
        println();
        for ((x,y) <- products) printf("%2d * %2d < 30%n", x, y)
    }
    
    def isPrime(n: Int): Boolean =
      n == 2 || n % 2 == 1 && Lst.range(3, n-1, 2).forall(n % _ != 0)
}