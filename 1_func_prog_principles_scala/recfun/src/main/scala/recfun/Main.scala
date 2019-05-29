package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == r || c == 0 || r == 0) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(chars: List[Char], acc: Int) : Boolean = {
        if(chars.isEmpty || acc == -1) acc == 0
        else balance(chars.tail, if(chars.head == '(') acc + 1 else if (chars.head == ')') acc -1 else acc)
      }
      balance(chars, 0)
    }

  
  /**
   * Exercise 3
    * money= 4, List(1, 2)
    * {4, List(1, 2) + 1} + {4, List(2), 0}
    *
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countChange(money: Int, coins: List[Int], acc: Int) : Int = {
        if(money == acc) 1
        else if(coins.isEmpty || acc > money) 0
        else
          countChange(money, coins, acc + coins.head) + countChange(money, coins.tail, acc)
      }
      countChange(money, coins, 0)

    }

  }
