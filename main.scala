object ConnectFour {
  def main(args: Array[String]) = {
    val rows = 6
    val columns = 7
    val matrix = Array.ofDim[String](rows, columns)
    var playing = true
    var xMoves: Array[Int] = Array()
    var oMoves: Array[Int] = Array()
    var activePlayer = "X"

    def initMatrix() = {
      for (r <- 0 to rows - 1) {
        for (c <- 0 to columns - 1) {
          matrix(r)(c) = "_"
        }
      }
    }

    def printMatrix(): Unit = {
      for (r <- 0 to rows - 1) {
        for (c <- 0 to columns - 1) {
          if (c == 6) print(matrix(r)(c) + "\n") else print(matrix(r)(c))
        }
      }
      println("\n")
    }

    def printMoves(): Unit = {
      print("X: ")
      for (x <- xMoves) {
        print(s"${x}, ")
      }
      print("\n" + "O: ")
      for (o <- oMoves) {
        print(s"${o}, ")
      }
      println("\n")
    }

    def pushToMatrix(col: Int, player: String) = {
      var pushed = false
      for (r <- rows - 1 to 0 by -1) {
        for (c <- 0 to columns - 1) {
          if (matrix(r)(col - 1) == "_" && !pushed) {
            matrix(r)(col - 1) = player
            pushed = true
            if (player == "X") xMoves = xMoves :+ col
            else oMoves = oMoves :+ col
          }
        }
      }
    }

    def checkForPlayAgain() = {
      println("Play again? (y/n)?")
      val again = scala.io.StdIn.readLine()
      if (again == "y") {
        initMatrix()
        xMoves = Array.emptyIntArray
        oMoves = Array.emptyIntArray
      } else System.exit(0)
    }

    def printVictory(player: String) = {
      println("---VICTORY---")
      println("Player " + player + " won!")
      checkForPlayAgain()
    }

    def checkForDraw() = {}

    def checkForWinner(player: String) = {
      // Horizontal
      for (c <- 0 to columns - 4) {
        for (r <- 0 to rows - 1) {
          if (
            matrix(r)(c) == player && matrix(r)(c + 1) == player && matrix(r)(
              c + 2
            ) == player && matrix(r)(c + 3) == player
          ) {
            printVictory(player)
          }
        }
      }
      // Vertical
      for (c <- 0 to columns - 1) {
        for (r <- 0 to rows - 4) {
          if (
            matrix(r)(c) == player && matrix(r + 1)(c) == player && matrix(
              r + 2
            )(
              c
            ) == player && matrix(r + 3)(c) == player
          ) {
            printVictory(player)
          }
        }
      }
      // Positive slope
      for (c <- 0 to columns - 4) {
        for (r <- 0 to rows - 4) {
          if (
            matrix(r)(c) == player && matrix(r + 1)(c + 1) == player && matrix(
              r + 2
            )(
              c + 1
            ) == player && matrix(r + 3)(c + 3) == player
          ) {
            printVictory(player)
          }
        }
      }
      // Negative slope
      for (c <- 0 to columns - 4) {
        for (r <- 3 to rows - 1) {
          if (
            matrix(r)(c) == player && matrix(r - 1)(c + 1) == player && matrix(
              r - 2
            )(
              c + 2
            ) == player && matrix(r - 3)(c + 3) == player
          ) {
            printVictory(player)
          }
        }
      }
    }

    def getPlayerInput(): Int = {
      var playerColumn = scala.io.StdIn.readLine()
      while {
        val columnNumber = playerColumn.toIntOption.getOrElse(-1)
        columnNumber == -1 || columnNumber < 1 || columnNumber > columns
      }
      do {
        playerColumn = scala.io.StdIn.readLine()
      }
      return playerColumn.toIntOption.getOrElse(-1)
    }

    initMatrix()
    printMatrix()

    while (playing) {
      if (activePlayer == "X") println("X's move") else println("O's move")
      pushToMatrix(getPlayerInput(), activePlayer)
      printMatrix()
      printMoves()
      checkForWinner(activePlayer)
      checkForDraw()
      activePlayer = if (activePlayer == "X") "O" else "X"
    }
  }
}
