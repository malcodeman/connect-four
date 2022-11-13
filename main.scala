object Hello {
  def main(args: Array[String]) = {
    val rows = 6
    val columns = 7
    val matrix = Array.ofDim[String](rows, columns)
    var playing = true

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
    }

    def pushToMatrix(col: Int, player: String) = {
      var done = false
      for (r <- rows - 1 to 0 by -1) {
        for (c <- 0 to columns - 1) {
          if (matrix(r)(col - 1) == "_" && !done) {
            matrix(r)(col - 1) = player
            done = true
          }
        }
      }
    }

    def printVictory(player: String) = {
      println("---VICTORY---")
      println("Player " + player + " won!")
      playing = false
    }

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
        for (r <- 0 to rows - 1) {
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

    initMatrix()
    printMatrix()

    var activePlayer = "X"

    while (playing) {
      val userColumn = scala.io.StdIn.readLine()
      pushToMatrix(userColumn.toIntOption.getOrElse(-1), activePlayer)
      printMatrix()
      checkForWinner(activePlayer)
      activePlayer = if (activePlayer == "X") "O" else "X"
    }
  }
}
