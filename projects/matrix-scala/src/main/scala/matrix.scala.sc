object matrix {
  final case class Matrix(rows: Vector[Vector[Int]]) {
    def addCol(col: Seq[Int]): Matrix = {
      require((!rows.isEmpty && rows.size == col.size) || rows.isEmpty, "Dimension of the new column is wrong.")
      val es = if (rows.isEmpty)
        (col map { _ ⇒ Vector.empty[Int] }).toVector
      else
        rows
      Matrix(
        es.zip(col).map {
          case (row, colValue) ⇒
            row :+ colValue
        }
      )
    }

    def addRow(row: Seq[Int]): Matrix = {
      require((!rows.isEmpty && rows.head.size == row.size) || rows.isEmpty, "Dimension of the new row is wrong.")
      Matrix(rows :+ row.toVector)
    }

    def pretty: String = {
      def maxLen(): Int = {
        val sizes = for {
          row ← rows
          col ← row
        } yield col.toString.size
        if (sizes.isEmpty)
          0
        else
          sizes.max
      }

      val max = maxLen()
      val s = rows.map { row ⇒
        val s = row map { i ⇒ s"%${max}d".format(i) }
        s.mkString("[ ", ", ", " ]")
      }
      s.mkString("\n")
    }
  }

  object Matrix {
    val empty = Matrix(Vector.empty)
  }

  def run(): Unit = {
    val matrix = Matrix.empty addCol (1 to 5) addRow Seq(10) addCol (20 to 25) addCol (31 to 36) addCol (15 to 20) addRow Seq(1, 1, 1, 1)
    println(matrix.pretty)
  }
}

matrix.run()
