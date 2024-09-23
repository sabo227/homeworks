trait Cell {
    def toString(): String
}

class EmptyCell extends Cell {
    override def toString() = "empty"
}

class NumberCell(num: Int) extends Cell {
    override def toString() = num.toString()
}

class StringCell(str: String) extends Cell {
    override def toString() = str
}

class ReferenceCell(x: Int, y: Int, table: Table) extends Cell {
    def findNonRefCell(from: Cell): Option[Cell] = {
        table.getCell(x, y) match {
            case Some(cell: ReferenceCell) => if (cell == from) None else cell.findNonRefCell(from)
            case Some(cell: Cell) => Some(cell)
            case None => None
        }
    }
    
    override def toString() = findNonRefCell(this) match {
        case Some(cell: Cell) => cell.toString()
        case None => "cyclic"
    }
}

