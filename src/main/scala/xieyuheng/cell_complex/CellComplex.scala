package xieyuheng.cell_complex

class Entry(src: Cell, tar: Cell, cfg: Cell) {
  assert(cfg.dom == src.dom)
  assert(cfg.cod == tar.dom)
}

class Figure(list: List[Entry])

class Cell(
  val dom: CellComplex,
  val cod: CellComplex,
  val fig: Figure,
  val uuid: String)

class CellComplex(
  val dim: Int,
  val cell_map: Map[Int, List[Cell]],
  val name_map: Map[String, Cell])
