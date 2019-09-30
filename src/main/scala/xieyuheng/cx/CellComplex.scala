package xieyuheng.cx

class Entry(src: Cell, tar: Cell, cfg: Cell) {
  assert(cfg.dom == src.dom)
  assert(cfg.cod == tar.dom)
}

class Figure(list: List[Entry])

class Morphism(
  val dom: CellComplex,
  val cod: CellComplex,
  val fig: Figure,
  val uuid: String)

class CellComplex(
  val dim: Int,
  val cell_map: Map[Int, List[Cell]],
  val name_map: Map[String, Cell])

class Cell(
  dom: Spherical,
  cod: CellComplex,
  fig: Figure,
  uuid: String,
) extends Morphism (
  dom: CellComplex,
  cod: CellComplex,
  fig: Figure,
  uuid: String)

class Spherical(
  dim: Int,
  cell_map: Map[Int, List[Cell]],
  name_map: Map[String, Cell],
  val spherical_evidence: SphericalEvidence,
) extends CellComplex(
  dim: Int,
  cell_map: Map[Int, List[Cell]],
  name_map: Map[String, Cell])

class SphericalEvidence() {
  // TODO
}
