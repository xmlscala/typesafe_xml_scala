package ImplementLift
import scala.reflect.api.Universe

trait XMLNodes {
  protected val __universe: Universe; import __universe._

  case class Unquote(tree: Tree) extends xml.SpecialNode {
    def label: String = "#UNQUOTE"
    def buildString(sb: StringBuilder): StringBuilder = sb.append(s"{${showCode(tree)}}")
  }
}
