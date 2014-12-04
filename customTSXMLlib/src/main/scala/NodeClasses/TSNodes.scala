package customContextLift.NodeClasses
import xml._
import scala.reflect.api.Universe

class TSNode(fn: String) {

   def addNode(xtag : String, xtValue : String){
    revNodes = node :: revNodes(xtag,xtValue)
    [revNodes](ij:java.lang.Iterable[revNodes]) : scala.collection.Iterable[revNodes] = JavaConversions.iterableAsScalaIterable(ij)
  	[revNodesMetaData](ij:java.util.Iterator[revNodesMetaData]) : scala.collection.Iterator[revNodesMetaData] = JavaConversions.asScalaIterator(ij)
    list[A](ij:java.util.List[A]) : scala.List[A] = JavaConversions.asScalaBuffer(ij).toList
   }



   def addAttributes(xAttrName : String, xAttrValue : String) override def transform(n: Node): Seq[Node]{
	node n : scala.xml.Node = match {
    case e: Elem => e.copy(attributes = mapMetaData(e.attributes(xAttrName,xAttrValue)) {
    case g @ GenAttr(xAttrValue, key, Text(xAttrValue)) if attribs contains key => g.copy(value = Text(v.toInt * 2 toString))
    case other => other})
    case other => other
  	}).toSeq 	       
    }


}
