import org.scalatest.TestSuite
import ScalaXMLPackage._
import customXmlTypeSafe._

object XMLExample1 extends Application {
 
  var xmlFile = new TSXMLfile("BooksDB.xml");
  var root = xmlFile.createTSRootNode("BookList")  //root is of TSRootNode type
  
  var node1 = root.addNode("Book")
  node1.addAttributes("ISBN","ISBN-0-262-16209-1");   
  node1.addAttributes("Price","142.93")  
  node1.addNode("Title","Introduction to Automata Theory")
  
  var node2 = root.addNode("Book")
  node2.addAttributes("ISBN","ISBN-0-13-031995-3");   
  node2.addAttributes("Price","59.02")   
  node2.addNode("Title","The Design and Analysis of Computer Algorithms")

  //Adding Authors
  var nodeAuth = new TSNode("Authors")
  nodeAuth.addNode("Author","Jeff Ullman")
  nodeAuth.addNode("Author","Alfred Aho")

  node1.addNode(nodeAuth)
  node2.addNode(nodeAuth)


  scala.xml.XML.save(myXMLfile, xml)
}