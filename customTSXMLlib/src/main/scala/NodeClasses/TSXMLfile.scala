package customContextLift._
import xml._
import scala.reflect.api.Universe
import javax.xml.parsers.SAXParser
import javax.xml.parsers.SAXParserFactory
import javax.xml.validation.Schema
import javax.xml.validation.ValidatorHandler


class TSXMLfile(fn: String) {
   var xmlfile: String = fn

   def createXMLfile(xmlFile: String) {
      var vFile : Boolean = isValidXMLGeneration(fc)
      if(vFile) 
       { 
        try {
          val schemaLang = "http://www.w3.org/2001/XMLSchema"
          val factory = SchemaFactory.newInstance(schemaLang)
          val schema = factory.newSchema(new StreamSource(xsdFile))
          val validator = schema.newValidator()
          validator.validate(new StreamSource(xmlFile))
        } catch {
          case ex: SAXException => println(ex.getMessage()); return false
          case ex: Exception => ex.printStackTrace()
        }

        xml.save(xmlFile) }
      else 
       { presentErrors(showErr) }
     }
       
   def isValidXMLGeneration(fc : String){
      var result : Boolean = false
      var xmlCheck : XMLNodes
    // Using native Scala XML parser
    val parser: SAXParser = try {
      val f = SAXParserFactory.newInstance()
      f.setNamespaceAware(true)
      f.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
      f.newSAXParser()
    } catch {
      case e: Exception =>
        Console.err.println("error: Unable to instantiate parser")
        throw e
    }

    val xr = parser.getXMLReader()
    val vh = schema.newValidatorHandler()
    vh.setContentHandler(this)
    xr.setContentHandler(vh)
    result = rootElem.asInstanceOf[Elem]
  
    // parse file
    scopeStack.push(result)
    xr.parse(source)
    scopeStack.pop
    
    return result
     }


   def presentErrors(err : Error) extends Exception(message, nestedException){
    Try[Try[Try[InputStream]]] = parseURL(err).map { u =>Try(u.openStream()).map(Any => Try(Any.getInputStream))} 
    def this() = this(err, System.MatchError)
    def this(message: String) = this(message, null)
    def this(nestedException : Throwable) = this("", nestedException)
    showErr.process(message, System.MetaData)
    return showErr
     }   


  def createTSRootNode(String : rootName){
      var root : new TSRootNode(rootName)
      return root
     }

  object XML extends XMLLoader[Elem] {
  val xml = "xml"
  val xmlns = "xmlns"
  val namespace = "http://www.w3.org/XML/1998/namespace"
  val preserve = "preserve"
  val space = "space"
  val lang = "lang"
  val encoding = "ISO-8859-1"

  /** Returns an XMLLoader whose load* methods will use the supplied SAXParser. */
  def withSAXParser(p: SAXParser): XMLLoader[Elem] =
    new XMLLoader[Elem] { override val parser: SAXParser = p }

  /**
   * Saves a node to a file with given filename using given encoding
   *  optionally with xmldecl and doctype declaration.
   *
   *  @param filename the filename
   *  @param node     the xml node we want to write
   *  @param enc      encoding to use
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def save(
    filename: String,
    node: Node,
    enc: String = encoding,
    xmlDecl: Boolean = false,
    doctype: dtd.DocType = null): Unit =
    {
      val fos = new FileOutputStream(filename)
      val w = Channels.newWriter(fos.getChannel(), enc)

      ultimately(w.close())(
        write(w, node, enc, xmlDecl, doctype)
      )
    }

  /**
   * Writes the given node using writer, optionally with xml decl and doctype.
   *  It's the caller's responsibility to close the writer.
   *
   *  @param w        the writer
   *  @param node     the xml node we want to write
   *  @param enc      the string to be used in `xmlDecl`
   *  @param xmlDecl  if true, write xml declaration
   *  @param doctype  if not null, write doctype declaration
   */
  final def write(w: java.io.Writer, node: Node, enc: String, xmlDecl: Boolean, doctype: dtd.DocType, minimizeTags: MinimizeMode.Value = MinimizeMode.Default) {
    /* TODO: optimize by giving writer parameter to toXML*/
    if (xmlDecl) w.write("<?xml version='1.0' encoding='" + enc + "'?>\n")
    if (doctype ne null) w.write(doctype.toString() + "\n")
    w.write(Utility.serialize(node, minimizeTags = minimizeTags).toString)
  }
}

object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "scala-xml"
  protected def pickJarBasedOn  = classOf[scala.xml.pull.XMLEventReader]
}

   
}



class TSRootNode(){
  public root : TSNodes
  def createTSRootNode(TSNodes : treeRoot, text => scala.xml.NodeText ){
       treeRoot = this.root
       treeRoot = new DOMXMLScoped.makeXMLtree(text) 
      return showErr
     }


}

  private object DOMXMLScoped {
    def makeXMLtree(tree: Tree)(implicit outer: xml.NamespaceBinding): Option[(xml.NamespaceBinding, Tree)] = tree match {
      case q"""
             var $$tmpscope: ${XML()}.NamespaceBinding = ${DDScope()}
             ..$scopes
             ${SynBlock(q"val $$scope: ${XML()}.NamespaceBinding = $$tmpscope" :: last)}
           """ =>
        withRetreat { retreat =>
          Some((scopes.foldLeft[xml.NamespaceBinding](outer) {
            case (ns, q"$$tmpscope = new ${XML()}.NamespaceBinding(${Str(prefix)}, ${uri: String}, $$tmpscope)") =>
              xml.NamespaceBinding(prefix, uri, ns)
            case _ =>
              retreat()
          }, q"..$last"))
        } {
          Some((outer, tree))
        }
      case q"..$stats" =>
        Some((outer, q"..$stats"))
    }
  }

  private object AttributeList {
    def unapply(tree: Tree)(implicit outer: xml.NamespaceBinding): Option[(xml.MetaData, Tree)] = tree match {
      case q"""
             var $$md: ${XML()}.MetaData = ${XML()}.Null
             ..$attributes
             $last
            """ =>
        withRetreat { retreat =>
          Some((attributes.foldLeft[xml.MetaData](xml.Null) {
            case (md, q"$$md = new ${XML()}.UnprefixedAttribute(${key: String}, ${value: xml.Node}, $$md)") =>
              new xml.UnprefixedAttribute(key, value, md)
            case (md, q"$$md = new ${XML()}.UnprefixedAttribute(${key: String}, $expr, $$md)") =>
              new xml.UnprefixedAttribute(key, Unquote(expr), md)
            case (md, q"$$md = new ${XML()}.PrefixedAttribute(${pre: String}, ${key: String}, ${value: xml.Node}, $$md)") =>
              new xml.PrefixedAttribute(pre, key, value, md)
            case (md, q"$$md = new ${XML()}.PrefixedAttribute(${pre: String}, ${key: String}, $expr, $$md)") =>
              new xml.PrefixedAttribute(pre, key, Unquote(expr), md)
            case _ =>
              retreat()
          }, last))
        } {
          Some((xml.Null, tree))
        }
      case q"..$stats" =>
        Some((xml.Null, q"..$stats"))
    }
  }

 
  implicit val TSNodeText : TSNodes[xml.Atom[String]] {
    case ImplementLiftablesPCData(pcdata)     => pcdata
    case ImplementLiftablesText(text)         => text
    case ImplementLiftablesUnparsed(unparsed) => unparsed
  }

  implicit val TSRootNode: TSNodes[xml.SpecialNode] = ImplementLiftablesable[xml.SpecialNode] {
    case ImplementLiftablesAtom(atom)           => atom
    case ImplementLiftablesComment(comment)     => comment
    case ImplementLiftablesProcInstr(procinstr) => procinstr
    case ImplementLiftablesEntityRef(entityref) => entityref
  }

  implicit def ImplementLiftablesNode(implicit outer: xml.NamespaceBinding = xml.TopScope): ImplementLiftablesable[xml.Node] = ImplementLiftablesable[xml.Node] {
    case q"${elem: xml.Elem}"     => elem
    case ImplementLiftablesSpecialNode(snode) => snode
  }
}
