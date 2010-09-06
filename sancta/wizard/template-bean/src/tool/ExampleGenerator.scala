package #{package}

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class #{class}Generator(destDir: String) extends GeneratorBase(destDir) {
  val templates = getTemplates("#{class}.stg")
  
  def generate(tree: SourceCode) {
    val header = tree.header.toJavaMap

    for (bean <- tree.bean) {
      val args = bean.toJavaMap
      args.put("it", header) // merge header
      writeFile(bean.id.text + ".java", templates.getInstanceOf("bean", args))
    }
  }
}

object #{class}Main extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    
    val grammar = new #{class}Grammar()

    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      new #{class}Generator(destDir).generate(grammar.tree)
    }
  }
}
