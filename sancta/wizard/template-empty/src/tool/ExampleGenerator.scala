package #{package};

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class #{class}Generator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("#{class}.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
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
