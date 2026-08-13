import java.nio.file.Paths

object ArchitectureGenerator {

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Usage: ArchitectureGenerator <semanticdb-directory>")
      return
    }

    val root = Paths.get(args(0))
    val documents = SemanticDbReader.read(root)
    val classes = ClassExtractor.extract(documents)

    classes.foreach { clazz =>
      println(
        s"${clazz.kind} ${clazz.name}(${clazz.parameters.mkString(", ")})"
      )

      clazz.methods.foreach { method =>
        println(
          s"  ${method.name}(${method.parameters.mkString(", ")}): ${method.returnType}"
        )
      }

      println()
    }
  }

}