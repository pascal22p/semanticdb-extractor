import scala.meta.internal.semanticdb.*

import java.nio.file.Files
import java.nio.file.Path

object InspectSemanticDb {

  def inspect(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Usage: InspectSemanticDb <semanticdb-file>")
      sys.exit(1)
    }

    val path = Path.of(args(0))

    val documents =
      TextDocuments.parseFrom(
        Files.readAllBytes(path)
      )

    documents.documents.foreach { document =>

      println(s"URI: ${document.uri}")

      document.symbols.foreach { symbol =>

        println()
        println(s"SYMBOL: ${symbol.symbol}")
        println(s"KIND: ${symbol.kind}")
        println(s"DISPLAY: ${symbol.displayName}")
        println(s"SIGNATURE CLASS: ${symbol.signature.getClass}")
        println(s"SIGNATURE: ${symbol.signature}")

      }
    }

    println(SymbolInformation.Property.values.toList)
  }
}