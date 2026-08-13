import scala.meta.internal.semanticdb.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object SemanticDbReader {

  def read(root: Path): Seq[TextDocument] = {

    Files
      .walk(root)
      .iterator()
      .asScala
      .filter(_.toString.endsWith(".semanticdb"))
      .flatMap { path =>
        try {
          val documents =
            TextDocuments.parseFrom(
              Files.readAllBytes(path)
            )

          documents.documents

        } catch {
          case e: Exception =>
            System.err.println(
              s"Failed to read $path: ${e.getMessage}"
            )
            Seq.empty
        }
      }
      .toSeq
  }
}