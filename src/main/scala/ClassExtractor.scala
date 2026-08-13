import scala.meta.internal.semanticdb.*

case class MethodData(
                       name: String,
                       parameters: List[String],
                       returnType: String
                     )

case class ClassData(
                      name: String,
                      kind: String,
                      parameters: List[String],
                      methods: List[MethodData]
                    )

object ClassExtractor {

  def extract(documents: Seq[TextDocument]): Seq[ClassData] = {

    val symbols =
      documents
        .flatMap(_.symbols)
        .map(symbol => symbol.symbol -> symbol)
        .toMap

    val classes =
      documents
        .flatMap(_.symbols)
        .filter(isClassLike)
        .groupBy(_.symbol)
        .values
        .map(_.head)
        .toSeq

    classes.map { classInfo =>
      extractClass(classInfo, symbols)
    }
  }
  
  private def isClassLike(
                           symbol: SymbolInformation
                         ): Boolean = {
    symbol.kind match {
      case SymbolInformation.Kind.CLASS =>
        true

      case SymbolInformation.Kind.OBJECT =>
        !isGeneratedCaseClassCompanion(symbol)

      case SymbolInformation.Kind.TRAIT =>
        true

      case _ =>
        false
    }
  }

  private def isGeneratedCaseClassCompanion(
                                             symbol: SymbolInformation
                                           ): Boolean = {
    symbol.symbol.endsWith(".") &&
      symbol.displayName.nonEmpty &&
      symbol.properties == 0
  }

  private def extractClass(
                            classInfo: SymbolInformation,
                            symbols: Map[String, SymbolInformation]
                          ): ClassData = {

    val classSymbol = classInfo.symbol

    val constructorParameterNames =
      extractConstructorParameterNames(classInfo, symbols)

    val methods =
      symbols.values
        .filter(symbol => isMethodOf(symbol, classSymbol))
        .filter(symbol => !isConstructor(symbol))
        .filter(symbol =>
          !isCaseClassGeneratedMethod(
            symbol.displayName,
            classInfo,
            constructorParameterNames
          )
        )
        .flatMap(symbol => extractMethod(symbol, symbols))
        .toList

    ClassData(
      name = classInfo.displayName,
      kind = classKind(classInfo),
      parameters = constructorParameterNames,
      methods = methods
    )
  }

  private def classKind(
                         symbol: SymbolInformation
                       ): String = {
    symbol.kind match {
      case SymbolInformation.Kind.CLASS
        if isCaseClass(symbol) =>
        "case class"

      case SymbolInformation.Kind.CLASS =>
        "class"

      case SymbolInformation.Kind.OBJECT =>
        "object"

      case SymbolInformation.Kind.TRAIT =>
        "trait"

      case _ =>
        "unknown"
    }
  }

  private def isCaseClass(
                           symbol: SymbolInformation
                         ): Boolean = {
    (symbol.properties & SymbolInformation.Property.CASE.value) != 0
  }

  private def isConstructor(
                             symbol: SymbolInformation
                           ): Boolean = {
    symbol.kind == SymbolInformation.Kind.CONSTRUCTOR
  }

  private def isMethodOf(
                          symbol: SymbolInformation,
                          classSymbol: String
                        ): Boolean = {
    symbol.kind == SymbolInformation.Kind.METHOD &&
      symbol.symbol.startsWith(classSymbol)
  }

  private def extractConstructorParameterNames(
                                                classInfo: SymbolInformation,
                                                symbols: Map[String, SymbolInformation]
                                              ): List[String] = {

    classInfo.signature match {
      case ClassSignature(_, _, _, Some(scope)) =>
        scope.symlinks
          .flatMap(symbols.get)
          .filter(_.kind == SymbolInformation.Kind.CONSTRUCTOR)
          .flatMap(constructor =>
            extractParameterNames(constructor, symbols)
          )
          .toList

      case _ =>
        Nil
    }
  }

  private def extractParameterNames(
                                     constructor: SymbolInformation,
                                     symbols: Map[String, SymbolInformation]
                                   ): List[String] = {

    constructor.signature match {
      case MethodSignature(_, parameterLists, _, _) =>
        parameterLists
          .flatMap(_.symlinks)
          .flatMap(symbols.get)
          .map(_.displayName)
          .toList

      case _ =>
        Nil
    }
  }

  private def extractMethod(
                             symbol: SymbolInformation,
                             symbols: Map[String, SymbolInformation]
                           ): Option[MethodData] = {

    symbol.signature match {
      case MethodSignature(_, parameterLists, returnType, _) =>

        val parameters =
          parameterLists
            .flatMap(_.symlinks)
            .flatMap(symbols.get)
            .map { parameter =>
              s"${parameter.displayName}: ${parameterType(parameter)}"
            }
            .toList

        Some(
          MethodData(
            name = symbol.displayName,
            parameters = parameters,
            returnType = typeName(returnType)
          )
        )

      case _ =>
        None
    }
  }

  private def parameterType(
                             parameter: SymbolInformation
                           ): String = {

    parameter.signature match {
      case ValueSignature(tpe) =>
        typeName(tpe)

      case _ =>
        "?"
    }
  }

  private def typeName(
                        tpe: Type
                      ): String = {

    tpe match {

      case TypeRef(_, symbol, arguments) =>
        val name = symbolName(symbol)

        if (arguments.isEmpty) {
          name
        } else {
          s"$name[${arguments.map(typeName).mkString(", ")}]"
        }

      case ThisType(symbol) =>
        symbolName(symbol)

      case ConstantType(constant) =>
        constant.toString

      case _ =>
        "?"
    }
  }

  private def symbolName(
                          symbol: String
                        ): String = {
    symbol
      .stripSuffix("#")
      .stripSuffix(".")
      .split("/")
      .lastOption
      .getOrElse(symbol)
  }

  private def isCaseClassGeneratedMethod(
                                          name: String,
                                          classInfo: SymbolInformation,
                                          constructorParameterNames: List[String]
                                        ): Boolean = {

    if (!isCaseClass(classInfo)) {
      false
    } else {
      name match {
        case "copy" =>
          true

        case "apply" =>
          true

        case "unapply" =>
          true

        case name if name.startsWith("copy$default$") =>
          true

        case name if name.matches("_\\d+") =>
          true

        case name if constructorParameterNames.contains(name) =>
          true

        case "canEqual" =>
          true

        case "equals" =>
          true

        case "hashCode" =>
          true

        case "toString" =>
          true

        case "productArity" =>
          true

        case "productElement" =>
          true

        case "productIterator" =>
          true

        case "productPrefix" =>
          true

        case "productElementName" =>
          true

        case "productElementNames" =>
          true

        case _ =>
          false
      }
    }
  }
}