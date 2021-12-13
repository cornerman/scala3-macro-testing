import scala.quoted.*

object MyMacro {
  inline def call[T, R](t: T, key: String): R = ${MyMacro.nameImpl[T, R]('t, 'key)}

  // def definedMethodsInType(tpe: Type): List[(MethodSymbol, Type)] = for {
  //   member <- tpe.members.toList
  //   if member.isAbstract
  //   if member.isMethod
  //   if member.isPublic
  //   if !member.isConstructor
  //   if !member.isSynthetic
  //   symbol = member.asMethod
  // } yield (symbol, symbol.typeSignatureIn(tpe))

  // def supportedMethodsInType(tpe: Type, expectedReturnType: Type): List[(MethodSymbol, Type)] = {
  //   val methods = definedMethodsInType(tpe)
  //   val validatedMethods = methods.map { case (sym, tpe) => validateMethod(expectedReturnType, sym, tpe) }
  //   val validatedType = eitherSeq(validatedMethods)
  //     .flatMap(methods => eitherSeq(validateAllMethods(methods)))

  //   validatedType match {
  //     case Right(methods) => methods
  //     case Left(errors) => abort(s"type '$tpe' contains unsupported methods: ${errors.mkString(", ")}")
  //   }
  // }

  //private def validateMethod(expectedReturnType: Type, symbol: MethodSymbol, methodType: Type): Either[String, (MethodSymbol, Type)] = for {
  //  _ <- methodType match {
  //    case _: MethodType | _: NullaryMethodType => Valid
  //    case _: PolyType => Invalid(s"method ${symbol.name} has type parameters")
  //    case _ => Invalid(s"method ${symbol.name} has unsupported type")
  //  }
  //  methodResult = methodType.finalResultType.typeConstructor
  //  returnResult = expectedReturnType.finalResultType.typeConstructor
  //  _ <- validate(methodResult <:< returnResult, s"method ${symbol.name} has invalid return type, required: $methodResult <: $returnResult")
  //} yield (symbol, methodType)

  ////TODO rename overloaded methods to fun1, fun2, fun3 or append TypeSignature instead of number?
  //private def validateAllMethods(methods: List[(MethodSymbol, Type)]): List[Either[String, (MethodSymbol, Type)]] =
  //  methods.groupBy(m => methodPathPart(m._1)).map {
  //    case (_, x :: Nil) => Right(x)
  //    case (k, _) => Left(s"""method $k is overloaded (rename the method or add a @PathName("other-name"))""")
  //  }.toList

  trait Upper {
    def theonlyValidForNowInherited: AResult[Int]
  }

  trait Test extends Upper {
    def foo: Int
    def fooWith(): Int
    def foobar(s:String): Int
    def heinz: String

    def NOHeinz: Int = 1

    def theonlyValidForNow: AResult[Int]
    // def theonlyValidForNow2ButNotAbstract: AResult[Int] = null

    def generic[T]: Int

    private def NO: Int = 1
    protected def NO2:Int
    private[MyMacro] def NO3 :Int
  }

  def definedMethodsInType[T: Type](using Quotes): List[quotes.reflect.Symbol] = {
    import quotes.reflect.*
    val tree = TypeTree.of[T]

    for {
      member <- tree.symbol.methodMembers
      //is abstract method, not implemented
      if member.flags.is(Flags.Deferred)

      //TODO: is that public?
      // TODO? if member.privateWithin
      if !member.flags.is(Flags.Private)
      if !member.flags.is(Flags.Protected)
      if !member.flags.is(Flags.PrivateLocal)

      if !member.isClassConstructor
      if !member.flags.is(Flags.Synthetic)
    } yield {
      // println(member)
      println("===")
      println(member.name)
      println(member.flags.show)
      println(member.getClass)
      member
    }
  }

  trait AResult[Warum] {
  }

  def nameImpl[T: Type, R: Type](t: Expr[T], key: Expr[String])(using Quotes): Expr[R] = {
    import quotes.reflect.*

    val Expr(keyString) = key

    val repr = TypeRepr.of[T]

    val expectedReturnType = TypeRepr.of[R]

    val methods = definedMethodsInType[T]

    def typeOfSymbol(method: Symbol) = {
      method.tree match {
        case DefDef(_,_,typedTree,_) => typedTree.tpe.asType
        case _ => ???
      }
    }

    def isCorrectReturnType(method: Symbol) = {
      method.tree match {
        case DefDef(_,_,typedTree,_) =>
            TypeRepr.of(using typedTree.tpe.asType) <:< expectedReturnType
      //     TypeRepr.of(using typedTree.tpe.asType) match {
      //       case AppliedType(repr, _) => repr <:< expectedReturnType
      //       case _ => false
      //     }
        case _ => false
      }
    }

    val validMethods = for {
      method <- methods
      if !method.paramSymss.headOption.exists(_.exists(_.isType)) //isGeneric
      if isCorrectReturnType(method)
      ///TODO no overloads
    } yield (method.name, method)

    val methodMap = validMethods.toMap

    val method = methodMap.get(keyString) match {
      case Some(method) => method
      case _ => ??? //compiletime.error(" not found") //in type '${TypeRepr.of[T].termSymbol.name}': $keyString")
    }
    println(method)


    method.paramSymss


    // val validatedMethods = methods.map { symbol => validateMethod(expectedReturnType, sym, tpe) }
    // val validatedType = eitherSeq(validatedMethods)
    //   .flatMap(methods => eitherSeq(validateAllMethods(methods)))

    // validatedType match {
    //   case Right(methods) => methods
    //   case Left(errors) => abort(s"type '$tpe' contains unsupported methods: ${errors.mkString(", ")}")
    // }

    val sym = Symbol.newMethod(repr.typeSymbol, method.name, TypeRepr.of(using typeOfSymbol(method)))
    println(sym)

    val x = '{
      val value = ${t}
      ${Select('{value}.asTerm, method).asExprOf[R]}
    }

    println(x.show)

    x
  }
}
