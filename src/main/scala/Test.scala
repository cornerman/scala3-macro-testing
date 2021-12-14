package test

import scala.quoted.*

object MyMacro {
  inline def router[T, R](t: T): Map[String, () => R] = ${MyMacro.routerImpl[T, R]('t)}
  inline def client[T, R](r: () => R): T = ${MyMacro.clientImpl[T, R]('r)}

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

  def isExpectedReturnType[R: Type](using Quotes): quotes.reflect.Symbol => Boolean = { method =>
    import quotes.reflect.*

    val expectedReturnType = TypeRepr.of[R]

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

  ///TODO no overloads
  def isValidMethod[R: Type](using q: Quotes): quotes.reflect.Symbol => Boolean = { method =>
    val isExpectedReturnTypeFun = isExpectedReturnType[R]

    !method.paramSymss.headOption.exists(_.exists(_.isType)) && //isGeneric
      isExpectedReturnTypeFun(method)
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
    } yield member
  }

  def routerImpl[T: Type, R: Type](t: Expr[T])(using Quotes): Expr[Map[String, () => R]] = {
    import quotes.reflect.*

    val methods = definedMethodsInType[T]
    val validMethods = methods.filter(isValidMethod[R])

    val x = '{
      val value = ${t}
      _root_.scala.collection.immutable.Map.from[String, () => R](${Expr.ofSeq(
        validMethods.map { method =>
          println(method.name)
          val call = Select('{value}.asTerm, method)

          val callWithArgs = method.paramSymss.foldLeft[Term](call)((accum, params) =>
            Apply(accum, params.map(_ => '{null}.asTerm))
          )

          '{
            ${Expr(method.name)} -> { () =>
              ${callWithArgs.asExprOf[R]}
            }
          }
        }
      )})
    }

    println(x.show)

    x
  }

  def clientImpl[T: Type, R: Type](r: Expr[() => R])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val apiType = TypeRepr.of[T]

    val methods = definedMethodsInType[T]
    val validMethods = methods.filter(isValidMethod[R])

    val tree = TypeTree.of[T]

    val aClassDef = '{ class Test }
    val Inlined(_, _, Block(list, _)) = aClassDef.asTerm
    // val aClassDef = '{ new AnyRef {
    //   def foo = 1
    // } }
    val orig = list.head.asInstanceOf[TypeDef].symbol
    val next = Symbol.classSymbol(apiType.typeSymbol.fullName)
    // val x = TypeDef.copy(TypeDef(Symbol.classSymbol("Test")))("Bla", '{}.asTerm).asExpr

    // println(x.asTerm)
    // val x = 
// // Block(
// //   List(
//     TypeDef(
//       $anon,
//       Template(
//         DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),
//         List(
//           Apply(Select(New(Ident(AnyRef)),<init>),List())
//         ),
//         ValDef(_,EmptyTree,EmptyTree),
//         List(
//           DefDef(foo,List(),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Int)],Literal(Constant(1)))
//         )
//       )
//     )
  // ),
  // Typed(
  //   Apply(Select(New(Ident($anon)),<init>),List()),
  //   TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]
  // )
// )

    // val x = '{
    //   // ${TypeDef(
    //   //   $anon,
    //   //   Template(
    //   //     DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),
    //   //     List(Apply(Select(New(Ident(AnyRef)),<init>),List())),
    //   //     ValDef(_,EmptyTree,EmptyTree),
    //   //     List(DefDef(foobar,List(),Ident(Int),Literal(Constant(1))))
    //   //   )
    //   // )}
    //   ${aClassDef.asExpr}
    //   // ${
    //   //   ClassDef
    //   //   .copy(aClassDef)(
    //   //     "Anon",
    //   //     aClassDef.constructor,
    //   //   parents = List.empty,
    //   // selfOpt = None,
    //   // body = Nil
    //   // )
    //   //   .asExpr
    //   // }
// // TypeDef('{}.asTerm)(name.toTypeName, Template.('{}.asTerm)(constr, parents, derived = Nil, selfOpt.getOrElse(tpd.EmptyValDef), body))
    //   // ${New.copy('{def foo = 1}.asTerm.symbol.tree)(TypeTree.of[T]).asExpr}
    //   // new Anon
    //   ()
    // }

    // println(x.asTerm)
    // println(x.asTerm.show)

    ???
  }
}

trait AResult[Warum] {
}

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
  private[test] def NO3 :Int
}

//     TypeDef(
//       $anon,
//       Template(
//         DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),
//         List(Apply(Select(New(Ident(AnyRef)),<init>),List())),
//         ValDef(_,EmptyTree,EmptyTree),
//         List(DefDef(foobar,List(),Ident(Int),Literal(Constant(1))))
//       )
//     )
// Block(
//   List(TypeDef(Boo,Template(DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]),ValDef(_,EmptyTree,EmptyTree),List()))),
//   Block(List(TypeDef($anon,Template(DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]),<init>),List()), Ident(Boo)),ValDef(_,EmptyTree,EmptyTree),List()))),Typed(Typed(Apply(Select(New(Ident($anon)),<init>),List()),TypeTree[TypeRef(NoPrefix,trait Boo)]),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)])))


// Block(
//   List(
//     // TypeDef(Foos,Template(DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]),ValDef(_,EmptyTree,EmptyTree),List())),
//     TypeDef(Bars,Template(DefDef(<init>,List(List()),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Unit)],EmptyTree),List(Apply(Select(New(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]),<init>),List()), Ident(Foos)),ValDef(_,EmptyTree,EmptyTree),List()))
//   ),
//   Literal(Constant(()))
// )
