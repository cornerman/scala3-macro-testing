package test

import scala.quoted.*

trait Thing {
  type Type
}

object MyMacro {
  transparent inline def genType[G[_[_]], F[_]]: Thing = ${MyMacro.genTypeImpl[G, F]}

  def genTypeImpl[G[_[_]]: Type, F[_]: Type](using Quotes): Expr[Thing] = {
    import quotes.reflect.*

    '{
      class MyThing(x: F[Int])

      new Thing {
        type Type = MyThing
      }
    }
  }

  def foo() = 1
  inline def router[T, R](t: T): Map[String, () => R] = ${MyMacro.routerImpl[T, R]('t)}
  inline def client[T, R](r: () => R): Any = ${MyMacro.clientImpl[T, R]('r)}
  inline def newInstance(): Any = ${MyMacro.newInstanceImpl}


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
  def checkMethod[R: Type](using q: Quotes)(method: quotes.reflect.Symbol): Option[String] = {
    val isExpectedReturnTypeFun = isExpectedReturnType[R]

    Option.when(method.paramSymss.headOption.exists(_.exists(_.isType)))(s"Method ${method.name} has a generic type parameter, this is not supported") orElse
      Option.when(!isExpectedReturnTypeFun(method))(s"Method ${method.name} has unexpected return type")
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
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val expr = '{
      val value = ${t}
      _root_.scala.collection.immutable.Map.from[String, () => R](${Expr.ofSeq(
        methods.map { method =>
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

    // println(expr.show)

    expr
  }

  def newInstanceImpl(using Quotes): Expr[Any] = {
    import quotes.reflect.*


    println("HH")
    val expr = Apply(Select.unique(New(TypeIdent(Symbol.requiredClass("Test"))), "<init>"), Nil)
    println("HH")
    println(expr.show)
    expr.asExpr
  }

  def clientImpl[T: Type, R: Type](r: Expr[() => R])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    def resolveThis = {
      var sym = Symbol.spliceOwner  // symbol of method where the macro is expanded
      while sym.owner != null && !sym.isClassDef do
        sym = sym.owner  // owner of a symbol is what encloses it: e.g. enclosing method or enclosing class
      sym
    }

  val tf = new TreeMap {
        override def transformTree(tree: Tree)(owner: Symbol): Tree = {
          println(s"Visit tree: ${tree.getClass}")
          super.transformTree(tree)(owner)
        }

        override  def transformTerm(tree: Term)(owner: Symbol): Term = {
          println(s"Visit term: ${tree.getClass}")
          tree match {
            // case New(ident) =>
            //   New(TypeIdent(Symbol.classSymbol("Test")).changeOwner(owner))
            case Block(stats, Typed(expr, tpt)) =>
              // super.transformTerm(Block.copy(tree)(transformStats(stats)(owner), expr))(owner)
              super.transformTerm(Block.copy(tree)(transformStats(stats)(owner), Typed(expr, TypeTree.of[T])))(owner)

            case other =>
              super.transformTerm(tree)(owner)
          }
        }
        override def transformStatement(tree: Statement)(owner: Symbol): Statement = {
          println(s"Visit statement: ${tree.getClass}")
          tree match {
            case c @ ClassDef(name, cstr, parents, self, body) =>
              println('{ class Foo }.asTerm)
              // val constr = DefDef(Symbol.newMethod(c.symbol, cstr.name, cstr.returnTpt.tpe, cstr.symbol.flags, Symbol.noSymbol), _ => None)
              val symbol = Symbol.classSymbol("Test")
              val constr = DefDef(Symbol.newMethod(symbol, "<init>", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol), _ => None).changeOwner(symbol)
              // println(cstr)
              // println(constr)
              ClassDef.copy(c)("Test", cstr, List(TypeTree.of[T]), self, body)
            case other =>
              super.transformStatement(tree)(owner)
          }
        }

  }

    val apiType = TypeRepr.of[T]
    val tree = TypeTree.of[T]

    val methods = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val clsSymbol = Symbol.newClass(Symbol.spliceOwner, "Bar", List(TypeRepr.of[T]))
    val clsConstructor = Symbol.newDefaultConstructor(clsSymbol)//.entered
    // val clsConstructor = Symbol.newMethod(clsSymbol, "<init>", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)

    val body = methods.map { method =>
      // val newMethod = Symbol.newMethod(clsSymbol, method.name, TypeRepr.of[R], Flags.Override, method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol))
      val newMethod = Symbol.newMethod(clsSymbol, method.name, TypeRepr.of[R], Flags.EmptyFlags, method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol))
      // val newMethod = method.tree.changeOwner(method).symbol
      // DefDef(newMethod, _ => Some('{${r}()}.asTerm))
      DefDef(newMethod, _ => Some(Literal(IntConstant(1))))
      // val Inlined(_, _, Block(List(defdef), _)) = '{
      //   def run: Int = 1
      // }.asTerm

      // defdef
    }
    val classDef = ClassDef(clsSymbol, clsConstructor /*DefDef(clsConstructor, _ => None)*/, body)

    // val exprX = '{ new {} }
    // val transformed = tf.transformTree(exprX.asTerm)(Symbol.spliceOwner)
    // println(transformed.show)

    // create a new ClassDef workaround
    // val Inlined(_, _, Block((otherClass: ClassDef) :: _, Typed(Apply(Select(New(otherIdent), constructorName), _), typed))) = '{class Foo extends Dynamic; new Foo}.asTerm.changeOwner(Symbol.spliceOwner)
    // val typeDef = TypeDef(Symbol.classSymbol("Bar"))
    // val typeDef = TypeDef(otherClass.symbol)
    // val classDef = otherClass
    // val constr = DefDef(Symbol.newMethod(Symbol.spliceOwner, "<init>", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol), _ => Some('{()}.asTerm))
    val Inlined(_, _, Block((otherClass: ClassDef) :: _, _)) = '{class Foo }.asTerm
    otherClass.symbol
    val constr = DefDef(Symbol.newMethod(Symbol.spliceOwner, "<init>", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol), _ => None)
    // val typeDef = TypeDef(Symbol.newClass(Symbol.spliceOwner, "Bar"))
    // val typeDef = TypeDef(constr.symbol)
    // println(typeDef.getClass)
    // val classDef = ClassDef.copy(typeDef)("Bar", constr, List(TypeTree.of[T]), None, Nil)
    // val classDef = ClassDef("Bar", constr, List(TypeTree.of[T]), None, body)

    // val realIdent = TypeIdent(classDef.symbol)
    val ident = TypeIdent(Symbol.classSymbol("Bar"))
    // val ident = realIdent
    // val ident = Ref.term(TermRef(This(resolveThis).tpe, "Bar")).asInstanceOf[TypeTree]
    // val ident = Ref.term(TermRef(This(Symbol.noSymbol).tpe, "Bar")).asInstanceOf[TypeTree]
    // println(Symbol.spliceOwner.owner.owner.owner.declarations)
    // println(resolveThis.symbol.declarations)

    // println(Symbol.spliceOwner)
    // println(Select.unique(resolveThis, "Bar"))
    // println(Ref.term(TermRef(resolveThis.tpe, "Bar")))

    // println(TypeIdent(classDef.symbol))
    // println(TypeIdent(Symbol.classSymbol("Bar")))
    // println(TypeIdent(Symbol.classSymbol(classDef.symbol.name)))
    // println(TypeIdent(Symbol.requiredClass("Bar")))
    // println(TypeIdent(Symbol.requiredClass(classDef.symbol.name)))
    // // println(Ident.copy(otherIdent)("Bar"))
    // // println(otherIdent)
    // println(realIdent)
    // println(ident)
    // println(TypeIdent(classDef.symbol).tpe)

    val expr = Block(
      List(classDef),
      // Typed(Apply(Select.unique(New(TypeIdent(classDef.symbol)), "<init>"), Nil), TypeTree.of[T])
      // Apply(Select.unique(New(TypeIdent(classDef.symbol)), "<init>"), Nil)
      '{???}.asTerm
    )
    val expr2 = '{
      class Bar() extends test.Command {
        def run: Int = 1
      }
      ???
    }.asTerm
    // val expr = ClassDef.anon(List(TypeTree.of[T]), None, body)

    println(expr2)
    println(expr2.show)
    println(expr)
    println(expr.show)

    expr.asExprOf[T]
    // expr2.asExprOf[T]
    // '{${expr}.asInstanceOf[T]}
    // transformed.asExpr
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
// Block(
//   List(
//     TypeDef(
//       Bar,
//       Template(
//         DefDef(<init>,List(List()),TypeTree[TypeRef(NoPrefix,class Bar)],EmptyTree),
//         List(
//           Apply(Select(New(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class java)),object lang),Object)]),<init>),List()),
//           TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class test)),class Command)]
//         ),
//         ValDef(_,EmptyTree,EmptyTree),
//         List(
//           DefDef(run,List(),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],Inlined(Ident(MyMacro$),List(),Ident(???)))
//         )
//       )
//     )
//   ),
//   Inlined(Ident(MyMacro$),List(),Ident(???))
// )
