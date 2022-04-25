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
  transparent inline def client[T, R](r: () => R): Any = ${MyMacro.clientImpl[T, R]('r)}
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

  def returnType(using Quotes): quotes.reflect.Symbol => quotes.reflect.TypeRepr = { method =>
    import quotes.reflect.*

    method.tree match {
      case DefDef(_,_,typedTree,_) =>
          TypeRepr.of(using typedTree.tpe.asType)
    //     TypeRepr.of(using typedTree.tpe.asType) match {
    //       case AppliedType(repr, _) => repr
    //       case _ => false
    //     }
      case _ => report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
    }
  }

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


    val expr = Apply(Select.unique(New(TypeIdent(Symbol.requiredClass("Test"))), "<init>"), Nil)
    println(expr.show)
    expr.asExpr
  }

  def clientImpl[T: Type, R: Type](r: Expr[() => R])(using Quotes): Expr[Any] = {
    import quotes.reflect.*

    val returnTypeFun = returnType


    def resolveThis = {
      var sym = Symbol.spliceOwner  // symbol of method where the macro is expanded
      while sym.owner != null && !sym.isClassDef do
        sym = sym.owner  // owner of a symbol is what encloses it: e.g. enclosing method or enclosing class
      sym
    }

    val apiType = TypeRepr.of[T]
    val tree = TypeTree.of[T]

    val methods = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val name = "_Anon"
    val parents = List(TypeTree.of[Object])//, TypeTree.of[T])

    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree match {
        case d: DefDef =>
            val DefDef(name,clauses,typedTree,_) = d.changeOwner(cls)
            val tpeRepr = TypeRepr.of(using typedTree.changeOwner(cls).tpe.asType)
            val names = clauses.flatMap(_.params.collect {
              case v: ValDef =>
                v.name
            })
            val tpes = clauses.flatMap(_.params.collect {
              case v: ValDef => v.tpt.tpe
            })
            clauses.flatMap(_.params.collect {
              case v: TypeDef => v.name
            })
            println(clauses)
            Symbol.newMethod(cls, name, MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Int]), flags = Flags.EmptyFlags /*TODO: method.flags */, privateWithin = Symbol.noSymbol /*TODO: method.privateWithin */)
            // Symbol.newMethod(cls, name, MethodType(names)(_ => tpes, _ => tpeRepr), flags = Flags.EmptyFlags /*TODO: method.flags */, privateWithin = Symbol.noSymbol /*TODO: method.privateWithin */)
        case _ => report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
      }
    }

    val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map { method =>
      DefDef(method, argss => Some('{???}.asTerm))
    }
    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Object])//, TypeTree.of[T])
    Block(List(clsDef), newCls).asExpr //Of[T]
  }
}
