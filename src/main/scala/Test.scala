package test

import scala.annotation.experimental
import scala.language.experimental
import scala.quoted.*

@experimental
object MyMacro {

  def isExpectedReturnType[R: Type](using Quotes): quotes.reflect.Symbol => Boolean = { method =>
    import quotes.reflect.*

    val expectedReturnType = TypeRepr.of[R]
    
    method.tree match {
      case DefDef(_,_,typedTree,_) =>
          TypeRepr.of(using typedTree.tpe.asType) <:< expectedReturnType
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

  transparent inline def client[T, I, R](r: I => R): T = ${MyMacro.clientImpl[T, I, R]('r)}

  def clientImpl[T: Type, I: Type, R: Type](r: Expr[I => R])(using Quotes): Expr[T] = {
    import quotes.reflect.*

    val apiType = TypeRepr.of[T]
    val tree = TypeTree.of[T]

    val methods = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }

    val className = "_Anon"
    var parents = List(TypeTree.of[Object], TypeTree.of[T])

    //TODO: use: https://github.com/lampepfl/dotty/pull/15024
    def decls(cls: Symbol): List[Symbol] = methods.map { method =>
      method.tree.changeOwner(cls) match {
        case DefDef(name, clauses, typedTree,_) =>
          val tpeRepr = TypeRepr.of(using typedTree.tpe.asType)

          val names = clauses.flatMap(_.params.collect { case v: ValDef => v.name })
          val tpes = clauses.flatMap(_.params.collect { case v: ValDef => v.tpt.tpe })

          // nullary methods
          val methodType = if (clauses.isEmpty) ByNameType(tpeRepr) else MethodType(names)(_ => tpes, _ => tpeRepr)
          Symbol.newMethod(cls, name, methodType, flags = Flags.EmptyFlags /*TODO: method.flags */, privateWithin = method.privateWithin.fold(Symbol.noSymbol)(_.typeSymbol))
        case _ =>
          report.errorAndAbort(s"Cannot detect type of method: ${method.name}")
      }
    }

    val cls = Symbol.newClass(Symbol.spliceOwner, className, parents.map(_.tpe), decls, selfType = None)
    val body = cls.declaredMethods.map { method => DefDef(method, argss => Some('{${r}(${argss(0)(0).asExprOf[I]})}.asTerm)) }
    val clsDef = ClassDef(cls, parents, body = body)
    val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[T])
    val result = Block(List(clsDef), newCls).asExprOf[T]
    println(result.show)
    result
  }

  transparent inline def router[T, I, R](r: T): Map[String, String => R] = ${MyMacro.routerImpl[T, I, R]('r)}

  def routerImpl[T: Type, I: Type, R: Type](impl: Expr[T])(using Quotes): Expr[Map[String, String => R]] = {
    import quotes.reflect.*

    val apiType = TypeRepr.of[T]
    val tree = TypeTree.of[T]

    val methods = definedMethodsInType[T]
    val invalidMethods = methods.flatMap(checkMethod[R])
    if (invalidMethods.nonEmpty) {
      report.errorAndAbort(s"Invalid methods: ${invalidMethods.mkString(", ")}")
    }


    val methodDefinitions = methods.map { symbol =>
      val lambdaType = MethodType(List("arg1"))(
      _ => List(TypeRepr.of[String]),
      _ => TypeRepr.of[Int],
      )
      
      val lambda = Lambda(Symbol.spliceOwner, lambdaType, { (_, args) =>
        val terms = args.map(_.asExpr.asTerm)
        Apply(Select(impl.asTerm, symbol), terms)
      })

      '{
        (${Expr(symbol.name)}, ${lambda.asExprOf[String => R]})
      }
    }

    val result = '{
      Map.from(${Expr.ofList(methodDefinitions)})
    }

    println(result.show)
    result
  }
}
