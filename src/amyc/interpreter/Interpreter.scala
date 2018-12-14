package amyc
package interpreter

import scala.language.implicitConversions
import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

import scala.collection.mutable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  type Lazy = () => Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), List[LazyValue] => Value] = Map(
      ("Std", "printInt") -> { args => println(args.head().asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head().asString); UnitValue },
      ("Std", "readString") -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt") -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString") -> { args => StringValue(args.head().asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head().asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined

    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name

    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    def interpret(expr: Expr)(implicit locals: mutable.Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) =>
          val v = locals(name)
          v match {
            case l: LazyValue =>
              val value = l()
              locals.update(name, value)
              value
            case _ => v
          }

        case IntLiteral(i) =>
          IntValue(i)

        case BooleanLiteral(b) =>
          BooleanValue(b)

        case StringLiteral(s) =>
          StringValue(s)

        case UnitLiteral() =>
          UnitValue

        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)

        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)

        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)

        case Div(lhs, rhs) =>
          val r = interpret(rhs).asInt
          if (r != 0) IntValue(interpret(lhs).asInt / interpret(rhs).asInt)
          else ctx.reporter.fatal("Division by 0")

        case Mod(lhs, rhs) =>
          val r = interpret(rhs).asInt
          if (r != 0) IntValue(interpret(lhs).asInt % interpret(rhs).asInt)
          else ctx.reporter.fatal("Modulo by 0")

        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)

        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)

        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)

        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)

        case Equals(lhs, rhs) => (interpret(lhs), interpret(rhs)) match {
          case (r: StringValue, l: StringValue) => BooleanValue(r.eq(l))
          case (r: CaseClassValue, l: CaseClassValue) => BooleanValue(r.eq(l))
          case (x, y) => BooleanValue(x == y)
        }
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString ++ interpret(rhs).asString)

        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)

        case Neg(e) =>
          IntValue(-interpret(e).asInt)

        case Call(qname, args) =>
          val interpretedArgs = args.map(arg => LazyValue(interpret(arg)))
          if (isConstructor(qname)) CaseClassValue(qname, interpretedArgs)
          else {
            val funOwner = findFunctionOwner(qname)
            if (builtIns.get((funOwner, qname.name)).isDefined) builtIns((funOwner, qname.name))(interpretedArgs)
            else {
              val fun = findFunction(funOwner, qname.name)
              val newLocals: mutable.Map[Name, Value] = mutable.Map() ++ fun.paramNames.zip(interpretedArgs).toMap
              interpret(fun.body)(newLocals)
            }
          }

        case Sequence(e1, e2) =>
          interpret(e1)
          interpret(e2)

        case Let(df, value, body) =>
          val newLocals: mutable.Map[Identifier, Value] = locals + (df.name -> LazyValue(interpret(value)))
          interpret(body)(newLocals)

        case Ite(cond, thenn, elze) =>
          if (interpret(cond).asBoolean) interpret(thenn)
          else interpret(elze)

        case Match(scrut, cases) =>
          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                Some(List())
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if (i1 == i2) Some(List())
                else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if (b1 == b2) Some(List())
                else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(List())
              case (LazyValue(l), _) =>
                matchesPattern(l(), pat)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                if (con1.name.equals(con2.name)) {
                  if (realArgs.isEmpty || formalArgs.isEmpty) Some(List())
                  else {
                    val r = realArgs.head
                    val f = formalArgs.head
                    if (realArgs.size == 1 && formalArgs.size == 1) matchesPattern(r, f)
                    else {
                      val headPat = matchesPattern(r, f)
                      val tailPat = matchesPattern(CaseClassValue(con1, realArgs.tail), CaseClassPattern(con2, formalArgs.tail))
                      if (headPat.isDefined && tailPat.isDefined) Some(headPat.get ++ tailPat.get)
                      else None
                    }
                  }
                } else None
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) =>
          ctx.reporter.fatal(msg)
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(mutable.Map())
    }
  }

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i

    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b

    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
      case LazyValue(f) => f.toString()
    }
  }

  case class IntValue(i: Int) extends Value

  case class BooleanValue(b: Boolean) extends Value

  case class StringValue(s: String) extends Value

  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  case class LazyValue(private val f: Lazy) extends Value {
    val expr: Lazy = force(f)

    def apply(): Value = expr()

    def force(f: Lazy): Lazy = {
      var evaluated: Boolean = false
      var value: Value = UnitValue
      () => {
        if (!evaluated) {
          value = f()
          evaluated = true
        }
        value
      }
    }
  }

  implicit def v2l(v: => Value): Lazy = () => v

  case object UnitValue extends Value
}
