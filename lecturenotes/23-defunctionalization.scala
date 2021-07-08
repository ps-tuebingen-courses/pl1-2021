/**
Defunctionalization
===================
The material in these notes is based on: John C. Reynolds: Definitional Interpreters for Higher-Order Programming Languages.
Higher-Order and Symbolic Computation 11(4): 363-397 (1998)

In the discussion of syntactic interpretation vs meta interpretation we have learned that we only learn something about (and control)
a language feature if we choose syntactic interpretation.

Today we want to discuss techniques with which we can make our interpreter so syntactic that it corresponds to an abstract machine:
A machine with a (possibly infinite) set of states and a simple transition relation between the states. We already know the technique
with which we can take control over the call stack management: CPS transformation. After CPS-transforming the interpreter, we do not
rely on the order of evaluation and call stack management of the meta language anymore.  We replicate its definition here:
*/

sealed abstract class Exp
case class Num(n : Int) extends Exp
case class Id(name : Symbol) extends Exp
case class Add(lhs : Exp, rhs : Exp) extends Exp
case class Fun(param : Symbol, body : Exp) extends Exp
case class App (funExpr : Exp, argExpr : Exp) extends Exp

sealed abstract class Value
type Env = Map[Symbol, Value]
case class NumV(n : Int) extends Value
case class ClosureV(f : Fun, env : Env) extends Value

object CPSTransformed {
  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) =>
      eval( l, env
          , lv => eval( r, env
                      , rv => (lv, rv) match {
                          case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
                          case _ => sys.error("can only add numbers")
                        } ) )
    case f@Fun(param, body) => k(ClosureV(f, env))
    case App(f, a) =>
      eval( f, env
          , cl => cl match {
              case ClosureV(f, closureEnv) =>
                eval( a, env
                    , av => eval(f.body, closureEnv + (f.param -> av), k) )
              case _ => sys.error("can only apply functions")
            } )
  }
}

/**
However, the CPS-transformed interpreter still uses high-level features of the meta-language, most notably first-class functions.
We will now introduce one transformation that can be used to transform a function using higher-order functions into one using only
first-order functions.  It is general program transformation technique, not restricted only to interpreters.

Lambda Lifting
--------------
The first of these techniques is _lambda lifting_. The goal of lambda lifting is to turn local functions into top-level functions.
That is, all "lambdas" only occur at the top-level. Variables in the local environment that are normally stored in the function's
closure are instead passed as parameters to the top-level function. Lambda lifting is accomplished by the following steps:
 
 1. Invent a new and unique name for each function that is not a top-level function.  
 2. Create a function with this name. Its body is the body of the former local function. Such a function will contain free variables.
 3. Add a parameter to so-obtained top-level function for each free variable in its body. 
    Thus it becomes a higher-order function that returns a function when passed these arguments. 
 4. Replace the local function by a call to the new top-level function and pass the corresponding local context via the arguments
    created in step 3.

Example: Let's lambda-lift the functions ``y => y + n`` and ``y => y*n`` in
*/ 
 
def map(f : Int => Int, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => f(x) :: map(f, xs)
}

def addAndMultNToList(n : Int, xs : List[Int]) = map(y => y * n, map(y => y + n, xs)) 

/** 
We create two new top-level functions. Let's call them ``f`` and ``g`` Their bodies are respectively ``y => y + n`` and ``y => y * n``.
We add a parameter for each free variable. In the example, the free variable is ``n`` in both cases:
*/

def f(n : Int) = (y : Int) => y + n
def g(n : Int) = (y : Int) => y * n

/** 
or shorter: 
*/

def f(n : Int)(y : Int) = y + n
def g(n : Int)(y : Int) = y * n

/**
The local function can now be replaced by a call to the new global function.
*/

def addAndMultNToList(n : Int, xs : List[Int]) = map(g(n), map(f(n), xs)) 

/**
Let's now perform the same technique to the CPS-transformed interpreter given above. It contains local functions in four places:
two in the ``Add`` branch and two in the ``App`` branch. We call the corresponding top-level functions, from left to right,
``addc1``, ``addc2``, ``appc1`` and ``appc2``.
An interesting novelty in the interpreter is that some local functions (corresponding to ``addc1`` and ``appc1``) create local
functions themselves. This means that ``addc1`` must call ``addc2`` and ``appc1`` must call ``appc2``. The rest of the transformation
is a straightforward application of the transformation steps described above:
*/

object LambdaLifted {
  def addc1[T](r : Exp, env : Env, k : Value => T)(lv : Value) = eval(r, env, addc2(lv, k))

  def addc2[T](lv : Value, k : Value => T)(rv : Value) = (lv, rv) match {
    case (NumV(v1), NumV(v2)) => k(NumV(v1 + v2))
    case _ => sys.error("can only add numbers") 
  }

  def appc1[T](a : Exp, env : Env, k : Value => T)(cl : Value) = cl match {
    case ClosureV(f, closureEnv) => eval(a, env, appc2(f, closureEnv, k))
    case _ => sys.error("can only apply functions")
  }

  def appc2[T](f : Fun, closureEnv : Env, k : Value => T)(av : Value) = eval(f.body, closureEnv + (f.param -> av), k)

  def eval[T](e : Exp, env : Env, k : Value => T) : T = e match {
    case Num(n : Int) => k(NumV(n))
    case Id(x) => k(env(x))
    case Add(l, r) => eval(l, env, addc1(r, env, k))
    case f@Fun(param, body) => k(ClosureV(f, env))
    case App(f, a) => eval(f, env, appc1(a, env, k))
  }
}

/**
The lambda-lifted interpreter contains no local functions anymore, but it still contains higher-order functions, since `addc1' etc.
return functions that are passed as parameters to other functions.

Defunctionalization
-------------------
_Defunctionalization_ is a program transformation technique that turns higher-order programs that have already been lambda-lifted
into first-order programs that contain no higher-order functions anymore.  Any program contains only finitely many function definitions.
The idea of defunctionalization is to assign a unique identifier to each of these function definitions. The function "dispatch" then
happens in a function ``apply``, which receives the identifier corresponding to a function definition and dispatches the identifier
to the right function body. Every function application within the program is then replaced by a call to the ``apply`` function with
the function identifier as the first argument.

In addition to the unique identifier, the ``apply`` function also needs bindings for the free variables in the function body.
Hence we need to store the values for these free variables along with the unique identifier. Finally, the ``apply`` function needs
to know about the argument to the function. These become additional parameters of the ``apply`` function.

Let's illustrate defunctionalization in the ``addAndMultNToList`` example from above.
*/

sealed abstract class FunctionValue
case class F(n : Int) extends FunctionValue
case class G(n : Int) extends FunctionValue

def apply(f : FunctionValue, y : Int) : Int = f match {
  case F(n) => y + n
  case G(n) => y * n
}

def map(f : FunctionValue, xs : List[Int]) : List[Int] = xs match {
  case Nil => Nil
  case (x :: xs) => apply(f, x) :: map(f, xs)
}

def addAndMultNToList(n : Int, xs : List[Int]) = map(G(n), map(F(n), xs))
 
/** 
Let's now apply defunctionalization to our CPS-transformed interpreter: 
*/
 
object Defunctionalized {

  sealed abstract class FunctionValue[T]
  case class AddC1[T](r : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class AddC2[T](lv : Value, k : FunctionValue[T]) extends FunctionValue[T]
  case class AppC1[T](a : Exp, env : Env, k : FunctionValue[T]) extends FunctionValue[T]
  case class AppC2[T](f : Fun, closureEnv : Env, k : FunctionValue[T]) extends FunctionValue[T]

  def apply[T](fv : FunctionValue[T], v : Value) : T  = fv match {
    case AddC1(r, env, k) => eval(r, env, AddC2(v, k))
    case AddC2(lv, k) => (lv, v) match {
      case (NumV(v1), NumV(v2)) => apply(k, NumV(v1 + v2))
      case _ => sys.error("can only add numbers") 
    }
    case AppC1(a, env, k) => v match {
      case ClosureV(f, closureEnv) => eval(a, env, AppC2(f, closureEnv, k))
      case _ => sys.error("can only apply functions")
    }
    case AppC2(f, closureEnv, k) => eval(f.body, closureEnv + (f.param -> v), k)
  }

  def eval[T](e : Exp, env : Env, k : FunctionValue[T]) : T = e match {
    case Num(n : Int) => apply(k, NumV(n))
    case Id(x) => apply(k, env(x))
    case Add(l, r) => eval(l, env, AddC1(r, env, k))
    case f@Fun(param, body) => apply(k, ClosureV(f, env))
    case App(f, a) => eval(f, env, AppC1(a, env, k))
  }
}

/**
This interpreter can be seen as an abstract machine. The state space of the abstract machine is
(``Exp`` x ``Env`` x ``FunctionValue``) U (``FunctionValue`` x ``Value``), where "x" stands for cross product and "U" stands for set union.
Every case in the pattern matches in ``apply`` and ``eval`` can be read as a transition in this state space.
*/
