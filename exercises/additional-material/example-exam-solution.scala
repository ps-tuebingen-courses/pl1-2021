
// Programmieraufgabe 1

// def eval(e: Exp, env: Env, k: Value => Nothing) : Nothing = e match {
//    ...
//    case Letcc(param,body) => eval(body, env + (param -> ContV(k)), k)
//  }

// Programmieraufgabe 2

def map(xs: List[Int])(f: Int => (Int, Int)): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }

def flatMap(xs: List[Int])(f: Int => List[(Int, Int)]): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => f(x) ++ flatMap(xs)(f)
  }

def product(l1: List[Int], l2: List[Int]) =
  flatMap(l1)(x =>
    map(l2)(y =>
      (x, y)))

// Lambda-Lifted:

def f2(x: Int)(y: Int) = (x, y)
def f1(l2 : List[Int])(x: Int) = map(l2)(f2(x))

def product_ll(l1: List[Int], l2: List[Int]) =
  flatMap(l1)(f1(l2))

// Defunctionalized:

object Defun {

trait FunctionValueIntPair
case class F2(x: Int) extends FunctionValueIntPair

def apply_1(f: FunctionValueIntPair, y: Int) =
  f match {
    case F2(x) => (x, y)
  }

trait FunctionValueListIntPair
case class F1(l2 : List[Int]) extends FunctionValueListIntPair

def apply_2(f: FunctionValueListIntPair, x: Int) =
  f match {
    case F1(l2) => map_defun(l2)(F2(x))
  }

def map_defun(xs: List[Int])(f: FunctionValueIntPair): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply_1(f,x) :: map_defun(xs)(f)
  }

def flatMap_defun(xs: List[Int])(f: FunctionValueListIntPair): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply_2(f,x) ++ flatMap_defun(xs)(f)
  }

def product_defun(l1: List[Int], l2: List[Int]) =
  flatMap_defun(l1)(F1(l2))

}

import Defun._
assert(product(List(1,2,3), List(3,4,5)) == product_defun(List(1,2,3), List(3,4,5)))



// Multiple Choice

// Richtige Antworten:

// 1. Jedes Programm, welches unter call-by-value terminiert, 
// terminiert auch unter call-by-need.

// 2. ... der möglicherweise zuletzt ausgeführte Funktionsaufruf
// in einer Funktion zur Laufzeit.

// 3. ... `(AddressV(1), Map(1 -> NumV(42)))` sein.

// 4. Ein Monadentransformer kann häufig verwendet werden,
// um Monaden miteinander zu kombinieren.
