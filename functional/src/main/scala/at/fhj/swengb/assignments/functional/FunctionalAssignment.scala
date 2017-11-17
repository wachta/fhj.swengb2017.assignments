package at.fhj.swengb.assignments.functional

/**
  * In this assignment you have the chance to demonstrate basic understanding of
  * functions like map/filter/foldleft a.s.o.
  **/
object FunctionalAssignment {

  /**
    * A function which returns its parameters in a changed order. Look at the type signature.
    */
  def flip[A, B](t: (A, B)): (B, A) = return((t._2,t._1))

  /**
    * given a Seq[A] and a function f : A => B, return a Seq[B]
    */
  def unknown[A, B](as: Seq[A], fn: A => B): Seq[B] = {
    var ls = Seq[B]()
    for (i <- 0 to (as.length - 1)) {
      ls = ls ++ Seq[B](fn(as(i)))
    }
    return(ls)
  }
  /**
    * Returns the absolute value of the parameter i.
    *
    * @param i a value, either with a positive or a negative sign.
    * @return
    */
  def abs(i: Int): Int = if (i < 0){ return(i*(-1))} else{return(i)}


  // Describe with your own words what this function does.
  // in the comment below, add a description what this function does - in your own words - and give
  // the parameters more descriptive names.
  //
  // Afterwards, compare your new naming scheme with the original one.
  // What did you gain with your new names? What did you loose?
  //
  /**
    *
    * @param as = Liste von Elementen
    * @param b = Startwert von foldleft
    * @param fn = Funktion die 2 Parameter nimmt und folgendermaßen angewandt wird: first = fn(b,as(0)) dann second = fn(first,as(1)) ... solange bis man das Ende der Seq erreicht hat
    * @tparam A = Class A
    * @tparam B = Class B
    * @return = liefert einen Wert zurück
    */
  def op[A, B](as: Seq[A], b: B)(fn: (B, A) => B): B = as.foldLeft(b)(fn)

  /**
    * implement the summation of the given numbers parameter.
    * Use the function 'op' defined above.
    *
    * @param numbers
    * @return
    */
  def sum(numbers: Seq[Int]): Int = op(numbers,0)(_+_)


  /**
    * calculate the factorial number of the given parameter.
    *
    * for example, if we pass '5' as parameter, the function should do the following:
    *
    * 5! = 5 * 4 * 3 * 2 * 1
    *
    * @param i parameter for which the factorial must be calculated
    * @return i!
    */
  def fact(i: Int): Int = {
    if(i>0) {
      var fract = i
      for (x <- 1 to (i - 1)) {
        fract = fract * x
      }
      return(fract)
    }
    else return(0)
  }

  /**
    * compute the n'th fibonacci number
    *
    * hint: use a internal loop function which should be written in a way that it is tail
    * recursive.
    *
    * https://en.wikipedia.org/wiki/Fibonacci_number
    */
  def fib(n: Int): Int = {
    if (n == 0) return(0)
    else {
      if (n == 1) return (1)
      else return (fib(n - 1) + fib(n - 2))
    }
  }

  /**
    * Implement a isSorted which checks whether an Array[A] is sorted according to a
    * given comparison function.
    *
    * Implementation hint: you always have to compare two consecutive elements of the array.
    * Elements which are equal are considered to be ordered.
    */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    var x = 0
    var sorted = true
    while(x < as.length-1){
      sorted = gt(as(x),as(x+1))
      if(!sorted){
        x = as.length
      }
      else {x = x + 1}
    }

    return(sorted)
  }

  /**
    * Takes both lists and combines them, element per element.
    *
    * If one sequence is shorter than the other one, the function stops at the last element
    * of the shorter sequence.
    */
  def genPairs[A, B](as: Seq[A], bs: Seq[B]): Seq[(A, B)] = {
    var x = 1
    var pairs = Seq((as(0),bs(0)))
    while(x<=as.length-1 && x<=bs.length-1){
      pairs = pairs ++ Seq((as(x),bs(x)))
      x = x + 1
    }
    return(pairs)
  }

  // a simple definition of a linked list, we define our own list data structure
  sealed trait MyList[+A]

  case object MyNil extends MyList[Nothing]

  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  // the companion object contains handy methods for our data structure.
  // it also provides a convenience constructor in order to instantiate a MyList without hassle
  object MyList {

    def sum(list: MyList[Int]): Int = ???

    def product(list: MyList[Int]): Int = ???

    def apply[A](as: A*): MyList[A] = {
      if (as.isEmpty) MyNil
      else Cons(as.head, apply(as.tail: _*))
    }

  }

}

