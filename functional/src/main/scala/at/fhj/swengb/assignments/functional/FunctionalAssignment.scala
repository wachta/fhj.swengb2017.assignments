package at.fhj.swengb.assignments.functional

/**
  * In this assignment you have the chance to demonstrate basic understanding of
  * functions like map/filter/foldleft a.s.o.
  **/
object FunctionalAssignment {

  /**
    * A function which returns its parameters in a changed order. Look at the type signature.
    */
  def flip[A, B](t: (A, B)): (B, A) = t.swap

  /**
    * given a Seq[A] and a function f : A => B, return a Seq[B]
    */
  def unknown[A, B](as: Seq[A], fn: A => B): Seq[B] = as.map(fn)

  /**
    * Returns the absolute value of the parameter i.
    *
    * @param i a value, either with a positive or a negative sign.
    * @return
    */
  def abs(i: Int): Int = if (i >= 0) i else (-1 * i)


  // Describe with your own words what this function does.
  // in the comment below, add a description what this function does - in your own words - and give
  // the parameters more descriptive names.
  //
  // Afterwards, compare your new naming scheme with the original one.
  // What did you gain with your new names? What did you loose?
  //
  /**
    * Function uses foldl of list which means:
    *   From left to right op iteratates through list of elements and applies a function 'func' on it.
    *   This function  produces a result wich is the start value of the next iteration path.
    *   At the end the produces result is returned.
    *
    *   Parameters of op:
    *       -> list   => List of alements
    *       -> acc    => Start value of accumulator
    *       -> func   => Function which takes an element from list and accumulator and produces a result.
    *                    This result is returned and stored as new accumulator for next iteration run
    *
    * @param list     => List with elements of 'oldVal'. This elements get changed by function func to 'new' which is
    *                    the same type as acc.
    * @param acc      => Accumulator for foldl. Start value for fist run
    * @param func     => Function which gets get current element of list and accumulator. This function get applied to
    *                    element of list and produces a new accumulator
    * @tparam listVal => Type of list elements
    * @tparam accVal  => Type of accumulator. Defines expected return type of function 'func' wich is the new
    *                    accumultor for the next run (or final return value if iteration is done)
    * @return         => Function return type of accVal
    *
    * Gained with new naming convention:
    *   --> Easier reading because of meaningfull parameter names (except as => list)
    */
  def op[listVal, accVal](list: Seq[listVal], acc: accVal)(func: (accVal, listVal) => accVal): accVal = list.foldLeft(acc)(func)

  /**
    * implement the summation of the given numbers parameter.
    * Use the function 'op' defined above.
    *
    * @param numbers
    * @return
    */
  def sum(numbers: Seq[Int]): Int = op(numbers,0)( (x: Int, y: Int) => x + y)

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
  def fact(i: Int): Int = if (i == 0) 1 else  (fact(i-1) * i )

  /**
    * compute the n'th fibonacci number
    *
    * hint: use a internal loop function which should be written in a way that it is tail
    * recursive.
    *
    * https://en.wikipedia.org/wiki/Fibonacci_number
    */
  def fib(n: Int): Int = n match {
    case n if (n <= 0) => 0
    case n if (n <= 2) => 1
    case _ => fib(n-1) + fib(n-2)
  }

  /**
    * Implement a isSorted which checks whether an Array[A] is sorted according to a
    * given comparison function.
    *
    * Implementation hint: you always have to compare two consecutive elements of the array.
    * Elements which are equal are considered to be ordered.
    */
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    if( as.length < 2)
      /*List to small to check: list with 0 or 1 element is definetly sorted */
      true
    else {
      /*Ok, we have to deal with a longer list -> check for sorting */
      def sortIter(i: Int): Boolean = {
        if (i >= as.length -1)
          /*Looped over all items found nothing weired -> all elements sorted we're done*/
          true
        else
          if (gt(as(i),as(i+1)))
            sortIter(i+1) /*Looks good, check next... */
          else
            false /*Ha- There it is! => unsorted elements found! */
      }
      sortIter(0)
    }
  }

  /**
    * Takes both lists and combines them, element per element.
    *
    * If one sequence is shorter than the other one, the function stops at the last element
    * of the shorter sequence.
    */
  def genPairs[A, B](as: Seq[A], bs: Seq[B]): Seq[(A, B)] = as.zip(bs)

  // a simple definition of a linked list, we define our own list data structure
  sealed trait MyList[+A]  /* +A means that List can add all types of classes from a type. In our example Int */

  case object MyNil extends MyList[Nothing]

  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  // the companion object contains handy methods for our data structure.
  // it also provides a convenience constructor in order to instantiate a MyList without hassle
  object MyList {

    def sum[Int] (list: MyList[Int]): Int = list match {
      case MyNil => 0 /*Empty List*/
      case Cons(h,t) => h + sum(t) /*Default case: calc sum with recursive call*/
    }

    def product[Int](list: MyList[Int]): Int = list match {
      case MyNil => 1 /*Emtpy List - or end of list reached */
      case Cons(0,_) => 0 /* Oh we've to deal with a 0 -> stop recursion we can predict the result already */
      case Cons(h,t) => h * (product(t)) /*Default case: calc product - When recursion reaches end, it get multiplied with 1 */
    }

    /*That's simple the constructor for MyList */
    def apply[A](as: A*): MyList[A] = {
      as match {
        case Nil => MyNil
        case h :: tl => Cons(h, apply(tl: _*))
      }
    }

  }

}

