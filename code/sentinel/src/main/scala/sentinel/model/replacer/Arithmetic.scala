/*
 * Arithmetic.scala
 */

package sentinel.model.replacer

import sentinel.model._
import sentinel.model.Replacer._
import sentinel.model.InstanceFactory._

/**
 * Contains helper methods relevant to all arithmetic operations.
 * @author Kyle Dewey
 */
object Arithmetic {
  // note that this will get a tail recursion optimization,
  // so it uses a constant (tiny) amount of stack space
  final def gcd( a: Long, b: Long ): Long = {
    if ( b == 0 ) a
    else gcd( b, a % b )
  }

  // THESE ARE DIFFERENT FROM MATCHER'S!
  def toInts( params: Seq[ Param ] ) =
    params.map( _.sentIntValue )
  def toReals( params: Seq[ Param ] ) =
    params.map( _.sentRealValue )
}

import Arithmetic._

/**
 * Adds a bunch of integers together.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more integers to add together.</li></ul>
 * @author Kyle Dewey
 */
class IntAdd( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "integers", params )

  /**
   * Adds all the given integers together.
   * @returns The result of adding all the integers together
   */
  override def replace() =
    toInts( nums ).reduceLeft( _ + _ )
}

/**
 * Adds a bunch of reals together.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"reals": One or more reals to add together.</li></ul>
 * @author Kyle Dewey
 */
class RealAdd( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "reals", params )

  /**
   * Adds all the given reals together.
   * @returns The result of adding all the reals together
   */
  override def replace() =
    toReals( nums ).reduceLeft( _ + _ )
}

/**
 * Subtracts a bunch of integers.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more integers to subtract.</li></ul>
 * @author Kyle Dewey
 */
class IntSubtract( val className: String,
		   val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "integers", params )

  /**
   * Subtracts all the given integers.
   * For instance, given 1, 2, 3, it returns 1 - 2 - 3
   * @returns The result of subtracting all the integers from each other
   */
  override def replace() =
    toInts( nums ).reduceLeft( _ - _ )
}

/**
 * Subtracts a bunch of reals.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"reals": One or more reals to subtract.</li></ul>
 * @author Kyle Dewey
 */
class RealSubtract( val className: String,
		    val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "reals", params )

  /**
   * Subtracts all the given reals.
   * For instance, given 1, 2, 3, it returns 1 - 2 - 3
   * @returns The result of subtracting all the reals from each other
   */
  override def replace() =
    toReals( nums ).reduceLeft( _ - _ )
}

/**
 * Gets the product of a bunch of integers.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more integers to get the product of.</li></ul>
 * @author Kyle Dewey
 */
class IntMultiply( val className: String,
		   val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "integers", params )

  /**
   * Multiplys all the given integers.
   * For instance, given 1, 2, 3, it returns 1 * 2 * 3
   * @returns The result of multiplying all the integers
   */
  override def replace() =
    toInts( nums ).reduceLeft( _ * _ )
}

/**
 * Gets the product of a bunch of reals.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more reals to get the product of.</li></ul>
 * @author Kyle Dewey
 */
class RealMultiply( val className: String,
		    val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "reals", params )

  /**
   * Multiplys all the given integers.
   * For instance, given 1, 2, 3, it returns 1 * 2 * 3
   * @returns The result of multiplying all the reals
   */
  override def replace() =
    toReals( nums ).reduceLeft( _ * _ )
}

/**
 * Divides a bunch of integers
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more integers to divide, in sequence.</li></ul>
 * @author Kyle Dewey
 */
class IntDivide( val className: String,
		 val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "integers", params )

  /**
   * Divides all the given integers.
   * For instance, given 1, 2, 3, it returns 1 / 2 / 3
   * @returns The result of dividing all the integers
   * @throws ReplaceException If an attempt was made to divide by zero
   */
  override def replace() = {
    try {
      toInts( nums ).reduceLeft( _ / _ )
    } catch {
      case e: ArithmeticException =>
	throw new ReplaceException( e.toString )
    }
  }
}

/**
 * Divides a bunch of reals
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": One or more reals to divide, in sequence.</li></ul>
 * @author Kyle Dewey
 */
class RealDivide( val className: String,
		  val params: Seq[ NamedParam ] ) extends Replacer {
  private val nums = asArray( "reals", params )

  /**
   * Divides all the given reals.
   * For instance, given 1, 2, 3, it returns 1 / 2 / 3
   * @returns The result of dividing all the reals
   * @throws ReplaceException If an attempt was made to divide by zero
   */
  override def replace() = {
    try {
      toReals( nums ).reduceLeft( _ / _ )
    } catch {
      case e: ArithmeticException =>
	throw new ReplaceException( e.getMessage )
    }
  }
}

/**
 * Gets the greatest common divisor of two integers.
 * Note that the implementation is borrowed from:
 * http://snippets.dzone.com/posts/show/2574
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num1": The first integer</li>
 * <li>"num2": The second integer</li></ul>
 * @author Kyle Dewey
 */
class GCD( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num1 = param( "num1", params )
  private val num2 = param( "num2", params )

  /**
   * Gets the gcd of the two numbers.
   * This is the number that can divide both evenly.
   * @returns The gcd of the two numbers
   */
  override def replace() =
    gcd( num1.sentIntValue, num2.sentIntValue )
}

/**
 * Gets the least common multiple if two integers.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num1": The first integer</li>
 * <li>"num2": The second integer</li></ul>
 * @author Kyle Dewey
 */
class LCM( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num1 = param( "num1", params )
  private val num2 = param( "num2", params )

  /**
   * Gets the LCM of two numbers
   * @return The LCM of the two numbers
   * @throws ReplaceException If the GCD was 0, which results in a divide
   * by zero
   */
  override def replace() = {
    val num1Value = num1.sentIntValue
    val num2Value = num2.sentIntValue
    try {
      scala.math.abs( num1Value * 
	        num2Value ) / gcd( num1Value,
				   num2Value )
    } catch {
      case e: ArithmeticException =>
	throw new ReplaceException( "Since the GCD is 0, there is no LCM" )
    }
  }
}

/**
 * Gets the floor of the given real number.
 * This is the greatest integer that is less than
 * or equal to the number.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the floor of</li></ul>
 * @author Kyle Dewey
 */
class Floor( val className: String,
	     val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the floor of the given number.
   * @return The floor
   */
  override def replace() =
    // note that floor returns a double, and we want to
    // return an integer, so we need to convert it
    scala.math.floor( num.sentRealValue ).asInstanceOf[ Long ]
}

/**
 * Gets the ceiling of the given real number.
 * This is the smallest integer that is greater than or
 * equal to the number.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the floor of</li></ul>
 * @author Kyle Dewey
 */
class Ceiling( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the ceiling of the number.
   * @return The ceiling of the number
   */
  override def replace() =
    scala.math.ceil( num.sentRealValue ).asInstanceOf[ Long ]
}

/**
 * Truncates the given real number to an integer.
 * For instance, 3.5 => 3.
 * This is equivalent of casting a double to an int.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to truncate</li></ul>
 * @author Kyle Dewey
 */
class Truncate( val className: String,
	        val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Truncates the number
   * @return The truncated number
   */
  override def replace() =
    num.sentRealValue.asInstanceOf[ Long ]
}

/**
 * Rounds the given real number to an integer.
 * For instance, 3.5 => 4
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to round</li></ul>
 * @author Kyle Dewey
 */
class Round( val className: String,
	     val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Rounds the number
   * @return The rounded number
   */
  override def replace() =
    scala.math.round( num.sentRealValue )
}

/**
 * Gets the modulus of the given numbers.
 * For instance, if given 1, 2, 3, this will return
 * 1, since 1 % 2 == 1, and 1 % 3 == 1.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"integers": The numbers to get the modulus of</li></ul>
 * @author Kyle Dewey
 */
class Mod( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val integers = asArray( "integers", params )

  /**
   * Gets the modulus of the numbers.
   * @return The modulus
   */
  override def replace() =
    toInts( integers ).reduceLeft( _ % _ )
}

class IntMax( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val integers = asArray( "integers", params )
  override def replace() =
    Replacer.max( sentinel.model.matcher.Arithmetic.toInts( integers ) ).longValue
}

class IntMin( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val integers = asArray( "integers", params )
  override def replace() =
    Replacer.min( sentinel.model.matcher.Arithmetic.toInts( integers ) ).longValue
}

class RealMax( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val reals = asArray( "reals", params )
  override def replace() =
    Replacer.max( sentinel.model.matcher.Arithmetic.toReals( reals ) ).doubleValue
}

class RealMin( val className: String,
	       val params: Seq[ NamedParam ] ) extends Replacer {
  private val reals = asArray( "reals", params )
  override def replace() =
    Replacer.min( sentinel.model.matcher.Arithmetic.toReals( reals ) ).doubleValue
}

/**
 * Generates random reals between 0 and 1
 * @param className The name of the class
 * @param params Params to the replacer (doesn't take any)
 * @author Kyle Dewey
 */
class Random( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val random = new scala.util.Random
  /**
   * Returns a random number between 0 and 1.
   * @return a random number beteen 0 and 1
   */
  override def replace() =
    random.nextDouble

  /**
   * Sets that this isn't pure.
   * @return false
   */
  override def isPure() = false
}

/**
 * Gets Euler's number e^num
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"exponent": The exponent for e</li></ul>
 * @author Kyle Dewey
 */
class Exp( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val exp = param( "exponent", params )

  /**
   * Gets e^num
   * @return e^num
   */
  override def replace() =
    scala.math.exp( exp.sentRealValue )
}

/**
 * Gets the given number to a given power
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The base number to raise by an exponent</li>
 * <li>"exponent": The exponent to put on the base number</li></ul>
 * @author Kyle Dewey
 */
class Pow( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )
  private val exp = param( "exponent", params )

  /**
   * Gets base^exponent
   * @return The base number raised to the exponent
   */
  override def replace() =
    scala.math.pow( num.sentRealValue,
	      exp.sentRealValue )
}

/**
 * Gets the logarithm of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the logarithm of<li>
 * <li>"base": The base for the number.  Without one, it will return
 * the natural logarithm of the number</li></ul>
 * @author Kyle Dewey
 */
class Log( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )
  private val base = opParam( "base", params )
  private val getBase: () => Double =
    if ( base.isDefined ) {
      () => scala.math.log( num.sentRealValue ) / scala.math.log( base.get.sentRealValue )
    } else {
      () => scala.math.log( num.sentRealValue )
    }

  /**
   * Gets the natural logarithm of the number
   * @return The natural logarithm of the number
   */
  override def replace() =
    getBase()
}

/**
 * Gets the square root of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the square root of</li></ul>
 * @author Kyle Dewey
 */
class Sqrt( val className: String,
	    val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the square root of the given number.
   * @return The square root of the number
   */
  override def replace() =
    scala.math.sqrt( num.sentRealValue )
}

/**
 * Gets the sine of the given number.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the sine of</li></ul>
 * @author Kyle Dewey
 */
class Sin( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the sine of the given number
   * @return the sine
   */
  override def replace() =
    scala.math.sin( num.sentRealValue )
}

/**
 * Gets the cosine of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the cosine of</li><ul>
 * @author Kyle Dewey
 */
class Cos( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the cosine of the given number
   * @return The cosine
   */
  override def replace() =
    scala.math.cos( num.sentRealValue )
}

/**
 * Gets the tangent of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num" The number to get the tangent of</li><ul>
 * @author Kyle Dewey
 */
class Tan( val className: String,
	   val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the tangent of the given number
   * @return the tangent
   */
  override def replace() =
    scala.math.tan( num.sentRealValue )
}

/**
 * Gets the arc sine of the given number.
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the arc sine of</li></ul>
 * @author Kyle Dewey
 */
class ArcSin( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the arc sine of the given number
   * @return the arc sine
   */
  override def replace() =
    scala.math.asin( num.sentRealValue )
}

/**
 * Gets the arc cosine of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num": The number to get the arc cosine of</li><ul>
 * @author Kyle Dewey
 */
class ArcCos( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the arc cosine of the given number
   * @return The arc cosine
   */
  override def replace() =
    scala.math.acos( num.sentRealValue )
}

/**
 * Gets the arc tangent of the given number
 * @param className The name of the class
 * @param params Params to the replacer
 * <ul><li>"num" The number to get the arc tangent of</li><ul>
 * @author Kyle Dewey
 */
class ArcTan( val className: String,
	      val params: Seq[ NamedParam ] ) extends Replacer {
  private val num = param( "num", params )

  /**
   * Gets the arc tangent of the given number
   * @return the arc tangent
   */
  override def replace() =
    scala.math.atan( num.sentRealValue )
}
