/*
 * Parameters.scala
 *
 * Version:
 *     $Id: Parameters.scala,v 1.3 2011/06/20 22:39:43 kyledewey Exp kyledewey $
 *
 * Revisions:
 *      $Log: Parameters.scala,v $
 *      Revision 1.3  2011/06/20 22:39:43  kyledewey
 *      Refactored how type conversions are performed so that
 *      it is extensible to other kinds of conversions.
 *
 *      Revision 1.2  2011/06/08 04:31:37  kyledewey
 *      Removed the now defunct Data type.
 *      validateChangeParameterType() now uses convertableTypes
 *      to make its judgement.
 *
 *      Revision 1.1  2011/05/31 00:08:17  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import javax.swing._

import sentinel.model._
import ParamType._
import GUIHelpers._

/**
 * Exception thrown when an attempt is made to change from
 * a stronger type to a weaker type.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class ParameterTypeChangeException( message: String )
extends Exception( message ) {
  /**
   * Generates a message based on the given types.
   * @param oldType The original type
   * @param newType The type that we are attempting to change to
   */
  def this( oldType: ParamType, newType: ParamType ) =
    this( "Attempt made to change parameter of type " +
	  ParamType.toString( oldType ) + 
	  " to parameter of incompatible type " +
	  ParamType.toString( newType ) )
}

/**
 * Creates type holders for the given types.
 * @author Kyle Dewey
 */
object TypeHolder {
  // holds created singletons
  private var types: Map[ ParamType, TypeHolder ] = Map()

  /**
   * Gets the singleton for the given type.
   * If it hasn't yet been created, it will create it
   * @param theType The type
   * @return A type holder for the given type
   */
  def apply( theType: ParamType ) = {
    if ( !types.contains( theType ) ) {
      types += Pair( theType, new TypeHolder( theType ) )
    }
    types( theType )
  }
}

/**
 * Holds a type, along with its associated string.
 * Types are ordered by their text type names
 * @param theType The type to store
 * @author Kyle Dewey
 */
class TypeHolder private ( val theType: ParamType ) extends Ordered[ TypeHolder ] {
  val typeString = ParamType.toString( theType )

  /**
   * Prints out the string version of the type.
   * @return <code>typeString</code>
   */
  override def toString() =
    typeString

  /**
   * Compares this type holder to another
   * @param other The other type holder to compare to
   * @return <code>typeString.compare( other.typeString )</code>
   */
  override def compare( other: TypeHolder ) =
    typeString.compare( other.typeString )
}

/**
 * Holds information relevant for type conversions.
 * @author Kyle Dewey
 */
trait TypeConversionValidator {
  // given a type, it returns the types that the type can be converted to
  // more precisely, this shows increasing strength of constraints.  String
  // is the weakest, so it can be converted to most everything else.  Int
  // and matcher are the strongest, so they cannot be converted to anything
  // else
  val convertableTypes = Map( StringType -> Set( StringType,
						 CharType,
						 RealType,
						 IntType,
						 ReplacerType ),
			      IntType -> Set( IntType ),
			      RealType -> Set( RealType,
					       IntType,
					       ReplacerType ),
			      CharType -> Set( CharType,
					       StringType,
					       ReplacerType ),
			      MatcherType -> Set( MatcherType ),
			      ReplacerType -> Set( ReplacerType,
						   StringType ) )

  /**
   * Determines that the given key/value pair is contained in the
   * given map of sets.
   * @param key The key
   * @param value The value to check for existence in the set
   * @param structure The data structure
   * @return true if it is contained, else false
   */
  def contains[ K, V ]( key: K, value: V, structure: Map[ K, Set[ V ] ] ): Boolean =
    structure.contains( key ) &&
    structure( key ).contains( value )

  /**
   * Like <code>contains</code>, only it is for validation.
   * Throws the given exception if it's not contained.
   * @param key The key
   * @param value The value
   * @param structure The data structure
   * @param exception The exception to throw
   */
  def validateContains[ K, V, E <: Exception ]( key: K, 
					        value: V, 
					        structure: Map[ K, Set[ V ] ],
					        exception: => E ) {
    if ( !contains( key, value, structure ) ) {
      throw exception
    }
  }

  /**
   * Validates that the given parameter type change is valid.
   * We can move from data to anything that extends data (string, etc.)
   * We can also move from Real to Integer.  Anything else is invalid
   * @param oldType The old type
   * @param newType the new type
   * @throws ParameterTypeChangeException If the type change is invalid
   */
  def validateChangeParameterType( oldType: ParamType, 
				   newType: ParamType ) {
    validateContains( oldType,
		      newType,
		      convertableTypes,
		      new ParameterTypeChangeException( oldType, newType ) )
  }
}

/**
 * Holds information about parameters, relevant for type conversions.
 * @author Kyle Dewey
 */
object TypeConversions extends TypeConversionValidator {
  /**
   * Given a set of types, it will return a sorted sequence of
   * type holders for them
   * @param types The set of types
   * @return A sorted sequence of the types
   */
  def typeHolders( types: Set[ ParamType ] ) =
    types.map( TypeHolder( _ ) ).toList.sort( _ < _ ).toSeq


  /**
   * Given a type, it will return a combo box holding what types it
   * can be converted to.  Note that if a given item can't be
   * converted to any other types, then the box won't allow for
   * editing.
   * @param theType The type to make a combo box for
   * @return A combo box holding all the types that it can be converted to
   */
  def makeTypeComboBox( theType: ParamType ) = {
    val combo = makeComboBox( typeHolders( convertableTypes( theType ) ) )
    combo.setSelectedItem( TypeHolder( theType ) )
    combo
  }

  /**
   * Makes type combo boxes for all the given param infos
   * @param preFunction The prefunction
   * @return A combo box for each param info object
   */
  def makeTypeComboBoxes( paramInfos: Seq[ ParamInfo ] ) =
    paramInfos.map( ( paramInfo: ParamInfo ) =>
      makeTypeComboBox( paramInfo.paramType ) )
}
