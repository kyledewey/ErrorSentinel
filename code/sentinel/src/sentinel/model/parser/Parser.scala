/*
 * Parser.scala
 *
 * Version:
 *     $Id: Parser.scala,v 1.11 2011/06/03 04:13:25 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Parser.scala,v $
 *      Revision 1.11  2011/06/03 04:13:25  kyledewey
 *      Refactored so that dependencies between files are
 *      now resolved properly.
 *
 *      Revision 1.10  2011/06/01 03:59:30  kyledewey
 *      Refactored so that dependencies can now be resolved
 *      between files.
 *
 *      Revision 1.9  2011/04/10 04:05:04  kyledewey
 *      Now uses FactoryManager's regiserFactory method.
 *
 *      Revision 1.8  2011/03/14 11:59:55  kyledewey
 *      Allowed for duplicate classes to overwrite new ones.
 *
 *      Revision 1.7  2010/07/11 05:49:13  kyledewey
 *      Changed implementation of ParseAndRegisterClasses().
 *
 *      Revision 1.6  2010/07/10 21:46:28  kyledewey
 *      Now uses a dependency graph as a guide for parsing, instead
 *      of merely parsing classes in in the order in which they
 *      appear in the file.
 *
 *      Revision 1.5  2010/06/25 03:18:31  kyledewey
 *      Refactored so that variables have types.
 *
 *      Revision 1.4  2010/06/20 17:30:05  kyledewey
 *      Made everything use the more generic
 *      InstanceFactory[ _ ] type instead of the less
 *      flexible and bulkier
 *      Either[ MatcherFactory, ReplacerFactory ].
 *
 *      Revision 1.3  2010/06/18 19:37:06  kyledewey
 *      Made factories take a name and description.
 *
 *      Revision 1.2  2010/06/18 03:09:30  kyledewey
 *      Moved some code to its rightful place in ParamType.
 *
 *      Revision 1.1  2010/06/15 17:56:41  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.parser

import sentinel.model._
import sentinel.model.matcher._
import sentinel.model.replacer._
import java.io._

/**
 * Exception thrown by the class parser when there is an error in the
 * underlying format.
 * @param message A message describing the error
 * @author Kyle Dewey
 */
case class ClassParseException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when an attempt is made to register a class that
 * appears to already have been registered
 * @param message A message describing the error
 * @author Kyle Dewey
 */
case class ClassAlreadyRegisteredException( override val message: String )
     extends ClassParseException( message ) {}

/**
 * Contains routines common to parsers.
 * @author Kyle Dewey
 */
object ClassParser {
  import sentinel.model.ParamType._

  /**
   * Validates that the given class name is that of a matcher
   * @param name The name of the potential matcher
   * @throws ClassParseException If the given class name does not
   *         correlate to that of a matcher
   */
  def validateMatcher( name: String ): Boolean = {
    if ( MatcherFactoryManager.isRegistered( name ) ) true
    else {
      throw new ClassParseException( "Unknown matcher: \"" + name + "\"" )
    }
  }

  /**
   * Validates that the given class name is that of a replacer
   * @param name The name of the potential replacer
   * @return True if it is
   * @throws ClassParseException If the given class name does not
   *         correlate to that of a replacer
   */
  def validateReplacer( name: String ): Boolean = {
    if ( ReplacerFactoryManager.isRegistered( name ) ) true
    else {
      throw new ClassParseException( "Unknown replacer: \"" + name + "\"" )
    }
  }

  /**
   * Given the text of an integer, it will return the corresponding
   * integer.
   * @param text The text of the integer
   * @return The integer
   * @throws ClassParseException If the text couldn't be parsed to an integer
   */
  def textToInteger( text: String ): Int = {
    try {
      Integer.parseInt( text )
    } catch {
      case e: NumberFormatException =>
	throw new ClassParseException( e.getMessage )
    }
  }

  /**
   * Given the text of a spreadsheet, it will return the corresponding
   * spreadsheet.
   * @param text The text of the spreadsheet name
   * @return The spreadsheet name, or NONE if it's a reference to
   * the current spreadsheet
   */
  def textToSpreadsheet( text: String ): Option[ String ] = {
    if ( text == Spreadsheet.ANY_SHEET ) None
    else Some( text )
  }

  /**
   * Given the text of a row, it will return the corresponding row
   * @param text The text of the row
   * @return The row, or None if it's a reference to the current row
   * @throws ClassParseException If an actual row was passed, but it
   * wasn't an integer
   */
  def textToRow( text: String ): Option[ Int ] = {
    if ( text == Spreadsheet.ANY_ROW ) None
    else Some( textToInteger( text ) )
  }

  /**
   * Given the text of a column, it will return the corresponding
   * column.
   * @param text The text of the column
   * @return The column, or None if it's a reference to the current column
   * @throws ClassParseException If an actual column was passed, but it
   * wasn't an integer
   */
  def textToColumn( text: String ): Option[ Int ] = {
    if ( text == Spreadsheet.ANY_COLUMN ) None
    else Some( textToInteger( text ) )
  }

  /**
   * Given the text of a boolean, it will return the
   * corresponding boolean.  Note that the test is case
   * insensitive.
   * @param text The textual type
   * @return the boolean
   * @throws ClassParseException If the text wasn't either
   *         "true" or "false
   */
  def textToBoolean( text: String ): Boolean = 
    text.toLowerCase match {
      case "true" => true
      case "false" => false
      case _ =>
	throw new ClassParseException( "Cannot convert \"" +
				       text +
				       "\" to a boolean value." )
    }
  
  /**
   * Given the textual type of a parameter, it will return
   * a ParamType that correlates to it.  Note that the test is case
   * insensitive.
   * @param text The textual type
   * @return The ParamType
   * @throws ClassParseException If the text didn't match
   *         any known parameter
   */
  def textToParamType( text: String ): ParamType =
    stringToParam.getOrElse( text, 
			     throw new ClassParseException( "Unknown param " +
							    "type: \"" + 
							    text  + "\"" ) )
  /**
   * Given the textual type of a parameter, it will return a param type
   * that correlates to it.  The type must be that of an instance
   * @param text The textual type
   * @return The corresponding type
   * @throws ClassParseException If the text didn't match a known parameter,
   * or if the parameter it matched was not that of an instance type
   */
  def textToInstanceType( text: String ) = {
    val retval = textToParamType( text )
    if ( !isInstanceType( retval ) ) {
      throw new ClassParseException( "Non-instance type parameter: " + text )
    } else retval
  }

  /**
   * Given a bunch of pre-classes, it will return a map where the key
   * is the pre-class and the value is the position in the original
   * sequence.
   * @param classes The pre-classes
   * @return A mapping of the pre-class to the pre-classes position
   */
  def classMap( classes: Seq[ PreClass ] ) = {
    val asArray = classes.toArray
    Map() ++ 0.until( classes.length ).map( id =>
      Pair( classes( id ), id ) )
  }

  /**
   * Given a bunch of pre-classes, it will return a dependency graph
   * representing the pre-classes.
   * @param classes The pre-classes
   * @return A dependency graph for the classes
   * @throws ClassParseException If a given pre class wasn't parsed in already
   */
  def dependencyGraph( classes: Set[ PreClass ] ) = {
    val newClasses = classes.filter( !_.parsedIn ).toSeq
    val id = classMap( newClasses )
    val retval = new DependencyGraph[ PreClass, Object ]( newClasses.length )

    // adds the given class to the graph
    def addClass( theClass: PreClass ) {
      val thisID = id( theClass )
      retval.setNodeData( thisID,
			  new DependencyNode( theClass ) )
      theClass.dependenciesAsPreClasses.foreach( ( current: PreClass ) => {
	if ( !current.parsedIn ) {
	  retval.addEdge( id( current ), thisID ) 
	} 
      } )
    }

    newClasses.foreach( addClass( _ ) )
    retval
  }
  
  /**
   * Given a bunch of pre-classes, it will return them in the order in
   * which they should be parsed.  This will make sure that the classes
   * can be parsed in in a way that resolves dependencies.
   * @param classes The pre-classes
   * @return The pre-classes in the proper parsing order
   */
  def toParseOrder( classes: Set[ PreClass ] ) = 
    dependencyGraph( classes ).topologicalSort.map( _.info )

    /**
   * Like <code>registerClass</code>, but if a repeat is found, it will
   * throw a ClassAlreadyRegistered exception.
   * @param theClass The class to register
   * @throws ClassAlreadyRegisteredException If the class was already registered
   * @throws ClassParseException If a factory type isn't recognized
   */
  def registerClassNoRepeat( theClass: InstanceFactory[ _ ] ) {
    assertClassNotRegistered( theClass )
    registerClass( theClass )
  }

  /**
   * Registers the given class.  If the name is a repeat, then it
   * will be overridden
   * @return true if it was a repeat, else false
   * @throws ClassParseException If a factory type isn't recognized
   */
  def registerClass( theClass: InstanceFactory[ _ ] ): Boolean = {
    val retval = classRegistered( theClass )
    try {
      FactoryManager.registerFactory( theClass )
    } catch {
      case UnknownFactoryTypeException( message ) =>
	throw new ClassParseException( message )
    }
    retval
  }

  /**
   * Asserts that the given class hasn't already been registered
   * @param theClass the class to check
   * @throws ClassAlreadyRegisteredException If the class was already registered
   */
  def assertClassNotRegistered( theClass: InstanceFactory[ _ ] ) {
    if ( classRegistered( theClass ) ) {
      throw new ClassAlreadyRegisteredException( "Class with name \"" +
						 theClass.name + 
						 "\" of type " +
						 ParamType.toString( theClass.instanceType ) + 
						 " is already registered." )
    }
  }

  /**
   * Determines if the given class is already registered
   * @param theClass the class to check
   * @return true if it is already registered, else false
   * @throws ClassParseException If a factory type isn't recognized
   */
  def classRegistered( theClass: InstanceFactory[ _ ] ) =
    theClass match {
      case matcher: MatcherFactory => 
	MatcherFactoryManager.isRegistered( matcher.name )
      case replacer: ReplacerFactory =>
	ReplacerFactoryManager.isRegistered( replacer.name )
      case _ =>
	throw new ClassParseException( "Unknown factory type: " +
				       ParamType.toString( theClass.instanceType ) )
    }
}

import sentinel.model.ParamType._

/**
 * Holds constants relevant to PreClass
 * @author Kyle Dewey
 */
object PreClassConstants {
  val OVERWRITE_DEFAULT = true
}

/**
 * Holds PreClass objects as singletons.
 * Note that the names and types of pre classes are used to differentiate
 * them, not the parser or dependencies.
 * @author Kyle Dewey
 */
object PreClass {
  private var _registered: Map[ Pair[ String, ParamType ], PreClass ] = Map()

  /**
   * Gets the registered pre classes.
   * @return The registered pre classes
   */
  def registered() =
    Set() ++ _registered.values
  
  /**
   * Given a pair holding the name and type of a pre class, this will
   * return whether or not a pre class has already been made with this
   * information
   * @param pair The name, type pair
   * @return The pre class registered with this pair, or None if no such
   * preclass exists.
   */
  def apply( pair: Pair[ String, ParamType ] ): Option[ PreClass ] =
    _registered.get( pair )

  /**
   * Given a name and a type, returns a PreClass if this given name
   * and type is already registered.  If not, this returns None.
   * @param name The name of the pre class.
   * @param theType The type of the pre class.
   * @return A pre class with the given name and type, or None if it doesn't
   * exist.
   */
  def apply( name: String, theType: ParamType ): Option[ PreClass ] =
    apply( Pair( name, theType ) )

  /**
   * Creates and registers a pre class with the given name, type, and parser.
   * Note that this will overwrite any previously written pre class objects.
   * @param pair The name, type pair describing the pre class
   * @param parser The parser that parsed in the pre class
   * @param dependencies The dependencies for this pre class
   * @return The created PreClass
   */
  def apply( pair: Pair[ String, ParamType ], 
	     parser: ClassParser,
	     dependencies: Seq[ Pair[ String, ParamType ] ] ): PreClass = {
    val retval = new PreClass( pair._1, 
			       pair._2, 
			       parser, 
			       dependencies )
    _registered += pair -> retval
    retval
  }

  /**
   * Like <code>apply</code>, only the pair is split between two parameters.
   * @param name The name of the pre class
   * @param theType the type of the pre class
   * @param parser The parser that parsed in the pre class
   * @param dependencies The dependencies for this pre class
   * @return The created PreClass
   */
  def apply( name: String, 
	     theType: ParamType, 
	     parser: ClassParser,
	     dependencies: Seq[ Pair[ String, ParamType ] ] ): PreClass =
    apply( Pair( name, 
		 theType ), 
	   parser, 
	   dependencies )
}

/**
 * Container for a class that has yet to be parsed in.
 * Note that it is intended that dependencies be filled in
 * before forming the dependency graph.  If they are not,
 * the resulting graph is worthless.
 * @param name The name of the class
 * @param theType The type of the class
 * @param parser The parser that parsed in the class
 * @param dependencies The dependencies of this pre class, as pairs
 * @author Kyle Dewey
 */
class PreClass private ( val name: String,
			 val theType: ParamType,
			 val parser: ClassParser,
			 val dependencies: Seq[ Pair[ String, ParamType ] ] ) {
  lazy val dependenciesAsPreClasses =
    getDependenciesAsPreClasses

  /**
   * Determines if the class corresponding to this pre class has
   * been parsed in.
   * @return true if it has been parsed in, else false
   * @throws ClassParseException If the given type isn't recognized
   * as an instance type
   */
  def parsedIn() =
    theType match {
      case MatcherType => 
	MatcherFactoryManager.isRegistered( name )
      case ReplacerType => 
	ReplacerFactoryManager.isRegistered( name )
      case _ => throw new ClassParseException( "Unknown instance type: " +
					       ParamType.toString( theType ) )
    }
  
  /**
   * Determines if this pre class is equal to another.
   * This is based upon the name and type.
   * @param other The other object to compare to
   * @return true if they are equal, else false
   */
  override def equals( other: Any ) = 
    other.isInstanceOf[ PreClass ] && 
    toPair == other.asInstanceOf[ PreClass ].toPair

  /**
   * Gets the hash code of this pre class.
   * The code is based on the name and the type
   * @return a hash code based on the name and the type
   */
  override def hashCode() =
    toPair.hashCode

  /**
   * Gets this pre class as a name, type pair
   * @return This pre class as a name, type pair
   */
  def toPair() =
    Pair( name, theType )

  /**
   * Gets the dependencies for this pre class, as pre class objects.
   * Note that this is intended to be called only after all pre class
   * objects have been generated.
   * @return The dependencies as pre class objects
   * @throws ClassParseException If one of the given pairs could
   * not be mapped to a pre class object.
   */
  protected def getDependenciesAsPreClasses() = 
    dependencies.map( pair => {
      val retval = PreClass( pair )
      if ( retval.isEmpty ) {
	throw new ClassParseException( "Unknown pre class: " + pair )
      }
      retval.get
    } )
  
  /**
   * Parses in the class corresponding to this pre class
   * @return The parsed in class
   * @throws ClassParseException If there was a format-level error
   * in the class
   * @throws IOException If an error occurred on reading
   */
  def parseClass() =
    parser.parseClass( this )

  /**
   * Parses in and registers the class corresponding
   * to this pre class.
   * @param overwrite If we should overwrite any previous definitions
   * of this class
   * @return true if a previous definition was overwritten, else false
   * @throws ClassParseException if there was a format-level in a class, or
   * if the given pre class doesn't correspond to this parser
   * @throws ClassAlreadyRegisteredException If overwrite is false and
   * the class was already registered
   * @throws IOException If an error occurred on reading
   */
  def parseAndRegisterClass( overwrite: Boolean ): Boolean =
    parser.parseAndRegisterClass( this, overwrite )

  /**
   * Like <code>parseAndRegiserClass</code>, but overwrite is
   * set to <code>OVERWRITE_DEFAULT</code>.
   * @return true if a previous definition was overwritten, else false
   * @throws ClassParseException if there was a format-level in a class, or
   * if the given pre class doesn't correspond to this parser
   * @throws ClassAlreadyRegisteredException If overwrite is false and
   * the class was already registered
   * @throws IOException If an error occurred on reading
   */
  def parseAndRegisterClass(): Boolean =
    parseAndRegisterClass( PreClassConstants.OVERWRITE_DEFAULT )
}

/**
 * Parses in classes.  Note that this also handles dependencies between
 * classes, etc.
 * @param fileName The name of the file to read in
 * @throws FileNotFoundException If the file could not be opened
 * @throws IOException If an error occurred on read
 * @author Kyle Dewey
 */
abstract class ClassParser( fileName: String ) {
  /**
   * Gets information about all classes within.
   * @return PreClass information about all classes within
   * @throws ClassParseException If there is a format-level error
   * @throws IOException If an error occurred on read
   */
  def classesInformation(): Seq[ PreClass ]

  /**
   * Parses in the given class.
   * It is guarenteed that <code>theClass</code> came from this parser
   * @param theClass The class to parse in
   * @return The parsed in class
   * @throws ClassParseException If there was a format-level error in the class
   * @throws IOException If an error occurred on reading
   */
  protected def internalParseClass( theClass: PreClass ): InstanceFactory[ _ ]

  /**
   * Parses in the given class.
   * @param theClass The class to parse in
   * @return The parsed in class
   * @throws ClassParseException If there was a format-level error in the class,
   * or if this class didn't come from this parser
   * @throws IOException If an error occurred on reading
   */
  def parseClass( theClass: PreClass ) = {
    assertParsedIn( theClass )
    internalParseClass( theClass )
  }

  /**
   * Asserts that the given pre class belongs to this parser
   * @param preClass The pre class to test
   * @throws ClassParseException If the pre class wasn't parsed
   * in by this parser
   */
  def assertParsedIn( preClass: PreClass ) {
    if ( preClass.parser ne this ) {
      throw new ClassParseException( "Attempt to parse in pre class that " +
				     "wasn't parsed in by the given parser. " +
				     "Pre class: " + preClass )
    }
  }

  /**
   * Parses in the given class and registers it
   * @param preClass The pre class corresponding to the given class. 
   * @param overwrite If true, then overwrite when a class that
   * has been encountered is found.  Otherwise throw an exception
   * @return true if a class was overwritten, else false
   * @throws ClassParseException if there was a format-level in a class, or
   * if the given pre class doesn't correspond to this parser
   * @throws ClassAlreadyRegisteredException If overwrite is false and
   * the class was already registered
   * @throws IOException If an error occurred on reading
   */
  def parseAndRegisterClass( preClass: PreClass, overwrite: Boolean ) = {
    val parsed = parseClass( preClass )
    if ( overwrite ) {
      ClassParser.registerClass( parsed )
    } else {
      ClassParser.registerClassNoRepeat( parsed )
      false 
    }
  }

  /**
   * Parses in and registers all classes.
   * Note that this should be called with the utmost discretion!
   * If there are dependencies that are in other files that are queued
   * to be parsed in, this will fail.
   * @param overwrite If true, then overwrite when a class that
   * has been encountered is found.  Otherwise throw an exception
   * @throws ClassParseException if there was a format-level in a class, or
   * if the given pre class doesn't correspond to this parser
   * @throws ClassAlreadyRegisteredException If overwrite is false and
   * the class was already registered
   * @throws IOException If an error occurred on reading
   */
  def parseAndRegisterClasses( overwrite: Boolean ) {
    classesInformation.foreach( _.parseAndRegisterClass( overwrite ) )
  }

  /**
   * Like <code>parseAndRegisterClasses</code>, but it uses
   * <code>PreClassConstants.OVERWRITE_DEFAULT</code> for
   * <code>overwrite</code>
   * @throws ClassParseException if there was a format-level in a class, or
   * if the given pre class doesn't correspond to this parser
   * @throws ClassAlreadyRegisteredException If overwrite is false and
   * the class was already registered
   * @throws IOException If an error occurred on reading
   */
  def parseAndRegisterClasses() {
    parseAndRegisterClasses( PreClassConstants.OVERWRITE_DEFAULT )
  }
}
