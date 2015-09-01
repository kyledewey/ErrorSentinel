/*
 * BuiltIns.scala
 * 
 * Version:
 *     $Id: BuiltIns.scala,v 1.8 2011/06/01 04:04:01 kyledewey Exp $
 *
 * Revisions:
 *      $Log: BuiltIns.scala,v $
 *      Revision 1.8  2011/06/01 04:04:01  kyledewey
 *      Now parses in the base language.
 *
 *      Revision 1.7  2011/02/27 05:20:04  kyledewey
 *      Refactored to include parameter order.
 *
 *      Revision 1.6  2010/07/11 05:56:36  kyledewey
 *      Modified to be compliant with the new interface
 *      specified by XMLParser; minor code cleanup.
 *
 *      Revision 1.5  2010/06/28 18:46:23  kyledewey
 *      The classes shown by displayClasses are now in abc order.
 *
 *      Revision 1.4  2010/06/26 17:57:40  kyledewey
 *      In addParam, exception strings are now displayed instead of
 *      silent failure.
 *
 *      Revision 1.3  2010/06/26 16:52:50  kyledewey
 *      Upon saving, it sets that there are no changes that need to be saved.
 *
 *      Revision 1.2  2010/06/26 04:08:14  kyledewey
 *      Added a bit more description to some of the commands.
 *
 *      Revision 1.1  2010/06/23 03:09:25  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.builtins

import sentinel.model._
import sentinel.model.ParamType._
import sentinel.model.parser.xml._
import sentinel.model.writer.xml._
import sentinel.view._

/**
 * Exception thrown when a class name conflicts with another.
 * @param message The message to show
 * @author Kyle Dewey
 */
case class ClassNameException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown when a class type is invalid.
 * @param message The message to show
 * @author Kyle Dewey
 */
case class ClassTypeException( message: String ) extends Exception( message ) {}

/**
 * Exception thrown if a class is not a built in
 * @param message A message to show
 * @author Kyle Dewey
 */
case class NonBuiltInException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when a conversion failed.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class ConversionException( message: String )
     extends Exception( message )

/**
 * Trait for a view that is to be manipulated by a built in model.
 * @author Kyle Dewey
 */
trait BuiltInView {
  /**
   * Tells the view that a given class has been added.
   * @param className The name of the class that has been added
   */
  def classAdded( className: String ): Unit

  /**
   * Tells the view that a given class has been removed.
   * @param className The name of the class that has been removed
   */
  def classRemoved( className: String ): Unit
}

/**
 * Interface that defines how a controller interacts with the
 * built in model.
 * @author Kyle Dewey
 */
trait BuiltInModel {
  /**
   * Adds a class to the model.
   * @param name The name of the class
   * @param desc A description of the class
   * @param theType The type of the class
   * @param className The name of the JVM class that backs the class
   * @param params Formal parameters for the class
   * @throws ClassNameException If a class with this name already exists
   * @throws ClassTypeException If the given parameter type is not that of
   * an instance type
   */
  def addClass( name: String,
	        desc: String,
	        theType: ParamType,
	        className: String,
	        params: Seq[ ParamInfo ] ): Unit

  /**
   * Gets the name of all classes in the model.
   * @return The name of all classes in the model
   */
  def getClasses(): Seq[ String ]

  /**
   * Gets information about the class with the given name.
   * @param name The name of the class to get information about
   * @return The class description, the class type, the JVM class name,
   * and the parameters the class takes, or none if the given
   * class doesn't exist
   */
  def getClassInfo( name: String ): Option[ Tuple4[ String, 
						    ParamType,
						    String,
						    Map[ String, ParamInfo ] ] ]

  /**
   * Removes a class from the model.
   * @param name The name of the class to remove
   * @return true if the class was removed, else false
   */
  def removeClass( name: String ): Boolean

  /**
   * Saves the classes to some permanent medium.
   * @throws IOException If an error occurred on write
   */
  def save(): Unit
}

/**
 * Contains helper routines for DefaultModel
 * @author Kyle Dewey
 */
object DefaultModel {
  /**
   * Given a factory manager, determines if all registered classes are
   * builtins.
   * @param manager The factory manager
   * @return true if all registered factories are builtins, else false
   */
  def allBuiltIns( manager: FactoryManager[ _, _ ] ) =
    manager.getFactories.forall( _.isInstanceOf[ ReflectionFactory[ _ ] ] )

  /**
   * Determines if all registered classes are builtins.
   * @return true if all registered classes a builtins, else false
   */
  def allBuiltIns(): Boolean = {
    allBuiltIns( MatcherFactoryManager ) && 
    allBuiltIns( ReplacerFactoryManager ) 
  }

  /**
   * Gets all the classes as ReflectionFactories from the given factory
   * @param manager The manager to get factories from
   * @return All the factories are reflection factories
   * @pre <code>allBuiltIns( manager ) == true</code>
   */
  def getClasses[ T <: Instance ]( manager: FactoryManager[ _, _ ] ) =
    manager.getFactories.map( _.asInstanceOf[ ReflectionFactory[ T ] ] )
  
  /**
   * Gets all classes which are registered
   * @return All the registered classes as reflection factories
   * @pre <code>allBuiltIns() == true</code>
   */
  def getClasses[ T <: Instance ](): Seq[ ReflectionFactory[ T ] ] = {
    getClasses[ T ]( MatcherFactoryManager ) ++ 
      getClasses[ T ]( ReplacerFactoryManager )
  }
}

/**
 * A model for an XML file filled with builtins.
 * Handles IO, etc.
 * @param fileName The name of the xml file
 * @param views The views to attach to this model
 * @throws NonBuiltInException If any of the given classes in the given
 * file name were not built ins
 * @author Kyle Dewey
 */
class DefaultModel( fileName: String, 
		    private var views: Seq[ BuiltInView ] ) 
extends BuiltInModel {
  import sentinel.project._
  import java.io._
  // begin instance variables
  private val writer = new XMLWriter( fileName )

  // the name is the key, and the value is the class itself
  private var classes: Map[ String, ReflectionFactory[ _ <: Instance ] ] = Map()
  // end instance variables

  // begin constructor
  LanguageReader.readBaseLanguage()

  // read in the existing classes if the file already exists
  if ( new File( fileName ).exists ) {
    new XMLParser( fileName ).parseAndRegisterClasses()
    if ( !DefaultModel.allBuiltIns ) {
      throw new NonBuiltInException( "Not all classes in file: " +
				     fileName + " are builtins." )
    }
    classes ++= DefaultModel.getClasses.map( theClass =>
      theClass.name -> theClass )
  }
  // end constructor

  /**
   * Creates a new model with no views attached to it.
   * @param fileName The name of the XML file to read from / write to
   */
  def this( fileName: String ) =
    this( fileName, Seq() )

  /**
   * Adds a class to the model.
   * @param name The name of the class
   * @param desc A description of the class
   * @param theType The type of instances the class makes
   * @param className The name of the JVM class that backs the class
   * @param params Formal parameters for the class
   * @throws ClassNameException If a class with this name already exists
   * @throws ClassTypeException If the given parameter type is not that of
   * @throws ClassNotFoundException If the given class name doesn't exist
   * @throws NoSuchMethodException If there was no constructor that takes
   *         Seq[ NamedParam ]   * an instance type
   */
  override def addClass( name: String,
			 desc: String,
			 theType: ParamType,
			 className: String,
			 params: Seq[ ParamInfo ] ) {
    val paramsAsSeq =
      params.map( _.name ).toArray
    val paramsAsMap = 
      Map() ++ params.map( param => (param.name, param) )
    if ( classes.contains( name ) ) {
      throw new ClassNameException( "Class with the name \"" + name +
				    "\" already exists" )
    }

    var theClass: ReflectionFactory[ _ <: Instance ] = null
    if ( theType == MatcherType ) {
      theClass = MatcherFactory( name,
				 desc,
				 paramsAsMap,
				 paramsAsSeq,
				 className )
    } else if ( theType == ReplacerType ) {
      theClass = ReplacerFactory( name,
				  desc,
				  paramsAsMap,
				  paramsAsSeq,
				  className )
    } else {
      throw new ClassTypeException( "Unknown class type: " +
				    ParamType.toString( theType ) )
    }
    classes += name -> theClass
    viewsClassAdded( name )
  }

  /**
   * Gets information about the given class, or None if the class
   * doesn't exist.
   * @param name The name of the class
   * @return The class description, class type, the JVM class name,
   * and the parameters the class takes, or None if the given class
   * doesn't exist
   */
  override def getClassInfo( name: String ) = {
    var retval: Option[ Tuple4[ String,
			        ParamType,
			        String,
			        Map[ String, ParamInfo ] ] ] = None
    if ( classes.contains( name ) ) {
      val theClass = classes( name )
      retval = Some( ( theClass.desc,
		       theClass.instanceType,
		       theClass.JVMClassName,
		       theClass.validParams ) )
    }
    retval
  }

  /**
   * Gets the name of all classes in the model.
   * @return The name of all classes in the model
   */
  override def getClasses() =
    classes.map( _._1 ).toSeq

  /**
   * Removes a class from the model.
   * @param name The name of the class to remove
   * @return true if the class was removed, else false, as in a class
   * with the given name didn't exist
   */
  override def removeClass( name: String ) = 
    if ( classes.contains( name ) ) {
      classes -= name
      viewsClassRemoved( name )
      true
    } else false
  
  /**
   * Adds a view to the model.
   * @param view The view to add
   */
  def addView( view: BuiltInView ) =
    views = views ++ Seq( view )
  
  /**
   * Removes a view from the model.  If no such view
   * exists, this is a no-op.
   * @param view The view to remove
   */
  def removeView( view: BuiltInView ) =
    views = views.filter( _ ne view )

  /**
   * Informs all views that a class with the given name has
   * been added.
   * @param className The name of the class that has been added
   */
  def viewsClassAdded( className: String ) =
    views.foreach( _.classAdded( className ) )

  /**
   * Informs all views that a class with the given name
   * has been removed
   * @param className The name of the class that has been removed
   */
  def viewsClassRemoved( className: String ) =
    views.foreach( _.classRemoved( className ) )

  
  /**
   * Writes the file out.
   * @throws IOException If an error occurred on write
   */
  def save() {
    writer.writeClasses( classes.values.toList )
  }
}
  
/**
 * Contains constant values for TextBuiltInView
 * @author Kyle Dewey
 */
object TextBuiltInView {
  // begin constants
  val PROMPT = "builtin> "
  val NO_SUCH_CLASS = "No class with the given name exists"
  val EMPTY = "EMPTY"
  // end constants

  /**
   * Converts a string to a boolean.
   * @param string The string to convert
   * @return The string as a boolean
   * @throws ConversionException If the conversion wasn't possible
   */
  def string2Boolean( string: String ): Boolean =
    string.toLowerCase match {
      case "true" => true 
      case "false" => false 
      case "yes" => true 
      case "no" => false 
      case _ => 
	throw new ConversionException( "Could not convert \"" + string +
				       "\" to a boolean value" )
    }

  /**
   * Converts a string to a param type
   * @param string The string to convert
   * @return The string as a param type
   * @throws ConversionException If the conversion wasn't possible
   */
  def string2ParamType( string: String ): ParamType = {
    if ( ParamType.stringToParam.contains( string ) ) {
      ParamType.stringToParam( string )
    } else {
      throw new ConversionException( "Could not convert \"" + string +
				     "\" to a parameter type" )
    }
  }
}

/**
 * Manipulates the built in model using a text interface.
 * @param model The model that is associated with this view.
 * @author Kyle Dewey
 */
class TextBuiltInView( val model: BuiltInModel ) 
extends InteractiveTextInterpreter( TextBuiltInView.PROMPT ) with BuiltInView {
  import sentinel.project._
  import TextBuiltInView._
  // begin instance variables
  private var changesMade = false // if we need to save

  // variables for the current class
  private var name: Option[ String ] = None
  private var desc: Option[ String ] = None
  private var theType: Option[ ParamType ] = None
  private var className: Option[ String ] = None
  private var params: Seq[ ParamInfo ] = Seq()
  // end instance variables

  // begin constructor
  // register all of our commands
  registerCommand( "show_classes",
		   Some( "Gets the names of all classes loaded" ),
		   this,
		   this.getClass.getMethod( "displayClasses", null ) )
  registerCommand( "delete_class",
		   Some( "Deletes a class with the given name" ),
		   this,
		   this.getClass.getMethod( "deleteClass", classOf[ String ] ) )
  registerCommand( "add_class",
		   Some( "Adds the current class to the list of classes" ),
		   this,
		   this.getClass.getMethod( "addClass", null ) )
  registerCommand( "clear",
		   Some( "Clears the current class" ),
		   this,
		   this.getClass.getMethod( "resetCurrentClass", null ) )
  registerCommand( "show_class",
		   Some( "Displays a class with the given name" ),
		   this,
		   this.getClass.getMethod( "displayClass", classOf[ String ] ) )
  registerCommand( "show_class",
		   Some( "Displays the current class" ),
		   this,
		   this.getClass.getMethod( "displayClass", null ) )
  registerCommand( "name",
		   Some( "Gives a new name to the current class" ),
		   ( newName: String ) => { name = Some( newName ) ; "" } )
  registerCommand( "desc",
		   Some( "Gives a new description to the current class" ),
		   ( newDesc: String ) => { desc = Some( newDesc ) ; "" } )
  registerCommand( "type",
		   Some( "Gives a new type to the current class" ),
		   this,
		   this.getClass.getMethod( "changeType", classOf[ String ] ) )
  registerCommand( "className",
		   Some( "Gives a new JVM class name to the current class" ),
		   ( newClassName: String ) => { className = Some( newClassName ) ; "" } )
  registerCommand( "add_param",
		   Some( "Adds a new parameter to the current class\n" +
		         "Needs name, description, type, array?, required?" ),
		   this,
		   this.getClass.getMethod( "addParam", 
					    classOf[ String ],
					    classOf[ String ],
					    classOf[ String ],
					    classOf[ String ],
					    classOf[ String ] ) )
  registerCommand( "delete_param",
		   Some( "Removes a parameter from the current class" ),
		   this,
		   this.getClass.getMethod( "removeParam", classOf[ String ] ) )
  registerCommand( "save",
		   Some( "Saves all changes to the output file" ),
		   () => { model.save() ; changesMade = false ; "" } )
  // end constructor

  /**
   * Removes a parameter from our parameters.
   * @param name The name of the parameter to remove
   * @return A string describing how it went
   */
  def removeParam( name: String ): String = {
    if ( params.contains( name ) ) {
      params = params.filter( _.name != name )
      ""
    } else {
      "Unknown parameter: \"" + name + "\""
    }
  }

  /**
   * Adds a parameter on to our parameters.
   * @param name The name of the parameter
   * @param desc A description of the parameter
   * @param theType The type of the parameter
   * @param isArray Whether or not the parameter is an element of an array
   * @param isRequired Whether or not the the parameter is required
   * @return A string describing how it went
   */
  def addParam( name: String, 
	        desc: String,
	        theType: String,
	        isArray: String,
	        isRequired: String ): String = {
    if ( params.contains( name ) ) {
      "\"" + name + "\" is already in the parameter list; remove first"
    } else {
      try {
	params ++=  Seq( new ParamInfo( name,
				        desc,
				        string2ParamType( theType ),
				        string2Boolean( isArray ),
				        string2Boolean( isRequired ) ) )
	""
      } catch {
	case e: ConversionException => e.getMessage
      }
    }
  }

  /**
   * Modifies the current type.
   * @param newType The type to change it to
   * @return A string giving some information to the user
   */
  def changeType( newType: String ): String = {
    if ( ParamType.isInstanceType( newType ) ) {
      theType = Some( ParamType.stringToParam( newType ) )
      ""
    } else {
      "Unknown type: \"" + newType + "\""
    }
  }

  /**
   * Merely records that a change was made.
   * @param className The name of the class that was added
   */
  def classAdded( className: String ) {
    changesMade = true
  }

  /**
   * Merely records that a change was made
   * @param className The name of the class that was removed
   */
  def classRemoved( className: String ) {
    changesMade = true
  }

  /**
   * Displays information about the given parameter.
   * @param param The parameter to display information about
   * @return A string describing this parameter
   */
  def displayParameter( param: ParamInfo ): String = {
    "\tParameter:\n" +
    "\t\tName: " + param.name + "\n" +
    "\t\tDescription: " + param.desc + "\n" +
    "\t\tType: " + ParamType.toString( param.paramType ) + "\n" +
    "\t\tArray: " + param.isArray.toString + "\n" +
    "\t\tRequired: " + param.isRequired.toString + "\n"
  }

  /**
   * Displays information about the current class.
   * @return A string describing the current class
   */
  def displayClass(): String = {
    displayClass( name,
		  desc,
		  theType,
		  className,
		  params )
  }

  /**
   * Displays information about the given class
   * @param name The name of the class
   * @return String information about the class
   */
  def displayClass( name: String ): String = {
    val info = model.getClassInfo( name )
    if ( info.isDefined ) {
      displayClass( Some( name ),
		    Some( info.get._1 ),
		    Some( info.get._2 ),
		    Some( info.get._3 ),
		    info.get._4.values.toList )
    } else NO_SUCH_CLASS
  }

  /**
   * Displays information about a class
   * @param name The name of the class
   * @param desc A description of the class
   * @param theType The type of the class
   * @param className The JVM class name of the class
   * @param params The parameters to the class
   * @return A string describing the class
   */
  def displayClass( name: Option[ String ],
		    desc: Option[ String ],
		    theType: Option[ ParamType ],
		    className: Option[ String ],
		    params: Seq[ ParamInfo ] ) = {
    val typeString = 
      if ( theType.isDefined ) {
	ParamType.toString( theType.get )
      } else EMPTY 
    val paramsString = 
      if ( !params.isEmpty ) {
	"\n" +
	params.map( displayParameter( _ ) )
              .mkString( "", "\n", "" )
      } else EMPTY 
    
    "Name: " + name.getOrElse( EMPTY ) + "\n" +
    "Description: " + desc.getOrElse( EMPTY ) + "\n" +
    "Type: " + typeString + "\n" +
    "JVMClassName: " + className.getOrElse( EMPTY ) + "\n" +
    "Parameters: " + paramsString
  }

  /**
   * Resets the current class parameters.
   */
  def resetCurrentClass() {
    name = None
    desc = None
    theType = None
    className = None
    params = Seq()
  }

  /**
   * Adds in the current class.
   * @return A string giving the user back information
   */
  def addClass(): String = {
    if ( name.isDefined &&
	 desc.isDefined &&
	 theType.isDefined &&
	 className.isDefined ) {
      try {
	model.addClass( name.get,
		        desc.get,
		        theType.get,
		        className.get,
		        params )
	val retval = "Added class \"" + name.get + "\""
	resetCurrentClass()
	retval
      } catch {
	case e: ClassNameException => e.getMessage
	case e: ClassTypeException => e.getMessage
	case e: ClassNotFoundException =>
	  "Unknown JVM class with full name: " + className.get
	case e: NoSuchMethodException =>
	  "Class didn't have compatible constructor ( Seq[ NamedParam ] )"
      }
    } else {
      "One or more parameters has no value"
    }
  }

  /**
   * Deletes a class with the given name.
   * @param name The name of the class to remove
   * @return A string describing the removal
   */
  def deleteClass( name: String ) = {
    if ( !model.removeClass( name ) ) NO_SUCH_CLASS
    else ""
  }

  /**
   * Gets all classes in the model.
   * @return A string giving all the class names in the model
   */
  def displayClasses() = 
    model.getClasses.toList.sortWith( _ < _ ).mkString( "\n" )

  /**
   * A custom exit command that will ask the user if he
   * wants to save in case there were changes.
   * @return A string to display
   */
  override def exit() = {
    if ( changesMade ) {
      prompt = "Do you want to save? (yes/no/cancel) "
      registerCommand( "yes",
		       () => { model.save()
			       super.exit() } )
      registerCommand( "no",
		       () => super.exit() )
      registerCommand( "cancel",
		       () => { unregisterCommands( "yes", 0 )
			       unregisterCommands( "no", 0 )
			       unregisterCommands( "cancel", 0 ) 
			       prompt = PROMPT ; "" } )
      ""
    } else {
      super.exit()
    }
  }
} // TextBuiltInView

/**
 * The actual program that can make XML descriptions of builtins.
 * @author Kyle Dewey
 */
object BuiltIns {
  /**
   * Prints usage information
   */
  def usage() {
    println( "Takes a filename to read from and write to as a param" )
  }

  /**
   * Does all the work.
   * @param args Command line arguments - The file to work with
   */
  def main( args: Array[ String ] ) {
    if ( args.length != 1 ) {
      usage()
      System.exit( 1 )
    }
    val model = new DefaultModel( args( 0 ) )
    val view = new TextBuiltInView( model )
    model.addView( view )
    view.mainLoop()
  }
}
