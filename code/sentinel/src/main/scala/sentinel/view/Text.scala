/*
 * Text.scala
 *
 * Version:
 *     $Id: Text.scala,v 1.4 2010/06/28 18:45:19 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Text.scala,v $
 *      Revision 1.4  2010/06/28 18:45:19  kyledewey
 *      Changed specialCharacter to return a Char;
 *      parseLine can now have a backslashed character within
 *      a quoted string.
 *
 *      Revision 1.3  2010/06/26 04:08:36  kyledewey
 *      Added routines so that the output doesn't contain
 *      excessive newlines in case of null strings.
 *
 *      Revision 1.2  2010/06/23 03:08:14  kyledewey
 *      Added the ability to unregister commands;
 *      Made it so the processing loop can handle other
 *      IO streams that those in System.
 *
 *      Revision 1.1  2010/06/22 22:50:50  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.view

import java.lang.reflect._

/**
 * Exception thrown when we failed to parse a string.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class StringParseException( message: String ) 
     extends Exception( message ) {}

/**
 * Exception thrown when an unknown command is attempted to
 * be executed.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class UnknownCommandException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when we couldn't determine the appropriate method
 * to use.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class MethodDeterminationException( message: String )
     extends Exception( message ) {}

/**
 * Exception thrown when a given method is not acceptable for use
 * by the interpreter.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class InvalidMethodException( message: String )
     extends Exception( message ) {}

/**
 * Contains helper methods for TextCommandProcessor.
 * @author Kyle Dewey
 */
object TextCommandProcessor {
  // begin constants
  // contains the acceptable parameters for a method which we take
  val understoodParams: Set[ Class[ _ ] ] = Set( classOf[ Int ],
						 classOf[ Char ],
						 classOf[ Double ],
						 classOf[ java.lang.String ] )
  val AUTOMAGIC_METHOD_NAME = "apply"
  val SHOW_COMMANDS_NAME = "show_commands"
  val SHOW_COMMANDS_HELP = "Shows all commands the interpreter understands"
  // end constants

  /**
   * Attempts to get a string representing a special character.
   * For instance, if given 'n', it will return "\n", and if
   * given '\\', it will return "\\".
   * However, if given anything else, it is considered an error.
   * @param theChar The special character
   * @return The string that represents the special character
   * @throws StringParseException If the special character is not
   *         recognized
   */
  def specialCharacter( theChar: Char ): Char = 
    theChar match {
      case '\\' => '\\'
      case '"' => '"'
      case 'n' => '\n'
      case ' ' => ' '
      case _ => 
	throw new StringParseException( "Unknown control character: " +
				        theChar )
    }

  /**
   * Parses the given line.
   * @param line The line to parse
   * @return The line as a bunch of literals
   * @throws StringParseException If we failed to parse the line properly
   */
  def parseLine( line: String ): Seq[ String ] = {
    var inQuote = false // true if we are in a quoted string
    var backslash = false // true if the previous char was a backslash
    var retval: Seq[ String ] = Seq()
    var current = "" // the current item we are working on
    val array = line.toArray
    
    def push() {
      retval = retval ++ Seq( current )
      current = ""
    }

    def pushIfSomething() {
      if ( current != "" ) push()
    }

    def addCurrent( toAdd: Char ) =
      current += "" + toAdd

    for{ x <- ( 0 to array.length - 1 ) } {
      if ( backslash ) {
	addCurrent( specialCharacter( array( x ) ) )
	backslash = false
      } else if ( array( x ) == '\\' ) {
	backslash = true 
      } else if ( inQuote ) {
	if ( array( x ) == '"' ) {
	  inQuote = false
	  push()
	} else {
	  addCurrent( array( x ) )
	}
      } else if ( array( x ) == '"' ) {
	inQuote = true
      } else if ( array( x ) == ' ' ) {
	pushIfSomething()
      } else {
	addCurrent( array( x ) )
      }
    }

    // add the last item on
    pushIfSomething()

    if ( inQuote ) {
      throw new StringParseException( "Unterminated quoted string" )
    } else if ( backslash ) {
      throw new StringParseException( "Backslash with no following character" )
    }

    retval
  }

  /**
   * Given a string argument, it will attempt to cast it to the given
   * type.
   * @param arg The argument to convert
   * @param toClass The class of the type to convert it to
   * @return The argument as that type, or None if conversion was impossible
   */
  def convertArg( arg: String, toClass: Class[ _ ] ): Option[ _ ] = {
    try {
      // for some odd reason the parser can't figure out the
      // equivalent (cleaner) pattern matching version...weird
      if ( toClass eq classOf[ Int ] ) Some( arg.toInt )
      else if ( toClass eq classOf[ Double ] ) Some( arg.toDouble )
      else if ( ( toClass eq classOf[ Char ] ) && arg.length == 1 ) Some( arg( 0 ) )
      else if ( toClass eq classOf[ String ] ) Some( arg )
      else None
    } catch {
      case e: NumberFormatException => None
    }
  }

  /**
   * Performs conversions on the given list of arguments.
   * @param args A list of arguments to convert
   * @param toTypes A parallel list of the types to convert the args to
   * @return The converted arguments.  Note that if a given conversion
   * failed, then that conversion will be None
   */
  def convertArgs( args: Seq[ String ], 
		   toTypes: Seq[ Class[ _ ] ] ): Seq[ Option[ Object ] ] = {
    val converted = args.zip(toTypes).map(p => convertArg(p._1, p._2))
    converted.map( arg =>
      if ( arg.isDefined ) {
	Some( arg.get.asInstanceOf[ Object ] )
      } else None )
  }
    
  /**
   * Given an object, a method, and a list of arguments, it will attempt
   * to execute the method with the arguments.  It may attempt to cast
   * the arguments to different types to try to be able to execute the
   * method.
   * @param theObject The object to execute the method on
   * @param method The method to call on the object
   * @param args The arguments to the method
   * @return The text the command returned if it was able to execute
   * the command, else None
   */
  def execute( theObject: AnyRef, 
	       method: Method, 
	       args: Seq[ String ] ): Option[ String ] = {
    val converted = convertArgs( args,
				 method.getParameterTypes )

    if ( converted.filter( _.isEmpty ).isEmpty ) {
      // all conversions were successful
      // note that the : _* is to do a conversion from an array to
      // a Java vararg
      val args = converted.map( _.get ).toArray
      Some( method.invoke( theObject, args : _* ).asInstanceOf[ String ] )
    } else None
  }

  /**
   * Attempts to execute one of a given number of candidate
   * object/method pairs given a list of arguments.
   * @param candidates The candidates to try
   * @param args The arguments for each of the pairs
   * @return The text the command returned if it was able to execute
   * the command, else None
   */
  def execute( candidates: Seq[ Pair[ AnyRef, Method ] ], 
	       args: Seq[ String ] ): Option[ String ] = {
    var retval: Option[ String ] = None
    var x = 0

    // try to execute each until one works
    while( x < candidates.length &&
	   retval.isEmpty ) {
      retval = execute( candidates( x )._1,
		        candidates( x )._2,
		        args )
      x += 1
    }

    retval
  }

  /**
   * Determines if the given method contains acceptable params.
   * @param method The method to test
   * @return True if it contains acceptable params for use, else false
   */
  def paramsAcceptable( method: Method ) = 
    method.getParameterTypes.forall( understoodParams.contains( _ ) )
  
  /**
   * Determines if the return type is acceptable for the given method.
   * The only acceptable return type is that of a string, for interaction
   * with the interpreter
   * @param method the method to test
   * @return true if the return type is acceptable (String)
   */
  def returnTypeAcceptable( method: Method ) =
    method.getReturnType == classOf[ String ]
  
  /**
   * Given a class, gets the appropriate method to use for the
   * text command interpreter.  Note that it expects that
   * the method name will be AUTOMAGIC_METHOD_NAME, and it will take
   * some combination of String, int, double, and char.  In addition,
   * it will have a return type of string.
   * @param theClass The class to get the method of
   * @return The correct method to use
   * @throws MethodDeterminationException If we couldn't figure
   *         out which is the appropriate method to use
   */
  def getMethod( theClass: Class[ _ ] ): Method = {
    val candidates = theClass.getMethods
                             .filter( method => 
			       method.getName == AUTOMAGIC_METHOD_NAME &&
			       returnTypeAcceptable( method ) &&
			       paramsAcceptable( method ) )
    if ( candidates.length == 0 ) {
      throw new MethodDeterminationException( "Could not determine " +
					      "appropriate method to use" )
    } else if ( candidates.length > 1 ) {
      throw new MethodDeterminationException( "Multiple methods determined " +
					      "to be appropriate" )
    }
    candidates.head
  }
}

/**
 * Class that acts as a key for commands in the interpreter.
 * Note that keys may have multiple values associated with them, in case
 * of overloading with different types.
 * @param name The name of the command
 * @param numParams The number of parameters for the command
 * @author Kyle Dewey
 */
case class CommandKey( val name: String, val numParams: Int ) {}

/**
 * Contains helper routines and constants for HelpCommand.
 * @author Kyle Dewey
 */
object HelpCommand {
  // begin constants
  val GENERAL_HELP = "Use show_commands to see all commands.  Use " +
                     "help commandName to see help information for the " +
                     "given command.  Use help to see this message."
  val NO_HELP = "No help information associated with the command"
  val HELP_COMMAND_NAME = "help"
  val HELP0 = "Prints general help information"
  val HELP1 =  "Prints help information about commands with the given name"
  val HELP2 = "Prints help information about the command with the " +
  "given name and given number of params"
  // end constants
}

/**
 * Implements the help command, which is fairly complex and dynamic.
 * @param commands Keys for commands along with help information for them
 * @author Kyle Dewey
 */
class HelpCommand( private var commands: Map[ CommandKey, String ] ) {
  /**
   * Creates a new HelpCommand with no registered commands.
   */
  def this() =
    this( Map() )

  /**
   * Unregisters the command with the given key
   * Safe to call if the command doesn't exist
   * @param key The key
   */
  def unregisterCommand( key: CommandKey ) {
    if ( commands.contains( key ) ) {
      commands -= key
    }
  }

  /**
   * Registers a command with the helper.
   * If the command already exists, then its description
   * is overridden.
   * @param key The command key for the command
   * @param help Help information for the command
   */
  def registerCommand( key: CommandKey,
		       help: String ) {
    commands += key -> help
  }

  /**
   * Registers a command with the helper.
   * @param name The name of the command
   * @param numParams The number of parameters for the command
   * @param help Help information for the command
   */
  def registerCommand( name: String,
		       numParams: Int,
		       help: String ) {
    registerCommand( CommandKey( name,
				 numParams ),
		     help )
  }

  /**
   * Determines if a command with the given key is registered.
   * @param key The key for the command
   * @return true if the command is registered, else false
   */
  def isRegistered( key: CommandKey ) =
    commands.contains( key )

  /**
   * Determines if a command with the given name and number
   * of params is registered.
   * @param name The name of the command
   * @param numParams The number of params the command takes
   * @return true if the command is registered, else false
   */
  def isRegistered( name: String,
		    numParams: Int ): Boolean = {
    isRegistered( CommandKey( name, numParams ) )
  }

  /**
   * The basic, no arg help command.
   * Prints out the GENERAL_HELP message
   */
  def apply() =
    HelpCommand.GENERAL_HELP 

  /**
   * Help command with a given command key.
   * Prints out information about command, or NO_HELP
   * if there is no associated help with the command.
   * @param key The key associated with the command
   */
  def apply( key: CommandKey ) = {
    key.name + "/" + key.numParams + ": " +
    ( if ( isRegistered( key ) )
        commands( key )
      else
	HelpCommand.NO_HELP )
  }

  /**
   * Help command with a given command name and number of params.
   * Prints out information about the command, or NO_HELP
   * if there is no associated help with the command.
   * @param name The name of the command to get information about
   * @param numParams The number of parameters associated with the command
   */
  def apply( name: String, numParams: Int ): String =
    apply( CommandKey( name, numParams ) )

  /**
   * Gets all keys associated with the given name.
   * Note that the keys are returned in no particular order.
   * @param name The name it is associated with
   * @return A sequence of keys that bare this name
   */
  def getKeys( name: String ): Seq[ CommandKey ] =
    commands.keys.toList.filter( _.name == name )

  /**
   * Gets all keys associated with the given number of parameters.
   * Note that keys are returned in no particular order.
   * @param numParams The number of parameters associated with the command
   * @return A sequence of keys with this number of parameters.
   */
  def getKeys( numParams: Int ): Seq[ CommandKey ] =
    commands.keys.toList.filter( _.numParams == numParams )
  
  /**
   * Help command with a given string argument.
   * Prints out information about every command with that name, in order
   * of the number of parameters.
   * @param name The name of the command to get information about
   */
  def apply( name: String ): String =
    getKeys( name ).toList.sortWith( _.numParams < _.numParams )
                          .map( apply( _ ) )
                          .mkString( "", "\n", "" )
}

/**
 * <p>Class that processes text in a line-based fashion.
 * Based upon what is read in, different actions are performed.
 * It is intended that each line holds a single command.  Commands
 * take the following format:
 * commandName option1 option2 ... optionN
 * Items that are quoted are considered one item, as in:
 * myCommand "this is a string" "this is another string"
 * ...where "this is a string" and "this is another string" are each
 * one item.
 * If one intends to put a quote into something, as in:
 * quote"quote
 * ...then use a backslash:
 * quote\"quote
 * In this case, quote"quote would be considered one item.
 * If a backslash is used on a non-backslashable item, as in:
 * \g
 * ...then it is considered an error.  Note that only \n is understood as
 * a control character.</p>
 * <p>Note that the following commands are predefined:
 * show_commands: shows all text commands that are available
 * help [ commandName ]: Displays a general help message, or the help message
 * associated with the given command</p>
 * <p>Commands have a strict number of params that they take.  That said, it
 * is possible to register multiple commands with the same name, but with
 * different numbers of params.  It is even possible to register a command
 * with the same number of parameters but with different types for the
 * parameters.  (Note that in such a case, register in the order that commands
 * should be tried; for instance, if there are two options with the
 * signatures: String, String and Int, Int, then register with Int, Int
 * first.  String, String will <b>always</b> match, and Int, Int will be
 * effectively ignored.</p>
 * @param help The help command associated with the processor
 * @author Kyle Dewey
 */
class TextCommandProcessor( private val help: HelpCommand ) {
  import TextCommandProcessor._

  // begin instance variables
  // a key is mapped to multiple commands that can execute
  // a command.  Each value consists of an object and a method
  // to invoke on that object
  private var commands: Map[ CommandKey, Seq[ Pair[ AnyRef, Method ] ] ] = Map()
  // end instance variables

  // register all versions of the help command
  registerCommand( HelpCommand.HELP_COMMAND_NAME,
		   Some( HelpCommand.HELP0 ),
		   help,
		   help.getClass.getMethod( "apply", null ) )
  registerCommand( HelpCommand.HELP_COMMAND_NAME,
		   Some( HelpCommand.HELP1 ),
		   help,
		   help.getClass.getMethod( "apply", classOf[ String ] ) )
  registerCommand( HelpCommand.HELP_COMMAND_NAME,
		   Some( HelpCommand.HELP2 ),
 		   help,
		   help.getClass.getMethod( "apply",
					    classOf[ String ],
					    classOf[ Int ] ) )

  // register the show_commands command
  registerCommand( SHOW_COMMANDS_NAME,
		   Some( SHOW_COMMANDS_HELP ),
		   this,
		   this.getClass.getMethod( "showCommands", null ) )

  /**
   * Creates a new processor with the default help command.
   */
  def this() =
    this( new HelpCommand() )
  
  /**
   * Implementation of the show_commands command.
   * @return The text of all the commands printed out
   */
  def showCommands() = {
    // note that multiple commands can have the same names, but
    // different numbers of params
    val names = Set() ++ commands.keys.map( _.name )
    names.toList.sortWith( _ < _ ).map( help( _ ) ).mkString( "", "\n", "" )
  }

  /**
   * Unregisters a command with the given key and signature.
   * @param key The key for the parameter
   * @param signature The signature of the method
   */
  def unregisterCommand( key: CommandKey,
			 signature: Seq[ Class[ _ ] ] ) {
    if ( commands.contains( key ) ) {
      val oldValues = commands( key )
      val sigArray = signature.toArray.deep
      // only get elements that differ from the given signature
      val newValues = oldValues.filter( !_._2.getParameterTypes
                                             .deep
                                             .equals(sigArray) )
      if ( newValues.isEmpty ) {
	commands -= key
	help.unregisterCommand( key )
      } else {
	commands += key -> newValues
      }
    }
  }

  /**
   * Unregisters the command with the given name,
   * number of params, and signature.
   * @param name The name of the command to remove
   * @param numParams The number of params the command takes
   * @param signature The method signature for the command
   */
  def unregisterCommand( name: String,
			 numParams: Int,
			 signature: Seq[ Class[ _ ] ] ) {
    unregisterCommand( CommandKey( name, numParams ), signature )
  }

  /**
   * Unregisters all commands with the given key
   * @param key The key
   */
  def unregisterCommands( key: CommandKey ) {
    if ( commands.contains( key ) ) {
      commands -= key
      help.unregisterCommand( key )
    }
  }
      
  /**
   * Unregisters all commands with the given name and number of params.
   * Use with caution!
   * @param name The name of the command to remove
   */
  def unregisterCommands( name: String, numParams: Int ) {
    unregisterCommands( CommandKey( name, numParams ) )
  }

  /**
   * Registers the given method with the command processor.
   * @param name The name of the command
   * @param help Help that is associated with the command.  Specify
   * None if no help is associated with the command
   * @param theObject The object that the method is executed on
   * @param method The method that can execute commands
   * @throws InvalidMethodException If the given method is not
   * appropriate for use by the interpreter
   */
  def registerCommand( name: String, 
		       help: Option[ String ],
		       theObject: AnyRef,
		       method: Method ) {
    if ( !paramsAcceptable( method ) ) {
      throw new InvalidMethodException( "All method parameters must be a " +
				        "combination of String, int, char, " +
				        "and double" )
    }

    val key = CommandKey( name, 
			  method.getParameterTypes.length )
    // append this method onto the methods associated with this key
    commands += key -> ( commands.getOrElse( key, Seq() ) ++ 
			Seq( ( theObject, method ) ) )
    if ( help.isDefined ) this.help.registerCommand( key, help.get )
  }

  /**
   * Registers the given command, without an associated help message
   * @param name The name of the command
   * @param theObject The object that the method is executed on
   * @param method The method that can execute commands
   * @throws InvalidMethodException If the given method is not
   * appropriate for use by the interpreter
   */
  def registerCommand( name: String,
		       theObject: AnyRef,
		       method: Method ) {
    registerCommand( name,
		     None,
		     theObject,
		     method )
  }

  /**
   * Registers the given command with the command processor.
   * Assumes that the given item has an apply method associated with it.
   * @param name The name of the command
   * @param help Help information associated with the given command.
   * Specify None if there is none
   * @param theObject The object that is associated with this command
   * @throws MethodDeterminationException If an appropriate method could
   * not be determined
   */
  def registerCommand( name: String,
		       help: Option[ String ],
		       theObject: AnyRef ) {
    registerCommand( name,
		     help,
		     theObject,
		     getMethod( theObject.getClass ) )
  }

  /**
   * Registers the given command with the command interpreter.
   * No help is associated with the command.
   * @param name The name of the command
   * @param theObject The object that is associated with this command
   * @throws MethodDeterminationExeption If an appropriate method
   * could not be determined
   */
  def registerCommand( name: String,
		       theObject: AnyRef ) {
    registerCommand( name,
		     None,
		     theObject )
  }
			 
  /**
   * Executes the given command with the given arguments.
   * Note that types will be converted as neccessary
   * @param name The name of the command to execute
   * @param args The arguments to the command
   * @return The text that the underlying command returned
   * @throws UnknownCommandException If a command with this name and these
   * parameters doesn't exist
   */
  def executeCommand( name: String, args: Seq[ String ] ) = {
    val candidates = commands.get( CommandKey( name, args.length ) )
    // make sure we have a candidate
    if ( candidates.isEmpty ) {
      throw new UnknownCommandException( "Command with the name: " +
					 name + " that takes " +
					 args.length + " arguments " +
					 "doesn't exist" )
    }

    // try to execute each until one works
    val retval = execute( candidates.get, args )

    // make sure we executed one
    if ( retval.isEmpty ) {
      throw new UnknownCommandException( "Command with the neccessary " +
					 "argument types doesn't exist" )
    }
    retval.get
  } // executeCommand

  /**
   * Parses the given command line, and executes any commands on
   * the line.
   * @param line The line to parse
   * @throws StringParseException If the line was unable to be parsed
   * @throws UnknownCommandException If a command with this name and
   * these parameters doesn't exist
   */
  def executeCommandLine( line: String ): String = {
    val tokens = parseLine( line ).toList
    executeCommand( tokens.head,
		    tokens.tail )
  }
}

/**
 * Contains helper routines and values for the interactive
 * text interpreter.
 * @author Kyle Dewey
 */
object InteractiveTextInterpreter {
  // begin constants
  val EXIT_COMMAND_NAME = "exit"
  val EXIT_COMMAND_HELP = "Exits the interpreter"
  val DEFAULT_PROMPT = "interpreter> "
  // end constants
}

/**
 * Tests the interactive text interpreter.
 * @author Kyle Dewey
 */
object TestText extends App {
  /**
   * Main function to test mainLoop()
   * This testing doesn't go well in the REPL; we close
   * System.in out from under it, causing the REPL itself
   * to throw an exception.  Additionally, the REPL shuts
   * off echo for us.
   */
  val x = new InteractiveTextInterpreter
  x.mainLoop()
}

/**
 * An interactive command line text interpreter.
 * @param prompt A text prompt to display before every command
 * @author Kyle Dewey
 */
class InteractiveTextInterpreter( var prompt: String ) extends TextCommandProcessor {
  import InteractiveTextInterpreter._
  import java.io._

  // begin instance variables
  private var shouldExit = false
  // end instance variables

  // register the exit command
  registerCommand( EXIT_COMMAND_NAME,
		   Some( EXIT_COMMAND_HELP ),
		   this,
		   this.getClass.getMethod( "exit", null ) )

  /**
   * Creates a new interpreter with DEFAULT_PROMPT.
   */
  def this() =
    this( InteractiveTextInterpreter.DEFAULT_PROMPT )

  /**
   * The command to exit.
   * @return Bye.
   */
  def exit() = {
    shouldExit = true
    "Bye."
  }

  /**
   * Formats a string for printing.
   * @param string The string to take
   * @return The formatted string
   */
  def formatString( string: String ) = {
    if ( string != "" ) string + "\n"
    else string
  }

  /**
   * The main processing loop of the interpreter.
   * Merely waits for user input and executes forever,
   * until the exit command is entered.
   * @param input Input stream to get commands from
   * @param output Output stream to print output to
   * @param error Error stream to print errors to
   */
  def mainLoop( input: InputStream, 
	        output: PrintStream,
	        error: PrintStream ) {
    import java.util._
    val userInput = new Scanner( input )
    while( !shouldExit ) {
      try {
	output.print( prompt )
	output.print( formatString( executeCommandLine( userInput.nextLine ) ) )
      } catch {
	case e: StringParseException => error.println( e.getMessage )
	case e: UnknownCommandException => error.println( e.getMessage )
      }
    }
    input.close()
  }

  /**
   * The main processing loop of the interpreter.
   * Uses System.in, System.out, and System.err for the
   * input, output, and error streams, respectively.
   */
  def mainLoop() {
    mainLoop( System.in,
	      System.out,
	      System.err )
  }
}

