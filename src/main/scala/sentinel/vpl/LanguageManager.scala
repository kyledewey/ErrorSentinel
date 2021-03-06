/*
 * LanguageManager.scala
 */

package sentinel.vpl

import javax.swing.tree._
import sentinel.model._

/**
 * Exception thrown when a function could not be added to a language
 * @param message The message to show
 * @param exception The exception that triggered this one
 * @author Kyle Dewey
 */
case class FunctionAddException( message: String, 
				 val exception: Option[ Exception ] ) 
extends Exception( message ) {
  /**
   * Creates an exception based on the given exception
   * @param exception The exception that triggered this one
   */
  def this( exception: Exception ) =
    this( exception.getMessage,
	  Some( exception ) )

  /**
   * Creates an exception with the given message
   * @param message The message to show
   */
  def this( message: String ) =
    this( message,
	  None )
}

/**
 * Represents a language that has been loaded in.
 * @author Kyle Dewey
 */
trait LoadedLanguage extends Manageable {
  /**
   * Adds a function to this language.
   * @pre The function has been registered
   * @param function The function to add
   * @throws FunctionAddException If we could not add this function
   */
  def addFunction( function: InstanceFactory[ _ ] ): Unit
}

/**
 * Holds constants and static routines for LanguageNode.
 * @author Kyle Dewey
 */
object LanguageNode {
  /**
   * Writes out the given class to the given file.
   * @param theClass The class to write
   * @param fileName The name of the file to write to
   * @throws FunctionAddException If an error occurred on write
   */
  def writeClass( theClass: InstanceFactory[ _ ],
		  fileName: String ) {
    import sentinel.model.writer.xml._

    try {
      XMLWriter.writeClass( theClass,
			    fileName,
			    true )
    } catch {
      case e: Exception => throw new FunctionAddException( e )
    }
  }

  /**
   * Converts the given class to a node.
   * @param theClass the class to convert
   * @return The class as a node
   */
  def classToNode( theClass: InstanceFactory[ _ ] ) =
    SentinelTreeHelpers.factoryToNode( theClass )
}

/**
 * Represents a language in Sentinel.
 * When a function is added, we must update the components tree with it.
 * In addition, we must write the function to a file.
 * @param treeModel The components tree model associated with the board
 * this language is in
 * @param rootNode The node associated with the language
 * @author Kyle Dewey
 */
class SentinelLanguage( val treeModel: ComponentsTreeModel,
		        val rootNode: DefaultMutableTreeNode ) 
extends NamedManageable( rootNode ) with LoadedLanguage {
  /**
   * Adds a function to this language.
   * Note that this will take care of writing out the function.
   * @param function The function to add
   * @throws FunctionAddException If we couldn't add the function.
   * Specifically, an IO error occurred.
   */
  def addFunction( function: InstanceFactory[ _ ] ) {
    import LanguageNode._
    writeClass( function, filePath )
    treeModel.insertNodeInto( classToNode( function ),
			      rootNode )
  }

  /**
   * Gets the file name of the node.
   * @return The file name
   */
  def filePath() =
    rootNode.getUserObject
            .asInstanceOf[ LanguageKey ]
            .filePath
}

/**
 * Manages languages that have been loaded in.
 * @param treeModel The model associated with the components tree
 * @author Kyle Dewey
 */
class LanguageManager( val treeModel: ComponentsTreeModel )
extends Manager[ LoadedLanguage ] {
  /**
   * Adds the given function to the root of the model.
   * @param function The function to add
   */
  def addFunctionToRoot( function: InstanceFactory[ _ ] ) {
    treeModel.insertNodeIntoRoot( LanguageNode.classToNode( function ) ) 
  }

  /**
   * Adds the given function.
   * It will try to add to the default language.  If there isn't a default,
   * it will add it to the root.
   * @param function The function to add
   */
  def addFunction( function: InstanceFactory[ _ ] ) {
    val default = getDefault
    if ( default.isDefined ) {
      default.get.addFunction( function )
    } else {
      addFunctionToRoot( function )
    }
  }    

  /**
   * Adds a language to this manager.
   * Languages simply hold a number of functions.
   * @param language The language to add
   */
  def addLanguage( language: LoadedLanguage ) {
    addItem( language )
  }

  /**
   * Adds all the languages for the given project to this manager.
   * Assumes that all child nodes of the given node are functions.
   * @param projectNode The node correlating to the project
   * @param creator Something that can make LoadedLanguage given the
   * node that correlates to the given language
   */
  def addProject( projectNode: DefaultMutableTreeNode,
		  creator: DefaultMutableTreeNode => LoadedLanguage ) {
    val children = projectNode.children
    while( children.hasMoreElements ) {
      addLanguage( creator( children.nextElement
			            .asInstanceOf[ DefaultMutableTreeNode ] ) )
    }
  }
}
