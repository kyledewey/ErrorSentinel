/*
 * SentinelTree.scala
 */

package sentinel.vpl

import sentinel.model._
import sentinel.model.parser._
import scala.xml._
import java.io._
import javax.swing.tree._

/**
 * Holds the different kinds of tree keys that are possible.
 * @author Kyle Dewey
 */
object TreeKeyType extends Enumeration {
  type TreeKeyType = Value
  val ProjectType,
      LanguageType,
      ComponentType = Value
}

/**
 * Represents an item in a tree.  The item can represent
 * a project, a language, or a component.
 * @author Kyle Dewey
 */
trait TreeKey {
  import TreeKeyType._

  /**
   * Gets the name of this key.
   * @return The name of this key
   */
  def name(): String

  /**
   * Gets the type of this key
   * @return The type of this key
   */
  def getType(): TreeKeyType

  /**
   * Converts this key to a string.
   * @return The name of this key
   */
  override def toString() =
    name
}

/**
 * Represents something with an associated file in a tree.
 * This is true of languages and projects.
 * @author Kyle Dewey
 */
trait FileKey extends TreeKey {
  /**
   * Gets the path to the file.
   * @return The path to the file
   */
  def filePath(): String 
}

/**
 * Represents a project in a tree.
 * @author Kyle Dewey
 */
trait ProjectKey extends FileKey {
  /**
   * Gets that this is a project type.
   * @return <code>TreeKeyType.ProjectType</code>
   */
  def getType() =
    TreeKeyType.ProjectType
}

/**
 * Represents a language in a tree.
 * @author Kyle Dewey
 */
trait LanguageKey extends FileKey {
  /**
   * Gets that this is a language type
   * @return <code>TreeKeyType.LanguageType</code>
   */
  def getType() =
    TreeKeyType.LanguageType
}

/**
 * Represents a component key in a tree.
 * Keys must have a way of showing a human-readable
 * name.  Additinally, it must be possible to make a node from a key.
 * @author Kyle Dewey
 */
trait ComponentKey[ T <: AnyRef, U ] extends Describable {
  /**
   * Gets the name of this key.
   * This is based on the name given by the describer.
   * @return <code>describer.name</code>
   */
  def name() =
    describer.name

  /**
   * Gets that this is a ComponentType
   * @return <code>TreeKeyType.ComponentType</code>
   */
  def getType() =
    TreeKeyType.ComponentType

  /**
   * Creates a node from this key
   * @return A node created from this key
   */
  def makeNode(): Node[ T, U ]

  /**
   * Returns the name of the key
   * @return The name of the key
   */
  override def toString() =
    name

  /**
   * Determines if one key equals another key.
   * @param other The other key to compare against
   * @return true if the keys equal, else false
   */
  def equals( other: Any ): Boolean
}

/**
 * Represents a file key with a static name and path.
 * @param name The name of the key
 * @param filePath The path to the file
 * @author Kyle Dewey
 */
abstract class StaticFileKey( val name: String, val filePath: String ) 
extends FileKey {}
  
/**
 * A project key in sentinel
 * @param name The name of the project key
 * @param filePath Path to the project file
 * @author Kyle Dewey
 */
class SentinelProjectKey( name: String, filePath: String )
extends StaticFileKey( name, filePath ) with ProjectKey {}

/**
 * A language key in sentinel
 * @param name The name of the language key
 * @param filePath Path to the language file
 * @author Kyle Dewey
 */
class SentinelLanguageKey( name: String, filePath: String )
extends StaticFileKey( name, filePath ) with LanguageKey {}

/**
 * Represents a tree key for sentinel.
 * @param factory The factory that can make instances
 * on the name of the key
 * @author Kyle Dewey
 */
class SentinelComponentKey( val factory: InstanceFactory[ _ ] )
extends ComponentKey[ InstanceFactory[ _ ], Param ] {
  /**
   * Creates a node from this key.
   * @return A node based on this key
   */
  def makeNode() =
    SentinelNode( factory )
  
  /**
   * Determines if this key equals another key
   * @param other The other key to compare to
   * @return true if the keys equal, else false.
   */
  override def equals( other: Any ) = {
    var retval = false

    if ( other.isInstanceOf[ SentinelComponentKey ] ) {
      val asKey = other.asInstanceOf[ SentinelComponentKey ]
      retval = ( asKey.name == name &&
		 asKey.factory.eq( factory ) )
    }

    retval
  }

  /**
   * Gets something that can describe this tree.
   * Merely uses the factory's describer.
   * @return <code>factory.describer</code>
   */
  def describer() =
    factory.describer
}
		 
/**
 * Contains helper routines for <code>SentinelTree</code>.
 * @todo Figure out why the name "SentinelTree" causes a symbol not found error
 * @author Kyle Dewey
 */
object SentinelTreeHelpers {
  // a file name filter that only accepts xml files
  // and directories
  val xmlFilter = 
    new FileFilter() {
      def accept( file: File ) =
	file.isDirectory || file.getName.toLowerCase.endsWith( ".xml" )
    }

  /**
   * Gets the name of the given file, without the file extension.
   * The file name itself is not permitted to have periods, which
   * will be interpreted as a file extension.
   * @param file The file to get the name of
   * @return The name of the file, without its extension
   */
  def nameWithoutExtension( file: File ) = {
    val name = file.getName
    val periodLoc = name.indexOf( '.' )

    if ( periodLoc == -1 ) {
      // no extension
      name
    } else {
      name.substring( 0, periodLoc )
    }
  }

  /**
   * Takes a file.  If the file is a directory, then it will return
   * all the files and directories in lexicographical order, as
   * two separate items.  If it isn't a directory, this returns None
   * @param file The file to look at
   * @return The files and directories as a pair that are below this
   * directory, or None if we were given a file
   */
  def getFilesDirectories( file: File ) = 
    if ( file.isDirectory ) {
      Some( file.listFiles( xmlFilter )
                .toList
                .sortWith( _.getName < _.getName )
                .partition( _.isDirectory ) )
    } else {
      None
    }

  /**
   * Given a PreClass object, it will return a factory that can
   * create such classes.
   * @pre The class behind the PreClass object has been parsed in
   * @param preClass The pre class describing a class
   * @return A factory that can create classes of this type
   */
  def preClassToFactory( preClass: PreClass ) = {
    import sentinel.model._

    val creator = preClass.theType match {
      case ParamType.MatcherType => MatcherFactoryManager
      case ParamType.ReplacerType => ReplacerFactoryManager
      case _ => null // shouldn't be possible
    }

    creator.getFactory( preClass.name ).get 
  }

  /**
   * Creates a tree node given an instance factory.
   * @param factory The factory to create a node from
   * @return The tree node based on this factory
   */
  def factoryToNode( factory: InstanceFactory[ _ ] ) =
    new DefaultMutableTreeNode( new SentinelComponentKey( factory ),
			        false )
  /**
   * Makes a tree node that correlates to the given project file.
   * It is assumed that this won't be a leaf
   * @param file The file
   * @return A node that corresponds to this file
   */
  def projectFileToNode( file: File ) =
    new DefaultMutableTreeNode( 
      new SentinelProjectKey( nameWithoutExtension( file ),
			      file.getPath ) )

  /**
   * Makes a tree node that correlates to the given language file.
   * It is assumed that this won't be a leaf
   * @param file The file
   * @return A node that corresponds to this file
   */
  def makeLanguageNode( file: File ) =
    new DefaultMutableTreeNode( 
      new SentinelLanguageKey( nameWithoutExtension( file ),
			       file.getPath ) )

  /**
   * Takes an xml file
   * Parses in the file, and returns a new tree node.
   * The node's children will contain leaf nodes.
   * @pre The given file is an XML file containing language definitions
   * @param xmlFile The xml file.  Assumes it contains Sentinel language
   * definitions
   * @return A node that holds all the definitions in the file
   */
  def languageFileToNode( xmlFile: File ): DefaultMutableTreeNode = {
    import sentinel.project._
    
    val retval = makeLanguageNode( xmlFile )
    val language = new Language( xmlFile.getPath, "XML" )
    LanguageReader.readLanguages( Seq( language ) )

    language.classesInformation
            .toList
            .sortWith( _.name < _.name )
            .foreach( ( preClass: PreClass ) =>
	      retval.add( factoryToNode( preClassToFactory( preClass ) ) ) )
    retval
  }

  /**
   * Like <code>xmlFileToNode</code>, only it takes a path to a file
   * @param path Path to the xml file containing language definitions
   * @return A node that holds all the definitions in the file
   */
  def languageFileToNode( path: String ): DefaultMutableTreeNode = 
    languageFileToNode( new File( path ) )

  /**
   * Given a base directory for builtin XML components, recursively
   * traverses through, parsing XML files as they are encountered
   * and adding them to a tree node.
   * @param baseDir The base directory containing builtin components
   * @return A tree node containing the directory hierarchy
   */
  def makeTreeNode( baseDir: File ): DefaultMutableTreeNode = {
    val below = getFilesDirectories( baseDir )
    
    if ( below.isEmpty ) {
      languageFileToNode( baseDir )
    } else {
      val ( dirs,
	    files ) = below.get
      val retval = new DefaultMutableTreeNode( baseDir.getName )
      dirs.foreach( ( dir: File ) => 
	retval.add( makeTreeNode( dir ) ) )
      files.foreach( ( file: File ) =>
	retval.add( languageFileToNode( file ) ) )
      retval
    }
  }

  /**
   * Like <code>makeTreeNode</code>, but it works with a base directory
   * that is a path.
   * @param baseDir The base directory, as a string
   * @return A tree node containing the directory hierarchy
   */
  def makeTreeNode( baseDir: String ): DefaultMutableTreeNode =
    makeTreeNode( new File( baseDir ) )

  /**
   * Creates a tree node from the given XML node.
   * Assumes that the node is a "Definition" node.
   * @param xmlNode the xml node
   * @return A tree node representing the node
   */
  def makeTreeNode( xmlNode: scala.xml.Node ): DefaultMutableTreeNode = 
    languageFileToNode( ( xmlNode \ "File" ).head.text )

  /**
   * Makes a tree node that is based on a project.
   * Note that this assumes that both the project and the language
   * definition are in XML!
   * @todo Make this not require everything to be in XML
   * @param projectFile The file that holds the XML project definition
   * @return A tree node that holds all the definitions in the file
   */
  def makeTreeNodeFromProject( projectFile: File ): DefaultMutableTreeNode = {
    val content = XML.loadFile( projectFile )
    val retval = projectFileToNode( projectFile )

    ( content \\ "Definition" ).map( makeTreeNode( _ ) )
                               .foreach( retval.add( _ ) )
    retval
  }

  /**
   * Like <code>makeTreeNodeFromProject</code>, but it uses a path
   * instead of a file.
   * @param projectPath Path to an XML project file
   * @return A tree node that holds all the definitions in the file
   */
  def makeTreeNodeFromProject( projectPath: String ): DefaultMutableTreeNode =
    makeTreeNodeFromProject( new File( projectPath ) )

  /**
   * Makes a root node for a tree in Sentinel, using the given builtins
   * base directory.
   * @param builtinDir The base directory containing builtins
   * @return A tree that contains the items in the base directory
   */
  def makeTreeNodeFromDir( builtinDir: String ): DefaultMutableTreeNode = {
    val retval = new DefaultMutableTreeNode( "Components" )
    retval.add( makeTreeNode( builtinDir ) )
    retval
  }

  /**
   * Makes a root node for a tree in Sentinel, using the given
   * builtins base directory and the project file.
   * If the project file is None, then it won't be used
   * @param builtinBaseDir Base directory for builtins
   * @param projectPath Path to a project file, or None if there is no project
   * @return A tree node that contains the given items
   */
  def makeTreeNode( builtinBaseDir: String, 
		    projectPath: Option[ String ] ): DefaultMutableTreeNode = {
    val retval = makeTreeNodeFromDir( builtinBaseDir )
    if ( projectPath.isDefined ) {
      retval.add( makeTreeNodeFromProject( projectPath.get ) )
    }
    retval
  }
}
