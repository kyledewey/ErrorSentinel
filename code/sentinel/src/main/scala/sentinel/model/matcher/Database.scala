/*
 * Database.scala
 */

package sentinel.model.matcher

import java.sql._
import sentinel.model._
import sentinel.model.matcher.StringHelpers._

/**
 * Defines a database handle.  Note that handles are loaded lazily.
 * @param name The name of the database handle
 * @param driver The driver the handle is to use
 * @param connectString The string used to connect to the database
 * @param userName The name of the user to connect with.
 * @param password The password to connect with.
 * @author Kyle Dewey
 */
class DatabaseHandle( val name: String, 
		      val driver: String, 
		      val connectString: String,
		      val userName: Option[ String ],
		      val password: Option[ String ] ) {
  lazy val connection = loadHandle()

  /**
   * Loads in the database handle.
   * @return The connection
   * @throws Exception If something goes wrong
   */
  def loadHandle() = {
    Class.forName( driver )
    if ( userName.isDefined ||
	 password.isDefined ) {
      DriverManager.getConnection( connectString, 
				   userName.getOrElse( "" ),
				   password.getOrElse( "" ) )
    } else {
      DriverManager.getConnection( connectString )
    }
  }

  /**
   * Sets the given statement to use the given params.
   * Note that for all parameters, setObject is used without typing information.
   * @param statement The statement
   * @param params The parameters for the statement.
   * @throws Exception If an exception was thrown while processing it
   */
  def setQueryParams( statement: PreparedStatement, params: Seq[ String ] ) {
    1.to( params.size ).foreach( paramNum =>
      statement.setObject( paramNum, 
			   params( paramNum - 1 ) ) )
  }

  /**
   * Runs the given query with the given parameters.
   * @param query The query to run
   * @param params The parameters for the query
   * @return The first item in the first row of the query, or None if there
   * were no results
   * @throws Exception If an exception was thrown upon trying to
   * run the query
   */
  def runQuery( query: String, params: Seq[ String ] ) = {
    var statement: PreparedStatement = null
    var toThrow: Exception = null
    var retval: Option[ String ] = None

    try {
      statement = connection.prepareStatement( query )
      setQueryParams( statement, params )
      val results = statement.executeQuery
      if ( results.next ) {
	val asObject = results.getObject( 1 )
	val asString =
	  if ( asObject == null ) "" else asObject.toString
	retval = Some( asString )
      }
    } finally {
      if ( statement != null ) {
	statement.close()
      }
    }

    retval
  }
}

/**
 * Contains routines for parsing in the database handle file.
 * Note that handles are loaded lazily.
 * @author Kyle Dewey
 */
object DatabaseHandleParser {
  import scala.xml._

  val DEFAULT_FILE_NAME = "Sentinel_DBHandles.xml"
  val DEFAULT_FILE_PATH =
    "xml" + java.io.File.separator + DEFAULT_FILE_NAME

  def getNodes( base: Node, tag: String ) =
    XMLHelpers.getNodes( base, tag, MatchException( _ ) )

  def getText( base: Node, tag: String ) =
    XMLHelpers.getText( base, tag, MatchException( _ ) )

  /**
   * Parses in the given database handle node.
   * @param node The node holding the database handle information
   * @return A database handle for the node
   */
  def parseDBHandle( node: Node ) = 
    new DatabaseHandle( getText( node, "Name" ),
		        getText( node, "Driver" ),
		        getText( node, "DatabaseConnect" ),
		        XMLHelpers.getOptionalText( node, "UserName" ),
		        XMLHelpers.getOptionalText( node, "Password" ) )
    
  /**
   * Parses in database handles from the given XML node.
   * @param node The node containing database handles.
   * @return Database handles, in the same order as seen in the file
   */
  def parseDBHandles( node: Node ): Seq[ DatabaseHandle ] =
    ( node \ "DatabaseHandle" ).map( parseDBHandle( _ ) )

  /**
   * Parses in database handles from the given file.
   * @param path The file's path to parse them in from
   * @throw MatchException If the file is malformed, doesn't exist, or could
   * not be read for whatever reason
   */
  def parseDBHandles( path: String ): Map[ String, DatabaseHandle ] = {
    try {
      Map() ++ parseDBHandles( XML.loadFile( path ) ).map( handle =>
	(handle.name, handle) )
    } catch {
      case e: MatchException => throw e
      case e: Exception => throw new MatchException( e.getMessage )
    }
  }

  /**
   * Parses in database handles from <code>DEFAULT_FILE_PATH</code>.
   * @throw MatchException If the file is malformed, doesn't exist, or could
   * not be read for whatever reason
   */
  def parseDBHandles(): Map[ String, DatabaseHandle ] =
    parseDBHandles( DEFAULT_FILE_PATH )
}

/**
 * Manages open database connections.
 * It would be overcomplicated to pass all the parameters needed to connect
 * to a database every time a database matcher would be used, so parameters
 * are stored elsewhere.
 * @author Kyle Dewey
 */
object DatabaseHandleManager {
  lazy val handle = 
    DatabaseHandleParser.parseDBHandles()
}

/**
 * Base class for database matchers and replacers.
 * Works like a normal instance.  Doesn't extend instance to prevent
 * it from being used as an instance.
 * @param className The name of the class
 * @param params Params to the matcher<ul>
 * <li>"handle": The name of the database handle to use.  Must be defined
 * in another file.  See <code>DatabaseHandleParser</code> for more
 * information.</li>
 * <li>"query": The parameterized query to use.  Uses question marks for
 * parameter positions, as is the typical parameterized SQL style.</li>
 * <li>"params": The parameters for the query.</li></ul>
 * @author Kyle Dewey
 */
class Database( val className: String,
	        val params: Seq[ NamedParam ] ) {
  import sentinel.model.InstanceFactory._
  private val handle = param( "handle", params )
  private val query = param( "query", params )
  private val queryParams: scala.Array[Param] = 
    opAsArray( "params", params ).getOrElse( scala.Array.empty[ Param ] )
  
  /**
   * Gets the first column of the first row of a query.
   * @return The first column of the first row of a query, or None
   * if the query returned no results.
   * @throws Exception If an exception occurred while processing the query.
   */
  def runQuery() =
    DatabaseHandleManager.handle( handle.sentStringValue )
                         .runQuery( query.sentStringValue,
				    toStrings( queryParams ) )
}

/**
 * Matcher that utilizes a relational database.
 * If a given query with given query parameters returned a result,
 * then this is considered a match.  Otherwise it's considered
 * no match.
 * @param className The name of the class
 * @param params Params to the matcher<ul>
 * <li>"handle": The name of the database handle to use.  Must be defined
 * in another file.  See <code>DatabaseHandleParser</code> for more
 * information.</li>
 * <li>"query": The parameterized query to use.  Uses question marks for
 * parameter positions, as is the typical parameterized SQL style.</li>
 * <li>"params": The parameters for the query.</li></ul>
 * @author Kyle Dewey
 */
class DatabaseMatcher( className: String, params : Seq[ NamedParam ] ) 
extends Database( className, params ) with Matcher {
  /**
   * Returns true if the database query with the given parameters
   * had a result.  Otherwise it returns false
   * @return true if the query had results, else false
   * @throws MatchException If there was any problem with processing
   * the query
   */
  override def matches() =
    try {
      runQuery.isDefined
    } catch {
      case e: Exception => throw new MatchException( e.toString )
    }
}
