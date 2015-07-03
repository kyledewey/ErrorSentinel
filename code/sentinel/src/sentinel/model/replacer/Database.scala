/*
 * Database.scala
 * 
 * Version:
 *     $Id: Database.scala,v 1.1 2011/06/04 07:43:26 kyledewey Exp $
 * 
 * Revisions:
 *      $Log: Database.scala,v $
 *      Revision 1.1  2011/06/04 07:43:26  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.replacer

import sentinel.model.matcher._

/**
 * Returns a record from a database.
 * Note that due to Sentinel's restriction of only one return value,
 * this can only get a single cell of a database at a time.
 * @param className The name of the class
 * @param params Parameters to the replacer<ul>
 * <li>"handle": The name of the database handle to use.  Must be defined
 * in another file.  See <code>DatabaseHandleParser</code> for more
 * information.</li>
 * <li>"query": The parameterized query to use.  Uses question marks for
 * parameter positions, as is the typical parameterized SQL style.</li>
 * <li>"params": The parameters for the query.</li></ul>
 * @author Kyle Dewey
 */
class DatabaseReplacer( className: String, params: Seq[ NamedParam ] )
extends Database( className, params ) with Replacer {
  import sentinel.model.Replacer._
  /**
   * Returns the first column of the first row.
   * If there isn't a value, it throws a replace exception.
   * @return The first column of the first row in the database
   * @throws ReplaceException If an exception was thrown while accessing
   * the database, or if there were no results.
   */
  override def replace() =
    try {
      val results = runQuery
      if ( results.isEmpty ) {
	throw new ReplaceException( "No results returned for query." )
      } else {
	results.get
      }
    } catch {
      case e: ReplaceException => throw e
      case e: Exception => throw new ReplaceException( e.toString )
    }
}
 
