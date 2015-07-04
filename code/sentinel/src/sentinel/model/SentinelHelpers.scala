/*
 * SentinelHelpers.scala
 *
 * Version:
 *     $Id: SentinelHelpers.scala,v 1.5 2011/06/20 22:40:51 kyledewey Exp $
 *
 * Revisions:
 *      $Log: SentinelHelpers.scala,v $
 *      Revision 1.5  2011/06/20 22:40:51  kyledewey
 *      Added the numTimesSeen() method.
 *
 *      Revision 1.4  2011/06/04 05:11:02  kyledewey
 *      Put the makeTable() and related methods into this file.
 *
 *      Revision 1.3  2011/05/31 00:02:59  kyledewey
 *      Added the moveSubsequence() method.
 *
 *      Revision 1.2  2011/05/28 02:37:45  kyledewey
 *      Moved mapFind to sentinel.model.SentinelHelpers.
 *
 *      Revision 1.1  2011/05/27 18:48:02  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model

/**
 * Holds helper routines that are generic enough to be of use throughout
 * Sentinel.
 * @author Kyle Dewey
 */
object SentinelHelpers {
  /**
   * Given a bunch of objects, it will generate a map.
   * Multiple values may be included with the same key
   * @param objects The sequence of objects
   * @param key Function used to get a key for an object
   * @param value Function use to get a value for an object
   * @return A mapping generated from the given objects
   */
  def multiMap[ O, K, V ]( objects: Seq[ O ],
			   key: O => K,
			   value: O => V ): Map[ K, Seq[ V ] ] = {
    var retval: Map[ K, List[ V ] ] = Map()
    objects.foreach( current => {
      val currentKey = key( current )
      val currentValue = value( current )
      val baseList =
	if ( retval.contains( currentKey ) ) retval( currentKey )
	else List[ V ]()
      retval += Pair( currentKey, 
		      currentValue :: baseList )
    } )
    retval.transform( ( key, list ) =>
      list.reverse.toSeq )
  }

  /**
   * Given a sequence of key value pairs, makes a map so that
   * for each key, all values seen for that key are represented.
   * @param pairs The sequence of key/value pairs
   * @return A map where each key is represented, and the values are
   * all values seen for the given key.  Order is retained.
   */
  def multiMap[ K, V ]( pairs: Seq[ Pair[ K, V ] ] ): Map[ K, Seq[ V ] ] = 
    multiMap( pairs, 
	      ( pair: Pair[ K, V ] ) => pair._1, 
	      ( pair: Pair[ K, V ] ) => pair._2 )

  /**
   * Like a find, but it maps the given element beforehand.  It will return
   * the mapped element, or None if there wasn't one.
   * @param seq The sequence
   * @param map The mapping function
   * @param pred The predicate to use on the result of the map
   * @return The first mapped element for which pred( map( element ) ) was
   * true, or None if there wasn't one
   */
  def mapFind[ E, M ]( seq: Seq[ E ], map: E => M, pred: M => Boolean ) = {
    var retval: Option[ M ] = None
    seq.find( element => {
      val mapped = map( element )
      val findResult = pred( mapped )
      if ( findResult ) {
	retval = Some( mapped )
      }
      findResult
    })
    retval
  }

  /**
   * Moves the given subsequence of a Seq to another position with the seq
   * @param start The starting position of the subsequence
   * @param end The ending position of the subsequence
   * @param to Where to put the subsequence
   * @param seq The whole sequence
   * @return The new subsequence holding this information
   */
  def moveSubsequence[ T ]( start: Int,
			    end: Int,
			    to: Int,
			    seq: Seq[ T ] ) = {
    val move = seq.slice( start, end + 1 )
    val toItem = seq( to )

    if ( to == start ) {
      seq
    } else if ( to < end ) {
      val before = seq.slice( 0, to )
      val between = seq.slice( to + 1, start )
      val after = seq.slice( end + 1, seq.length )
      before ++ move ++ Seq( toItem ) ++ between ++ after
    } else { // to > end
      val beforeStart = seq.slice( 0, start )
      val beforeTo = seq.slice( end + 1, to )
      val between = seq.slice( to + 1, to + move.length )
      val retval = beforeStart ++ beforeTo ++ Seq( toItem ) ++ between ++ move
      retval ++ seq.slice( retval.length, seq.length )
    }
  }

    /**
   * Makes a table of the given width and height.
   * Each cell is initialized with the result of a given function.
   * @param numRows The number of rows in the table
   * @param numColumns The number of columns in the table
   * @param initializer Creates the initial element for each cell.  Takes
   * the row and column as a param
   * @return A table of the given width and height initialized for
   * each cell with the given initializer
   */
  def makeTable[T : ClassManifest]( numRows: Int, numColumns: Int, initializer: ( Int, Int ) => T ): Array[Array[T]] = {
    val retval: Array[Array[T]] = Array.ofDim[T](numRows, numColumns)
    0.until(numRows).foreach(row =>
      0.until(numColumns).foreach(column =>
        retval(row)(column) = initializer(row, column)))
    retval
  }

  /**
   * Like <code>makeTable</code>, but it works with a matrix
   * @param size The size of the matrix (NxN)
   * @param initializer The initializer to use
   * @return A matrix
   */
  def makeMatrix[T: ClassManifest]( size: Int, initializer: ( Int, Int ) => T ) =
    makeTable( size, size, initializer )

  /**
   * Makes a table of sets.  Note that since sets are immutable,
   * each cell is initialized to the same set.
   * @param numRows The number of rows in it
   * @param numColumns The number of columns in it
   * @return A matrix of sets.
   */
  def makeTableOfSets( numRows: Int, numColumns: Int ) = {
    val set = Set()
    makeTable( numRows, 
	       numColumns, 
	       ( row, column ) => set )
  }

  /**
   * Given a sequence, determines the number of times a given item has been
   * seen.
   * @param items The sequence of items
   * @return A mapping of the items to the number of times that they
   * have been seen
   */
  def numTimesSeen[ T ]( items: Seq[ T ] ) = {
    var retval = Map() ++ items.map( Pair( _, 0 ) )
    items.foreach( item => {
      val oldNum = retval( item )
      retval += Pair( item, oldNum + 1 )
    })
    retval
  }
}
