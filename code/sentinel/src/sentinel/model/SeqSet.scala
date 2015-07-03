/*
 * SeqSet.scala
 *
 * Version:
 *     $Id: SeqSet.scala,v 1.3 2011/06/17 19:51:52 kyledewey Exp $
 * 
 * Revisions:
 *      $Log: SeqSet.scala,v $
 *      Revision 1.3  2011/06/17 19:51:52  kyledewey
 *      Now uses mutable sets and lists for the underlying
 *      implementation.  Massive performance improvements
 *
 *      Revision 1.2  2011/05/29 22:11:57  kyledewey
 *      Removed the removeItem() method.
 *      Added the clear() method.
 *
 *      Revision 1.1  2011/05/25 20:04:57  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model

/**
 * A mutable set that retains the order of items added.
 * Supports constant time contains() and addItem().  Note
 * that asSeq is in linear time.
 * Removals are not supported due to implementation difficulty.
 * If they were supported, the order of all removals would have
 * to be recorded as well as the order of insertions
 * to ensure correctness with a stream of arbitrary operations.
 * Generating the resulting list in linear time is difficult, and a naive
 * O(n) method requires SeqSets internally!
 * @param seq The underlying sequence to use
 * @author Kyle Dewey
 */
class SeqSet[ T ]( seq: Seq[ T ] ) {
  import scala.collection.mutable.ArrayBuffer
  import java.util.{ Set => JSet, HashSet }
  // begin instance variables
  private val list = new ArrayBuffer[ T ]()
  private val set = new HashSet[ T ]()
  seq.foreach( addItem( _ ) )
  // end instance variables

  /**
   * Creates a new SeqSet without anything in it
   */
  def this() =
    this( Seq() )

  /**
   * Clears all items from this.
   */
  def clear() {
    set.clear()
    list.clear()
  }

  /**
   * Determines if the given item is already contained within
   * @param item The item to check
   * @return true if it's already contained, else false
   */
  def contains( item: T ) =
    set.contains( item )

  /**
   * Adds the given item to this.
   * This is contant time.
   * If the item is already contained, this is a no-op
   * @param item The item to add
   */
  def addItem( item: T ) {
    if ( set.add( item ) ) {
      list += item
    }
  }

  /**
   * Gets the items as a seq.
   * This is linear time.
   * @return The items as a seq, in the order they were added.
   */
  def asSeq: Seq[ T ] = 
    list.toList
}
