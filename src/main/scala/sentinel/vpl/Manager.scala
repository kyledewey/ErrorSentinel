/*
 * Manager.scala
 */

package sentinel.vpl

import javax.swing._

/**
 * Exception thrown when an attempt is made to use an item that
 * doesn't exist
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class NoSuchItemException( message: String ) 
     extends Exception( message ) {}

/**
 * Represents an item that can be managed.
 * @author Kyle Dewey
 */
trait Manageable {
  /** 
   * Gets the name of the item.
   * Note that the name is supposed to be unique.
   * @return The name of the item
   */
  def name(): String

  /**
   * Gets the hash code of the name
   * @return <code>name.hashCode</code>
   */
  override def hashCode() =
    name.hashCode

  /**
   * Determines if this item is equal to another.
   * This is based on the name.
   * @param other The other item to compare to
   * @return <code>name.equals( other.name )</code>
   */
  override def equals( other: Any ) = 
    if ( !other.isInstanceOf[ Manageable ] ) {
      false
    } else {
      name.equals( other.asInstanceOf[ Manageable ].name )
    }

  /**
   * Gets this item as a string.
   * Merely returns the name.
   * @return The name of this item
   */
  override def toString() =
    name
}

/**
 * A named manageable object
 * @param name The name to use
 * @author Kyle Dewey
 */
class NamedManageable( val name: String ) extends Manageable {
  import javax.swing.tree._

  /**
   * Merely calls <code>node.getUserObject.toString</code> for the name
   * @param node The node to use
   */
  def this( node: javax.swing.tree.DefaultMutableTreeNode ) =
    this( node.getUserObject.toString )
}

/**
 * Manages manageable items.
 * Order in which items were added is significant.
 * @author Kyle Dewey
 */
class Manager[ T <: Manageable ] {
  import scala.collection.mutable._

  // begin instance variables
  private val buffer = new ArrayBuffer[ T ]()
  private var map: Map[ String, T ] = Map()
  private var default: Option[ T ] = None
  // end instance variables

  /**
   * Gets the number of items there are in this manager
   * @return The number of items in this manager
   */
  def numItems() =
    buffer.size

  /**
   * Gets all items, in the order they were laded in
   * @return All items in the order they were loaded in
   */
  def items() =
    buffer.toSeq

  /**
   * Adds an item.  Note that this will overwrite any other item with
   * the same name.
   * @param item The item to add
   */
  def addItem( item: T ) {
    buffer += item
    map += (item.name -> item)
    if ( default.isEmpty ) {
      setDefault( item )
    }
  }

  /**
   * Validates that the given item exists
   * @param item The item to validate
   * @throws NoSuchItemException If there isn't such an item
   */
  def validateItem( item: T ) {
    if ( !map.contains( item.name ) ) {
      throw new NoSuchItemException( "Item with the name " +
				     item.name +
				     " isn't recognized." )
    }
  }

  /**
   * Sets the default item to use
   * @param item The default item to use
   * @throws NoSuchItemException If there isn't such an item
   */
  def setDefault( item: T ) {
    validateItem( item )
    default = Some( item )
  }

  /**
   * Gets the default item to use
   * @return The default item.  Returns None if there are no items
   */
  def getDefault() =
    default

  /**
   * Gets a combo box representation of the items contained within.
   * Items are in the same order as they are in this, and the
   * default item will be selected (if there is one)
   * @return The items within as a combo box
   */
  def toComboBox() = {
    val retval = new JComboBox( items.map( _.asInstanceOf[ Object ] )
			             .toArray )
    if ( default.isDefined ) {
      retval.setSelectedItem( default.get )
    }
    retval
  }
}
