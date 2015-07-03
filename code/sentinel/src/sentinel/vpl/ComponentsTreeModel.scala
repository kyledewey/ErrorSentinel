/*
 * ComponentsTreeModel.scala
 *
 * Version:
 *     $Id: ComponentsTreeModel.scala,v 1.1 2011/04/10 04:06:46 kyledewey Exp $
 *
 * Revisions:
 *      $Log: ComponentsTreeModel.scala,v $
 *      Revision 1.1  2011/04/10 04:06:46  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.vpl

import javax.swing.tree._

/**
 * Tree model used for <code>ComponentsTree</code>.
 * @param rootNode The root of the tree
 * @author Kyle Dewey
 */
class ComponentsTreeModel( val rootNode: DefaultMutableTreeNode )
extends DefaultTreeModel( rootNode ) {
  /**
   * Like <code>insertNodeInto</code>, only it will always insert
   * into the last index.
   * @param child The child node
   * @param parent The parent node
   */
  def insertNodeInto( child: MutableTreeNode,
		      parent: MutableTreeNode ) {
    insertNodeInto( child,
		    parent,
		    parent.getChildCount )
  }

  /**
   * Inserts the given child at the root at the given index.
   * @param node The node to insert
   * @param index The index to insert at
   */
  def insertNodeIntoRoot( node: MutableTreeNode,
			  index: Int ) {
    insertNodeInto( node,
		    rootNode,
		    index )
  }

  /**
   * Like <code>insertNodeIntoRoot</code>, but it inserts at
   * the last index.
   * @param node The node to insert
   */
  def insertNodeIntoRoot( node: MutableTreeNode ) {
    insertNodeInto( node,
		    rootNode )
  }
}

