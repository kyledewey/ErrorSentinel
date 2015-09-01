/*
 * ParseTree.scala
 *
 * Version:
 *     $Id: ParseTree.scala,v 1.7 2011/06/18 03:28:47 kyledewey Exp $
 *
 * Revisions:
 *      $Log: ParseTree.scala,v $
 *      Revision 1.7  2011/06/18 03:28:47  kyledewey
 *      Added support for optimizations.
 *
 *      Revision 1.6  2011/03/27 14:04:37  kyledewey
 *      getChildren().map changed to getChildren().flatMap
 *
 *      Revision 1.5  2010/06/20 22:54:19  kyledewey
 *      Made factory an accessable value in InternalNode.
 *
 *      Revision 1.4  2010/06/20 17:25:38  kyledewey
 *      Moved ParseNode and ParseTreeFactory to Factory.scala.
 *
 *      Revision 1.3  2010/06/18 19:37:06  kyledewey
 *      Made factories take a name and description.
 *
 *      Revision 1.2  2010/06/16 01:00:30  kyledewey
 *      Made it so getValue() in ParseNode returns a
 *      Seq in order to account for arrays.
 *
 *      Revision 1.1  2010/06/15 17:56:41  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model

/*
 * Note that ParseNode is defined in Factory.scala.  This is needed
 * since there is a factory that creates instances via ParseNodes.
 */

/**
 * A terminal node containing a parameter.
 * @param name The name of the node
 * @param item The item to store
 * @author Kyle Dewey
 */
class TerminalNode( name: String, val item: Param ) 
extends ParseNode( name ) {
  private val retval = new NamedParam( name, item )

  /**
   * Gets the value of the terminal node
   * @param params Parameters to the node (ignored)
   * @return the value
   */
  override def getValue( params: Seq[ NamedParam ] ) =
    Seq( retval )
}

/**
 * A node that is to be used as a variable.  This means a
 * variable in the tree, NOT a Variable.  Whereas a Variable
 * refers specifically to a value in the spreadsheet, this variable
 * can be any Param.
 * @param name The name of the node
 * @param mapsTo What parameter this node maps to in the given list of params
 * @author Kyle Dewey
 */
class VariableNode( name: String, val mapsTo: String ) 
extends ParseNode( name ) {
  /**
   * Gets the variable this refers to.
   * @param params The parameters which have been passed to the tree
   * @return The param that we map to, with our name
   */
  override def getValue( params: Seq[ NamedParam ] ) = 
    params.filter( _.name == mapsTo ).map( _.rename( name ) )
}

/**
 * Holds helper routines for InternalNode.
 * @author Kyle Dewey
 */
object InternalNode {
  /**
   * Given a matcher, returns either True or False, based on the matches().
   * @param matcher The matcher
   * @return Either True or False, depending on what the value of matches() is
   */
  def toMatcherConstant( matcher: Matcher ) =
    if ( matcher.matches ) new True()
    else new False()

  /**
   * Converts the given instance to a constant.
   * @param instance The instance to convert
   * @return The instance's value as a constant
   */
  def toConstant( instance: Instance ): Param =
    if ( instance.isInstanceOf[ Matcher ] ) {
      toMatcherConstant( instance.asInstanceOf[ Matcher ] )
    } else if ( instance.isInstanceOf[ Replacer ] ) {
      instance.asInstanceOf[ Replacer ].replace
    } else {
      instance
    }

  /**
   * Performs the purity optimization on the given param, if applicable.
   * This means that the given parameter is a pure instance that takes
   * constants as parameters.  In such a case, the return value will never
   * differ, and so it merely returns the return value.
   * @param param The parameter to attempt this optimization on.
   * @return Either the same parameter if the optimization wasn't possible,
   * or a new parameter that is equivalent if the optimization was possible
   */
  def purityOptimization( param: Param ) = {
    var retval = param
    if ( param.isInstanceOf[ Instance ] ) {
      val instance = param.asInstanceOf[ Instance ]
      if ( instance.isPure &&
	   instance.params.forall( _.param.isConstantType ) ) {
	retval = toConstant( instance )
      }
    }
    retval
  }
}

/**
 * An internal node representing either a matcher or replacer.
 * @param name The name of the node
 * @param factory A factory that can get the given matcher or replacer
 * @author Kyle Dewey
 */
class InternalNode( name: String, val factory: InstanceFactory[ _ ] )
extends ParseNode( name ) {
  /**
   * Gets the instance that comes about through the evaulation of the given
   * parameters.
   * @param params The parameters for children
   * @param optimize true if we should optimize, else false
   * @return The resulting instance.
   */
  def getInstance( params: Seq[ NamedParam ], optimize: Boolean ) = {
    val myParams =
      if ( optimize ) getChildren.flatMap( _.getOptimizedValue( params ) )
      else getChildren.flatMap( _.getValue( params ) )
    factory.instantiate( myParams, optimize ).asInstanceOf[ Param ]
  }
  
  /**
   * Gets the matcher or replacer.
   * All internal parameters to the matcher or replacer are initialized
   * via child nodes
   * @param params The parameters to the tree (merely passed to children)
   * @return The matcher or replacer
   */
  override def getValue( params: Seq[ NamedParam ] ) = 
    toReturnValue( getInstance( params, false ) )

  /**
   * Given the parameter to return, it will create a return value appropriate
   * for both <code>getValue</code> and <code>getOptimizedValue</code>
   * @param param The parameter to return
   * @return <code>Seq( new NamedParam( name, param ) )</code>
   */
  def toReturnValue( param: Param ) =
    Seq( new NamedParam( name, param ) )

  /**
   * Like <code>getValue</code>, only it performs optimizations.
   * Specifically, if the instance is pure, and its parameters are all
   * constants, then this simply returns a constant holding the instance's
   * value.
   * @param params The parameters for the tree to pass to children.
   * @return A parameter for this tree.
   */
  override def getOptimizedValue( params: Seq[ NamedParam ] ) = 
    toReturnValue( InternalNode.purityOptimization( getInstance( params, true ) ) )
}
