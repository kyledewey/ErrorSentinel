/*
 * AssociationGraph.scala
 */

package sentinel.utils.interactive

import sentinel.model._
import sentinel.model.parser.{ Graph, ListGraph }

/**
 * Holds helper routines for AssociationGraph.
 * @author Kyle Dewey
 */
object AssociationGraph {
  /**
   * Gets all cell ranges that are associated with the given named parameters.
   * @param namedParams The named parameters
   * @return All associated cell ranges (assumed to be spreadsheet variables)
   */
  def cellPointers( namedParams: Seq[ NamedParam ] ): Set[ CellPointer ] =
    Set() ++ namedParams.filter( _.param.isInstanceOf[ CellRange ] )
                        .map( _.param.asInstanceOf[ CellRange ].cellPointer )
  
  /**
   * Gets all the associated cell ranges for the given instance.
   * @param instance The instance to get associated cell ranges from
   * @return All associated cell ranges.
   */
  def cellPointers( instance: Instance ): Set[ CellPointer ] =
    cellPointers( instance.params )

  /**
   * Converts the given cell pointers to row, column pairs.
   * Be advised that the spreadsheet name information is lost in the conversion.
   * @param pointers The cell pointers
   * @return Pairs holding the same rows and columns as the cell pointers
   */
  def pointersToPairs( pointers: Seq[ CellPointer ] ) =
    Set() ++ pointers.map( pointer => 
      (pointer.row, pointer.column) )
}

/**
 * A graph that shows associations between cells in a replacer spreadsheet.
 * @param sheet The replacer spreadsheet used to make the graph of
 * associations.
 * @author Kyle Dewey
 */
trait AssociationGraph extends ReplacerSpreadsheet {
  import javax.swing.event._
  import AssociationGraph._

  // begin instance variables
  protected var graph: Graph[ Any, Boolean ] = null
  // end instance variables

  // begin constructor
  resetGraph()
  // end constructor

  /**
   * Resets the graph.
   */
  def resetGraph() {
    graph = makeGraph
  }

  /**
   * Gets all cells that are associated with the given cell.
   * Note that this is nondirectional.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return The related cells as row, column pairs
   */
  def relatedCells( row: Int, column: Int ) =
    Set() ++ adjacentNodes( row, column ).map( rowColumn( _ ) )

  /**
   * Determines if the given edge exists when the graph is represented
   * as a bidirectional graph.  Since the graph is represented without
   * direction, this is not simply <code>graph.isEdge</code>
   * @param source The source vertex id
   * @param dest The destination vertex id
   * @return True if it's an edge, else false
   */
  protected def isEdge( source: Int, dest: Int ) = 
    graph.getEdgeData( source, dest ).isDefined

  /**
   * Filters nodes that are adjacent to the given vertex
   * @param id the vertex id
   * @param predicate The predicate function to filter with. Takes a
   * vertex id.
   * @return The matching adjacent vertex ids
   */
  protected def filterAdjacentNodes( id: Int, predicate: Int => Boolean ) = 
    adjacentNodes( id ).filter( predicate( _ ) )
  
  /**
   * Like <code>filterAdjacentNodes</code>, but it returns the
   * results as row/column pairs.
   * @param id The vertex id
   * @param pred Predicate to filter vertex ids with
   * @return The matching row/column pairs
   */
  protected def filterAdjacentNodesToPairs( id: Int, pred: Int => Boolean ) =
    idsToPairs( filterAdjacentNodes( id, pred ) )

  /**
   * Determines all the cells that are parameters of the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return The cells that are parameters of the given cell
   */
  def parameterCells( row: Int, column: Int ) = {
    val cellId = vertexId( row, column )
    filterAdjacentNodesToPairs( cellId, isEdge( _, cellId ) )
  }
  
  /**
   * Gets all cells that are used downstream of this cell.
   * In other words, all cells that take this cell as a parameter.
   * @param row The row of this cell
   * @param column The column of this cell
   * @return The cells that use this cell as a parameter
   */
  def usedAsParameterCells( row: Int, column: Int ) = {
    val cellId = vertexId( row, column )
    filterAdjacentNodesToPairs( cellId, isEdge( cellId, _ ) )
  }

  /**
   * Gets the adjacent nodes of the given vertex id
   * @param id The vertex id
   * @return the adjacent nodes
   */
  protected def adjacentNodes( id: Int ): Seq[ Int ] =
    graph.adjacentNodes( id )

  /**
   * Gets the adjacent nodes of the vertex stipulated by the given
   * row and column
   * @param row The row of the cell
   * @param column The column of the cell
   * @return vertex ids of adjacent nodes
   */
  protected def adjacentNodes( row: Int, column: Int ): Seq[ Int ] =
    adjacentNodes( vertexId( row, column ) )

  /**
   * Gets all cell pointers which are related to the given cell.
   * This means every good data matcher or error correction
   * rule that is registered on the cell.
   * @param row The row of the instance
   * @param column The column of the instance
   */
  protected def determineRelatedCells( row: Int, column: Int ) = {
    updateCurrent( row, column )
    Set() ++ getInstances( row, column )
             .flatMap( ( instance: Instance ) =>
	       pointersToPairs( cellPointers( instance ).toSeq ) )
  }

  /**
   * Gets all cells that are associated with the given cell.
   * Note that this includes the given cell.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return Associated cell pointers
   */
  protected def determineAllAssociatedCells( row: Int, column: Int ) = 
    determineRelatedCells( row, column ) + (row -> column)

  /**
   * Makes an association graph.
   * Each cell is a vertex, and each edge represents a dependency of
   * the given cell on another cell.
   * @return An association graph.
   * vertices or edges.
   */
  protected def makeGraph() = {
    val retval = new ListGraph[ Any, Boolean ]( getRowCount * getColumnCount )

    // sets that the cell stipulated by the given row and column is
    // dependent on the cell stipulated by the given pointer
    def dependentOn( row: Int, column: Int, pointer: (Int, Int) ) {
      val vertex1 = vertexId( row, column )
      val vertex2 = vertexId( pointer._1, pointer._2 )
      retval.addEdge( vertex1, vertex2 )
      retval.addEdge( vertex2, vertex1 )
      retval.setEdgeData( vertex2, vertex1, true )
    }

    foreachRowColumn( ( row, column ) => 
      determineAllAssociatedCells( row, column ).foreach( pointer =>
	dependentOn( row, column, pointer ) ) )
    retval
  }

  /**
   * Given the row and column of a cell, it returns an id mapping it
   * to a vertex.
   * @param row The row of the cell
   * @param column The column of the cell
   * @return A vertex id
   */
  protected def vertexId( row: Int, column: Int ) =
    getColumnCount * row + column

  /**
   * Given a vertex id, gets the row and column corresponding to it.
   * @param id The vertex id
   * @return A pair holding the row and column corresponding to the
   * given id
   */
  protected def rowColumn( id: Int ) = {
    val numCols = getColumnCount
    (id / numCols, id % numCols)
  }

  /**
   * Maps the given vertex ids to row/column pairs.
   * @param ids The vertex ids
   * @return Row/column pairs
   */
  protected def idsToPairs( ids: Seq[ Int ] ) =
    ids.map( rowColumn( _ ) )
}
