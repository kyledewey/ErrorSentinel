/*
 * Graph.scala
 *
 * Version:
 *     $Id: Graph.scala,v 1.3 2011/06/04 05:15:38 kyledewey Exp $
 *
 * Revisions:
 *      $Log: Graph.scala,v $
 *      Revision 1.3  2011/06/04 05:15:38  kyledewey
 *      Made graph an abstract class with an adjacency
 *      matrix and adjacency list representation.
 *      Changed DependencyGraph to use the list representation.
 *
 *      Revision 1.2  2010/07/11 05:49:13  kyledewey
 *      Fixed bug in DFSVisit that could cause infinite
 *      recursion under certain circumstances.
 *
 *      Revision 1.1  2010/07/10 21:46:28  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.model.parser

import sentinel.model._

/*
 * Describes a basic graph with a fixed number of vertices.
 * Vertices are identified by ids 0 .. numVertices - 1.
 * Provides a basic implementation of node data based on sequences.
 * @param numNodes The number of vertices in the graph
 * @author Kyle Dewey
 */
abstract class Graph[ N, E ]( val numNodes: Int ) {
  // begin constructor
  private val nodeData = new Array[ Option[ N ] ]( numNodes )
  0.until( numNodes ).foreach( nodeData( _ ) = None )
  // end constructor

  /**
   * Sets the data for the given node.
   * Overwrites anything already there.
   * @param node The id of the node
   * @param data The data to put in for the node
   */
  def setNodeData( node: Int, data: N ) {
    nodeData( node ) = Some( data )
  }

  /**
   * Deletes the node data for a given node
   * @param node The id of the node
   */
  def deleteNodeData( node: Int ) {
    nodeData( node ) = None
  }

  /**
   * Gets the data for a given node
   * @param node The id of the node to get data for
   * @return The data for the node, or None if there was no data
   */
  def getNodeData( node: Int ) =
    nodeData( node )

  /**
   * Determines if the given edge exists.
   * @param from The starting node
   * @param to The ending edge.
   * @return true if it exists, else false
   */
  def isEdge( from: Int, to: Int ): Boolean

  /**
   * Adds the given edge to the graph
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def addEdge( from: Int, to: Int ): Unit

  /**
   * Deletes edge data for a given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdgeData( from: Int, to: Int ): Unit 

  /**
   * Removes an edge from the graph.
   * Note that this also deletes any edge data associated
   * with the edge.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdge( from: Int, to: Int ): Unit

  /**
   * Gets edge data for the given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @return The data for the edge, or None if there is none
   */
  def getEdgeData( from: Int, to: Int ): Option[ E ]

  /**
   * Sets edge data for the given edge.
   * If this isn't an edge, this won't do anything.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @param data The data to put in for the edge
   */
  def setEdgeData( from: Int, to: Int, data: E ): Unit
  
  /**
   * Gets all nodes that are adjacent to the given node.  This
   * is the list of nodes are connected to this node by a single
   * edge.
   * @param node The given node
   * @return All nodes that are adjacent to this node
   */
  def adjacentNodes( node: Int ): Seq[ Int ]
}

/**
 * Graph implementation based on an adjacency matrix
 * @param numNodes The maximum number of nodes allowed in the graph
 * @author Kyle Dewey
 */
class MatrixGraph[ N, E ]( numNodes: Int ) extends Graph[ N, E ]( numNodes ) {
  // begin instance variables
  // determines if a given node is connected to another given node
  private val edges = 
    SentinelHelpers.makeMatrix[ Option[ Option[ E ] ] ]( numNodes,
							 ( row, column ) => None )
  // end instance variables

   /**
   * Determines if the given edge exists.
   * @param from The starting node
   * @param to The ending edge.
   * @return true if it exists, else false
   */
  def isEdge( from: Int, to: Int ) =
    edges( from )( to ).isDefined

  /**
   * Adds the given edge to the graph
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def addEdge( from: Int, to: Int ) {
    if ( !isEdge( from, to ) ) {
      edges( from )( to ) = Some( None )
    }
  }

  /**
   * Deletes edge data for a given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdgeData( from: Int, to: Int ) {
    if ( isEdge( from, to ) ) {
      edges( from )( to ) = Some( None )
    }
  }

  /**
   * Removes an edge from the graph.
   * Note that this also deletes any edge data associated
   * with the edge.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdge( from: Int, to: Int ) {
    edges( from )( to ) = None
  }

  /**
   * Gets edge data for the given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @return The data for the edge, or None if there is none
   */
  def getEdgeData( from: Int, to: Int ) =
    if ( isEdge( from, to ) ) {
      edges( from )( to ).get
    } else {
      None
    }

  /**
   * Sets edge data for the given edge.
   * If this isn't an edge, this won't do anything.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @param data The data to put in for the edge
   */
  def setEdgeData( from: Int, to: Int, data: E ) {
    if ( isEdge( from, to ) ) {
      edges( from )( to ) = Some( Some( data ) )
    }
  }
  
  /**
   * Gets all nodes that are adjacent to the given node.  This
   * is the list of nodes are connected to this node by a single
   * edge.
   * @param node The given node
   * @return All nodes that are adjacent to this node
   */
  def adjacentNodes( node: Int ) =
    0.until( numNodes ).filter( isEdge( node, _ ) )

  /**
   * Gets all nodes that the given node is incident to.
   * @param node The incident node
   * @return All nodes that this node is incident to
   */
  def incidentNodes( node: Int ) =
    0.until( numNodes ).filter( isEdge( _, node ) )
}

/**
 * A graph represented as an adjacency list.
 * For sparse graphs with large numbers of nodes, this can save
 * an enourmous amount of memory.
 * @param numNodes The number of nodes in the graph
 * @author Kyle Dewey
 */
class ListGraph[ N, E ]( numNodes: Int ) extends Graph[ N, E ]( numNodes ) {
  // begin instance variables
  private val edges = new Array[ java.util.AbstractMap[ Int, Option[ E ] ] ]( numNodes )
  0.until( numNodes ).foreach( edges( _ ) = new java.util.HashMap() )
  // end instance variables

  /**
   * Determines if the given edge exists.
   * @param from The starting node
   * @param to The ending edge.
   * @return true if it exists, else false
   */
  def isEdge( from: Int, to: Int ) =
    edges( from ).containsKey( to )

  /**
   * Adds the given edge to the graph
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def addEdge( from: Int, to: Int ) {
    if ( !isEdge( from, to ) ) {
      edges( from ).put( to, None )
    }
  }

  /**
   * Deletes edge data for a given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdgeData( from: Int, to: Int ) {
    if ( isEdge( from, to ) ) {
      edges( from ).put( to, None )
    }
  }

  /**
   * Removes an edge from the graph.
   * Note that this also deletes any edge data associated
   * with the edge.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   */
  def deleteEdge( from: Int, to: Int ) {
    edges( from ).remove( to )
  }

  /**
   * Gets edge data for the given edge
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @return The data for the edge, or None if there is none
   */
  def getEdgeData( from: Int, to: Int ) = 
    if ( isEdge( from, to ) ) {
      edges( from ).get( to )
    } else {
      None
    }

  /**
   * Sets edge data for the given edge.
   * If this isn't an edge, this won't do anything.
   * @param from The starting node of the edge
   * @param to The ending node of the edge
   * @param data The data to put in for the edge
   */
  def setEdgeData( from: Int, to: Int, data: E ) {
    if ( isEdge( from, to ) ) {
      edges( from ).put( to, Some( data ) )
    }
  }
  
  /**
   * Gets all nodes that are adjacent to the given node.  This
   * is the list of nodes are connected to this node by a single
   * edge.  Note that these are returned in no particular order.
   * @param node The given node
   * @return All nodes that are adjacent to this node
   */
  def adjacentNodes( node: Int ) = {
    import scala.collection.JavaConverters._
    edges( node ).keySet.asScala.toSeq
  }
}

/**
 * A container for useful information for a dependency graph.
 * It is intended that each node will have one of these containers
 * @param info Some information about this node
 * @param start When the node was first discovered in a DFS
 * @param finish When the node was finished in a DFS
 * @author Kyle Dewey
 */
case class DependencyNode[ T ]( val info: T,
			        var start: Int,
			        var finish: Int ) {
  /**
   * Merely sets the start and finish to be 0.
   * @param info Some information about this node
   */
  def this( info: T ) =
    this( info, 0, 0 )
}

/**
 * Represents a dependency graph.
 * @param numNodes The number of nodes in the graph
 * @author Kyle Dewey
 */
class DependencyGraph[ N, E ]( numNodes: Int )
extends ListGraph[ DependencyNode[ N ], E ]( numNodes ) {
  // represents colors, which is useful for DFS
  object Color extends Enumeration {
    type Color = Value
    val White,
        Black,
        Gray = Value
  }
  import Color._
  private var colors: Array[ Color ] = new Array( numNodes )
  ( 0 to numNodes - 1 ).foreach( colors( _ ) = White )
  private var time = 0 // used for DFS

  /**
   * Visits a given node in DFS.
   * @param node The id of the node that is being visited
   * @pre All DependencyNode objects have been created and set
   */
  def DFSVisit( node: Int ) {
    colors( node ) = Gray
    time += 1
    getNodeData( node ).get.start = time
    adjacentNodes( node ).foreach( otherNode => {
      if ( colors( otherNode ) == White ) {
	DFSVisit( otherNode )
      } 
    } )
    colors( node ) = Black
    time += 1
    getNodeData( node ).get.finish = time
  }

  /**
   * Performs a DFS.
   * @pre All DependencyNode objects have been created and set
   */
  def DFS() {
    ( 0 to colors.length - 1 ).foreach( colors( _ ) = White )
    time = 0
    ( 0 to numNodes - 1 ).foreach( node => {
      if ( colors( node ) == White ) {
	DFSVisit( node )
      }
    } )
  }

  /**
   * Performs a topological sort.
   * @pre All DependencyNode objects have been created and set
   * @return The nodes, in the order that they must be processed to
   * avoid dependency errors.
   */
  def topologicalSort(): Seq[ DependencyNode[ N ] ] = {
    DFS()
    ( 0 to numNodes - 1 ).map( getNodeData( _ ).get )
                         .toList
                         .sortWith( _.finish > _.finish )
  }
}
