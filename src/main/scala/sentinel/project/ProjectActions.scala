/*
 * ProjectActions.scala
 */

package sentinel.project

import sentinel.model._

// note that the contents of this file most clearly fit in with
// ProjectHelpers.scala, but due to circular file dependencies that
// would be introduced by putting these contents in that file, they
// have been placed into this file

/**
 * Exception thrown when the given type of project isn't understood.
 * @param message An informative message for the user
 * @author Kyle Dewey
 */
case class UnknownProjectTypeException( message: String )
     extends Exception( message )

/**
 * Used for reading in projects.
 * @author Kyle Dewey
 */
object ProjectReader
extends FileActionMap[ ProjectParser ]( Seq( new XMLValue[ ProjectParser ]( ParseXML ) ) ) {
  /**
   * Reads in a project of the given type.
   * @param fileName The name of the file to parse in
   * @param fileType The type of the file to parse in
   * @param spreadsheetFactory Something that can convert spreadsheets
   * to a given desired format.  Takes a spreadsheet and whether or not to
   * register the spreadsheet as a parameter
   * @param register Whether or not to register created spreadsheets
   * @return The project that was read in
   * @throws UnknownProjectTypeException If the project type isn't known
   * @throws ProjectParseException If a format-level error exists in the
   * project
   * @throws FileNotFoundException If the file could not be opened
   * @throws IOException If an error occurred on read
   */
  def readProject[ T <: Spreadsheet ]( fileName: String, 
				       fileType: String,
				       spreadsheetFactory: ( Spreadsheet, Project[ T ], Boolean ) => T,
				       register: Boolean ) = {
    if ( map.contains( fileType ) ) {
      map( fileType ).value.parseProject( fileName, 
					  spreadsheetFactory,
				          register )
    } else {
      throw new UnknownProjectTypeException( "Unknown input project file " +
					     "type: " + fileType )
    }
  }

  /**
   * Reads in all languages from the given project.
   * Note that this internally parses in the entire project, and doesn't
   * register any of the created sheets.
   * @param fileName The name of the file to parse in
   * @param fileType The type of the file to parse in
   * @throws UnknownProjectTypeException If the project type isn't known
   * @throws ProjectParseException If a format-level error exists in the
   * project
   * @throws FileNotFoundException If the file could not be opened
   * @throws IOException If an error occurred on read
   */
  def readLanguagesInProject( fileName: String, fileType: String ) {
    readProject( fileName,
		 fileType,
		 ( sheet: Spreadsheet, 
		   project: Project[ Spreadsheet ], 
		   register: Boolean ) => sheet,
	         false )
  }
}

/**
 * Used for writing out projects.
 * Note that the "Object" postfix is because the Scala compiler gets confused
 * with where the symbol is; might actually be a compiler level bug.
 * @author Kyle Dewey
 */
object ProjectWriterObject
extends FileActionMap[ ProjectWriter ]( Seq( new XMLValue[ ProjectWriter ]( WriteXML ) ) ) {
  /**
   * Writes out a project of the given type.
   * @param project The project to write out
   * @param fileName The name of the file to write to
   * @param fileType The type of the output file
   * @throws UnknownProjectTypeException If the project type isn't known
   * @throws ProjectWriteException If a format-level project writing exception
   * occurred
   * @throws IOException If an error occurred on write
   */
  def writeProject( project: Project[ _ ],
		    fileName: String,
		    fileType: String ) {
    if ( map.contains( fileType ) ) {
      map( fileType ).value.writeProject( project,
					  fileName )
    } else {
      throw new UnknownProjectTypeException( "Unknown project output file " +
					     "type: " + fileType )
    }
  }
}
