/*
 * Writer.scala
 */

package sentinel.model.writer

import sentinel.model._

/**
 * Exception thrown when there was an error on writing out a class.
 * @param message A message to show
 * @author Kyle Dewey
 */
case class ClassWriteException( message: String ) 
     extends Exception( message ) {}

/**
 * Contains routines for writing classes to any given format.
 * @param fileName The file to wirte to
 * @author Kyle Dewey
 */
abstract class ClassWriter( val fileName: String ) {
  /**
   * Writes all of the given classes to the file.
   * Note that this will OVERWRITE any classes already existing
   * in the given file, and for that matter the entire contents
   * of the file.
   * @param classes The classes to write out
   * @throws ClassWriteException If it couldn't figure out how to
   *         write out a given class
   * @throws IOException If an error occurred on writing
   */
  def writeClasses( classes: Seq[ InstanceFactory[ _ ] ] )
}
