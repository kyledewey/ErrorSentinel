/*
 * ErrorShower.scala
 *
 * Version:
 *     $Id: ErrorShower.scala,v 1.1 2011/05/31 17:20:58 kyledewey Exp $
 *
 * Revisions:
 *      $Log: ErrorShower.scala,v $
 *      Revision 1.1  2011/05/31 17:20:58  kyledewey
 *      Initial revision
 *
 *
 */

package sentinel.utils.interactive

import java.awt._
import javax.swing._

/**
 * Defines a component that can show error messages.
 * Ultimately, this merely saves a bit of typing.
 * @author Kyle Dewey
 */
trait ErrorShower extends Component {
  /**
   * Shows an error to the user.
   * @param message The message to show to the user in the error box
   */
  def showError( message: String ) {
    JOptionPane.showMessageDialog( this, 
                                   message, 
                                   "Error",
                                   JOptionPane.ERROR_MESSAGE )
  }

  /**
   * Shows an error to the user, raised from an exception
   * @param exception The exception
   */
  def showError( exception: Exception ) {
    showError( exception.getMessage )
  }
}
