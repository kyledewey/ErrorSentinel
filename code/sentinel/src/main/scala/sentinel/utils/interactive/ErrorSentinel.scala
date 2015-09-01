/*
 * ErrorSentinel.scala
 */

package sentinel.utils.interactive

import java.io._
import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.filechooser.{ FileFilter => SFileFilter }
import sentinel.model._
import sentinel.project._
import sentinel.io._
import sentinel.io.output._

/**
 * Holds constants and helper routines for <code>ErrorSentinel</code>
 * @author Kyle Dewey
 */
object ErrorSentinel {
  val HEADER_FRAME_NAME = "Error Sentinel"
  val DEFAULT_INFO_WIDTH = 400
  val FRAME_X_OFFSET = 30
  val FRAME_Y_OFFSET = FRAME_X_OFFSET

  // menu text
  val FILE = "File"
  val SAVE_PROJECT = "Save Project"
  val SAVE_PROJECT_AS = "Save Project As..."
  val EXIT = "Exit"
  val VIEW = "View"
  val SHOW_ERRORS = "Show Cell Errors"
  val SHOW_ERRORS_WITH_PARAMS = "Show Cell Errors with Parameters"
  val SAVE_SHEET_PREFIX = "Save Sheet \""
  val SAVE_SHEET_POSTFIX = "\""
  val SAVE_SHEET_AS_PREFIX = SAVE_SHEET_PREFIX
  val SAVE_SHEET_AS_POSTFIX = SAVE_SHEET_POSTFIX + " as..."
  val SAVE_ALL_SHEETS = "Save All Sheets"

  /**
   * Determines if the given item looks like a save sheet command.
   * @param text The text to check
   * @return True if it looks like a save sheet command
   */
  def looksLikeSaveSheet( text: String ) =
    SentinelTable.hasPrefixPostfix( text,
				    SAVE_SHEET_PREFIX, 
				    SAVE_SHEET_POSTFIX )

  /**
   * Determines if the given item looks like a save sheet as command.
   * @param text The text to check
   * @return True if it looks like a save sheet as command
   */
  def looksLikeSaveAsSheet( text: String ) =
    SentinelTable.hasPrefixPostfix( text,
				    SAVE_SHEET_AS_PREFIX,
				    SAVE_SHEET_AS_POSTFIX )

  /**
   * Extracts the text from a save sheet command.
   * @param text The text to extract from
   * @return The inner text, or None if it's not a save sheet command
   */
  def extractSaveSheet( text: String ) =
    SentinelTable.extractInner( text, 
			        SAVE_SHEET_PREFIX, 
			        SAVE_SHEET_POSTFIX )
  
  /**
   * Extracts the text from a save sheet as command.
   * @param text The text to extract from
   * @return The inner text, or None if it's not a save sheet as command
   */
  def extractSaveAsSheet( text: String ) =
    SentinelTable.extractInner( text,
			        SAVE_SHEET_AS_PREFIX,
			        SAVE_SHEET_AS_POSTFIX )

  /**
   * Makes a file chooser that will open in the given directory and
   * will use the given file filters.  Note that the accept all file
   * filter will be disabled.
   * @param directory The directory path
   * @param filters The file filters to use
   * @return A file chooser that will open to the given directory and
   * will use the given filters
   */
  def makeFileChooser( directory: String, 
		       filters: Seq[ SFileFilter ] ): JFileChooser = {
    val retval = new JFileChooser( directory )
    retval.setAcceptAllFileFilterUsed( false )
    filters.foreach( retval.addChoosableFileFilter( _ ) )
    retval
  }

  /**
   * Like <code>makeFileChooser</code>, but it opens to the current directory.
   * @param filters The file filters to use
   * @return A file chooser that will open to the current directory that
   * will use the given file filters
   */
  def makeFileChooser( filters: Seq[ SFileFilter ] ): JFileChooser =
    makeFileChooser( ".", filters )
}

import ErrorSentinel._

/**
 * Holds the desktop pane.
 * @param project The project to use
 * @param projectFile The file that the project came from
 * @author Kyle Dewey
 */
class ErrorSentinel( val project: Project[ WithReplacementSpreadsheet[ WithReplacementCellContents ] ], val projectFile: File ) 
extends JFrame( HEADER_FRAME_NAME ) with ErrorShower with ActionListener with FocusListener {
  // begin instance variables
  val actionsMap: Map[ String, () => Unit ] = 
    Map( SHOW_ERRORS -> showErrorsAction,
	 SHOW_ERRORS_WITH_PARAMS -> showErrorsWithParamsAction,
	 SAVE_PROJECT -> saveProjectAction,
         SAVE_PROJECT_AS -> saveProjectAsAction,
	 SAVE_ALL_SHEETS -> saveAllSheetsAction,
         EXIT -> exitAction )
  val desktop = new JDesktopPane()
  private var focusedComponent: Option[ SentinelTable ] = None
  private var numFrames = 0 // the number of frames that have been created
  val saveSheetMenu = menuItem( "" )
  val saveSheetAsMenu = menuItem( "" )
  // end instance variables

  // begin constructor
  setupSaveItems()
  setJMenuBar( makeMenuBar )
  setContentPane( desktop )
  addFrameToDesktop( SentinelInformationPanel )
  // pack isn't sufficient for determining width
  SentinelInformationPanel.setSize( DEFAULT_INFO_WIDTH, 
				    SentinelInformationPanel.getHeight )
  project.foreachSpreadsheet( addSheetToDesktop( _ ) )
  // end constructor

  /**
   * If the focused component is defined, the given function will be called.
   * @param function The function to call if the focused component is defined
   * @return The result of the function, or None if it isn't defined
   */
  def callIfFocused[ T ]( function: SentinelTable => T ) = 
    if ( focusedComponent.isDefined ) {
      Some( function( focusedComponent.get ) )
    } else {
      None
    }

  /**
   * Returns the name of the focused sheet with the given prefix and
   * postfix.  If there isn't a focused sheet, it simply appends the two.
   * @param prefix The prefix to use
   * @param postfix The postfix to use
   * @return the prefix appended onto the sheet name, or a null string,
   * followed by the postfix
   */
  def prefixNamePostfix( prefix: String, postfix: String ) =
    prefix + callIfFocused( _.name ).getOrElse( "" ) + postfix

  /**
   * Gets the text that the save sheet menu item should currently show.
   * @return The text that the save sheet menu item should currently show
   */
  def saveSheetText() = 
    prefixNamePostfix( SAVE_SHEET_PREFIX, 
		       SAVE_SHEET_POSTFIX )

  /**
   * Gets the text that should be shown by the save as menu item
   * @return The text that should be shown by the save as menu item
   */
  def saveSheetAsText() =
    prefixNamePostfix( SAVE_SHEET_AS_PREFIX,
		       SAVE_SHEET_AS_POSTFIX )

  /**
   * Toggles whether or not the save menu items are enabled.
   * @param enabled Whether or not they should be enabled.
   */
  def setSaveMenuItemsEnabled( enabled: Boolean ) {
    saveSheetMenu.setEnabled( enabled )
    saveSheetAsMenu.setEnabled( enabled )
  }

  /**
   * Sets up the save menu items based on whether or not there
   * is currently a sheet focused.
   */
  def setupSaveItems() {
    saveSheetMenu.setText( saveSheetText )
    saveSheetAsMenu.setText( saveSheetAsText )
    setSaveMenuItemsEnabled( focusedComponent.isDefined )
  }

  /**
   * Creates a sentinel table frame from the given spreadsheet.
   * Adds this as a focus listener.
   * @param sheet The spreadsheet
   * @return A frame holding the spreadsheet
   */
  def sheetToFrame( sheet: WithReplacementSpreadsheet[ WithReplacementCellContents ] ) = {
    val retval = new SentinelTableFrame( new SentinelTable( sheet ) )
    retval.table.addFocusListener( this )
    retval
  }

  /**
   * Processes a gain in focus
   * @param event The event corresponding to the change in focus.
   */
  def focusGained( event: FocusEvent ) {
    focusedComponent =
      if ( event.getComponent.isInstanceOf[ SentinelTable ] ) {
	Some( event.getComponent.asInstanceOf[ SentinelTable ] )
      } else {
	None
      }
    setupSaveItems()
  }

  /**
   * Processes a loss in focus.
   * Note that since there is a gain for every loss, this does nothing.
   * @param event The event corresponding to the change in focus
   */
  def focusLost( event: FocusEvent ) {}

  /**
   * Makes a menu item with the given text that holds <code>this</code>
   * as an <code>ActionListener</code>.
   * @param text The text of the menu item
   * @return The menu item with the given text holding <code>this</code>
   * as an <code>ActionListener</code>
   */
  def menuItem( text: String ) =
    SentinelTable.menuItem( text, this )

  /**
   * Prepares the given spreadsheet for use with this.
   * This will prep the instanceResults and add it to
   * the desktop.
   * @param sheet The spreadsheet
   */
  def addSheetToDesktop( sheet: WithReplacementSpreadsheet[ WithReplacementCellContents ] ) {
    sheet.resetGraph()
    sheet.tryValueAtAllCells()
    addFrameToDesktop( sheetToFrame( sheet ) )
  }

  /**
   * Adds the given internal frame to this desktop.
   * It will take care of packing the frame and making
   * it visible.
   * @param frame The frame to add
   */
  def addFrameToDesktop( frame: JInternalFrame ) {
    frame.setLocation( FRAME_X_OFFSET * numFrames,
		       FRAME_Y_OFFSET * numFrames )
    desktop.add( frame )
    numFrames += 1
    frame.pack()
    frame.setVisible( true )
  }

  /**
   * Makes the file menu.
   * @return The file menu
   */
  protected def makeFileMenu() = {
    val retval = new JMenu( FILE )
    retval.add( menuItem( SAVE_PROJECT ) )
    retval.add( menuItem( SAVE_PROJECT_AS ) )
    retval.addSeparator()
    retval.add( saveSheetMenu )
    retval.add( saveSheetAsMenu )
    retval.addSeparator()
    retval.add( menuItem( SAVE_ALL_SHEETS ) )
    retval.addSeparator()
    retval.add( menuItem( EXIT ) )
    retval
  }

  /**
   * Makes the view menu
   * @return The view menu
   */
  protected def makeViewMenu() = {
    val retval = new JMenu( VIEW )
    retval.add( menuItem( SHOW_ERRORS ) )
    retval.add( menuItem( SHOW_ERRORS_WITH_PARAMS ) )
    retval
  }
  
  /**
   * Makes the menu bar.
   * @return The menu bar to use
   */
  protected def makeMenuBar() = {
    val menuBar = new JMenuBar()
    menuBar.add( makeFileMenu )
    menuBar.add( makeViewMenu )
    menuBar
  }

  /**
   * Shows all error colors based on contained instances for
   * all sheets.
   */
  def showErrorsAction() {
    SentinelTable.showAllCellColors()
  }

  /**
   * Shows all error colors including parameters.
   * Note that this means a single cell can have multiple colors
   * assigned.  If this is the case, then the "worst" color is
   * shown ( green < yellow < red ).
   */
  def showErrorsWithParamsAction() {
    SentinelTable.showAllCellColorsWithParams()
  }

  /**
   * Writes this project to the given file.
   * If an error occurrs on write, it will display it to the user
   * @param file The file to write to
   */
  def writeProject( file: File ) {
    try {
      WriteXML.writeProject( project, file )
    } catch {
      case e: ProjectWriteException => showError( e )
      case e: IOException => showError( e )
    }
  }

  /**
   * Saves the current project to its original file.
   */
  def saveProjectAction() {
    writeProject( projectFile )
  }

  /**
   * Attempts to write a spreadsheet out to a file, using the given file
   * chooser as a guide.  If this isn't possible for whatever reason, an
   * error is displayed.
   * @param sheet The sheet to write out
   * @param file The file to write to
   * @param fileFilter The file filter selected by the user
   */
  def writeSheet( sheet: Spreadsheet,
		  file: File,
		  fileFilter: SFileFilter ) {
    if ( fileFilter.isInstanceOf[ FileActionMapValue[ _ ] ] ) {
      writeSheet( sheet,
		  fileFilter.asInstanceOf[ FileActionMapValue[ _ ] ]
		            .toSheet( sheet.name,
				      file.getPath ) )
    } else {
      unrecognizedFileFilterError( fileFilter )
    }
  }

  /**
   * Saves the current project to an arbitrary file.
   */
  def saveProjectAsAction() {
    val fc = makeFileChooser( Seq( ProjectFileChooser ) )
    if ( fc.showSaveDialog( this ) == JFileChooser.APPROVE_OPTION ) {
      writeProject( fc.getSelectedFile )
    }
  }
  
  /**
   * Exits the program.
   */
  def exitAction() {
    System.exit( 0 )
  }

  /**
   * Shows the user an error message saying that the given file filter
   * isn't recognized.
   * @param filter The file filter.
   */
  def unrecognizedFileFilterError( filter: SFileFilter ) {
    showError( "Unrecognized file filter: " + filter.toString ) 
  }

  /**
   * Shows the user an error message saying that the given sheet doesn't exist.
   * @param name The name of the sheet
   */
  def noSuchSheetError( name: String ) {
    showError( "Spreadsheet with the name \"" + name + "\" doesn't exist." )
  }

  /**
   * Calls the given function with the given sheet.
   * If the sheet doesn't exist, it will show the user an error message.
   * @param name The name of the sheet
   * @param function The function to call with the sheet as a parameter
   */
  def callWithSheet( name: String, function: SentinelTable => Unit ) {
    if ( SentinelTable.callIfSheet( name, function ).isEmpty ) {
      noSuchSheetError( name )
    }
  }

  /**
   * Writes out the given sheet.  Shows the user any errors that occurred
   * while it was written out.
   * @param spreadsheet The sheet to write out
   * @param describer Describes how to perform the write
   */
  def writeSheet( spreadsheet: Spreadsheet, sheet: Sheet ) {
    try {
      SheetWriter.writeSpreadsheet( spreadsheet, sheet )
    } catch {
      case e: UnknownSheetTypeException => showError( e )
      case e: SpreadsheetWriteException => showError( e )
      case e: IOException => showError( e )
    }
  }

  /**
   * Saves the sheet with the given name back to its original file.
   * If the given sheet doesn't exist, it will show the user an error
   * message.
   * @param sheetName The name of the sheet to write out
   */
  def saveSheetAction( sheetName: String ) {
    callWithSheet( sheetName,
		   ( sheet: SentinelTable ) =>
		     writeSheet( sheet.model,
				 project.sheets( sheetName ) ) )
  }

  /**
   * Saves the sheet with the given name to an user-selected arbitrary file.
   * @param sheetName The name of the sheet to write out
   */
  def saveSheetAsAction( sheetName: String ) {
    val fc = makeFileChooser( SheetWriter.filters )
    if ( fc.showSaveDialog( this ) == JFileChooser.APPROVE_OPTION ) {
      callWithSheet( sheetName,
		     ( sheet: SentinelTable ) =>
		       writeSheet( sheet.model, 
				   fc.getSelectedFile,
				   fc.getFileFilter ) )
    }
  }

  /**
   * Called when the user wants to save all sheets in the project.
   */
  def saveAllSheetsAction() {
    project.foreachSpreadsheet( sheet => 
      writeSheet( sheet, project.sheets( sheet.name ) ) )
  }

  /**
   * Called when a menu item is clicked.
   * @param event The event correlating to the click
   */
  def actionPerformed( event: ActionEvent ) {
    val command = event.getActionCommand
    if ( actionsMap.contains( command ) ) {
      actionsMap( command )()
    } else if ( looksLikeSaveSheet( command ) ) {
      saveSheetAction( extractSaveSheet( command ).get )
    } else if ( looksLikeSaveAsSheet( command ) ) {
      saveSheetAsAction( extractSaveAsSheet( command ).get )
    }
  }
}

