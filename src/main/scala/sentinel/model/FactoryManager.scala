/*
 * FactoryManager.scala
 */

package sentinel.model

/**
 * Manages a bunch of objects to be used like singletons.
 * @param items The initial key, value pairs
 * @author Kyle Dewey
 */
class Manager[ K, V ]( private var items: Map[ K, V ] ) {
  /**
   * Creates a new, empty manager.
   */
  def this() =
    this( Map() )

  /**
   * Gets the value associated with the given key.
   * @param key The key associated with the value
   * @return The value associated with the given key
   * @throws NoSuchElementException if no value is associated
   *         with the given key
   */
  def apply( key: K ) =
    items( key )

  /**
   * Registers a given item.  If an item with this key
   * already exists, then it will be overridden.
   * @param key The key for the item
   * @param item The item itself
   */
  def register( key: K, item: V ) =
    items += key -> item

  /**
   * Gets the given item.  If the item doesn't exist, then
   * it returns None.
   * @param key The key of the item
   * @return The item associated with this key
   */
  def get( key: K ) =
    items.get( key )

  /**
   * Determines if the given item has been registered.
   * @param key The key of the item
   * @return true if it has, else false
   */
  def isRegistered( key: K ) =
    items.contains( key )

  /**
   * Registers a given item, but only if it is not already contained
   * in the database.
   * @param key The key for the item
   * @param value The value for the item
   * @return true if the item was added, else false (it already exists)
   */
  def registerIfNew( key: K, value: V ): Boolean = {
    val retval = !isRegistered( key )
    if ( retval ) register( key, value )
    retval
  }

  /**
   * Gets the key of every item that has been registered.
   * @return A sequence of keys
   */
  def getKeys(): Seq[ K ] =
    items.keys.toList.toSeq
} // Manager

/**
 * Exception thrown when no factory/class with the given
 * name exists.
 * @param message A message to display
 */
case class NoSuchFactoryException( message: String ) 
     extends Exception( message ) {}

/**
 * Like Manager, but it is specialized for factories.
 * @param factories The initial key, factory pairs
 * @author Kyle Dewey
 */
class FactoryManager[ T <: Instance, V <: InstanceFactory[ T ] ]
( factories: Map[ String, V ] ) 
extends Manager[ String, V ]( factories ) {
  /**
   * Creates a new empty factory manager.
   */
  def this() =
    this( Map() )

  /**
   * Gets the factory associated with the given name
   * @param name The name of the factory
   * @return The factory with the given name
   * @throws NoSuchFactoryException If there was no factory with the
   *         given name
   */
  override def apply( name: String ) = {
    try {
      super.apply( name )
    } catch {
      case e: NoSuchElementException => 
	throw new NoSuchFactoryException( e.getMessage )
    }
  }

  /**
   * Creates an instance of a class.
   * Convenience method that is equivalent to:
   * <code>factory( name ).instantiate( params, optimize )</code>
   * @param name The name of the factory
   * @param params Params for the factory to create the object with
   * @param optimize Whether or not to perform optimizations
   * @throws NoSuchFactoryException If the given factory doesn't exist
   * @throws ParameterNameException If one of the parameters
   *         has a name that we don't recognize
   * @throws ParameterTypeException If the expected types and
   *         actual types of the param differ
   * @throws ParameterRequirementException If a neccessary param
   *         is missing
   * @throws ParameterArrayException If our expectation of an array
   *         or not is different from the given param
   * @throws ParameterizedInstantationException If the params checked
   *         out ok but the object could not be initialized.
   */
  def instantiate( name: String,
		   params: Seq[ NamedParam ],
		   optimize: Boolean ): T = 
    apply( name ).instantiate( params, optimize )
  
  /**
   * Gets all factories.  They will be returned in
   * abc order by name.
   * @return all factories, in abc order by name
   */
  def getFactories(): Seq[ V ] =
    getKeys.map( get( _ ).get ).toList.sortWith( _ < _ )

  /**
   * Gets the factory with the given key.
   * @param key The key for the factory
   * @return the factory associated with the key
   */
  def getFactory( key: String ): Option[ V ] = 
    get( key )

  /**
   * Registers the given factory.
   * If a factory with this name already exists,
   * it will be overridden
   * @param key Key for the factory
   * @param desc A description for the factory
   * @param factory The factory itself
   */
  def registerFactory( factory: V ) =
    register( factory.name, factory )
} // FactoryManager

/**
 * Exception thrown when we don't know the type of the given factory.
 * @param message A message to show the user
 * @author Kyle Dewey
 */
case class UnknownFactoryTypeException( message: String )
     extends Exception( message ) {}

/**
 * Dispatcher for matcher/replacer factories.
 * @author Kyle Dewey
 */
object FactoryManager {
  /**
   * Registers the given factory.
   * @param factory The factory to register
   * @throws UnknownFactoryTypeException If we don't know the kind of manager
   * to register it with
   */
  def registerFactory( factory: InstanceFactory[ _ ] ) {
    factory match {
      case matcher: MatcherFactory => 
	MatcherFactoryManager.registerFactory( matcher )
      case replacer: ReplacerFactory => 
	ReplacerFactoryManager.registerFactory( replacer )
      case _ =>
	throw new UnknownFactoryTypeException( "Unknown factory type: " +
					       ParamType.toString( factory.instanceType ) )
    }
  }

  /**
   * Given either "matcher" or "replacer", it will return the appropriate
   * factory manager.
   * @param factoryText The text representing the factory manager
   * @return The factory manager that goes with this
   * @throws UnknownFactoryTypeException If we don't know what the text means
   */
  def getFactoryManager( factoryText: String ) =
    factoryText.toLowerCase match {
      case "matcher" => MatcherFactoryManager
      case "replacer" => ReplacerFactoryManager
      case _ => 
	throw new UnknownFactoryTypeException( "Unknown factory type: " + 
					       factoryText )
    }

  /**
   * Like <code>getFactoryManager</code>, but it takes both the
   * name of the type of manager to get and the actual factory
   * @param factoryType The type of the factory.  Either "matcher" or
   * "replacer"
   * @param name The name of the factory
   * @return The factory, or None if there isn't a factory that goes by
   * this name
   * @throws UnknownFactoryTypeException If the type of the factory isn't
   * recognized
   */
  def getFactory( factoryType: String, name: String ) =
    getFactoryManager( factoryType ).getFactory( name )
}

/**
 * Organizes all matcher factories.
 * @author Kyle Dewey
 */
object MatcherFactoryManager 
extends FactoryManager[ Matcher, MatcherFactory ]() {}

/**
 * Organizes replacer factories.
 * @author Kyle Dewey
 */
object ReplacerFactoryManager
extends FactoryManager[ Replacer, ReplacerFactory ]() {}
