package supports.properties

trait ObservableProperties[B] {
  def observableProperty[T](init: T, serialize: T => B, deserialize: B => T)(implicit name: sourcecode.Name): Property[T] =
    observableProperty(name.value, init)(serialize, deserialize)

  def observableProperty[T](init: T)(implicit name: sourcecode.Name, serialize: T => B, deserialize: B => T): Property[T] =
    observableProperty(name.value, init)(serialize, deserialize)

  def observableProperty[T](name: String, init: T)(implicit serialize: T => B, deserialize: B => T): Property[T]
}
