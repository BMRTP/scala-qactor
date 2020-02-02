package supports.properties

object PropertyImplicits {
  implicit def propertyValue[T](property: Property[T]): T = property.get
}

trait Property[T] {
  def get: T

  def set(value: T): Unit

  def onChange(body: Property[T] => Unit): Property[T]

  override def toString: String = get.toString
}