package supports.resource

abstract class ResourceSupport(val resourceName: String) {
  def createProperty(name: String, init: String): Boolean

  def setProperty(name: String, value: String): Boolean

  def getProperty(name: String): String
}
