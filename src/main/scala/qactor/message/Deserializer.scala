package qactor.message

import java.lang.reflect.Modifier

import org.reflections.Reflections
import qactor.ExternalQActor
import qactor.context.Context

import scala.language.postfixOps
import scala.util.Try

object Deserializer {

  private var classes: Seq[Class[_]] = Seq()

  def registerAll(packageName: String): Unit = {
    val reflections = new Reflections(packageName)
    val allMessageClasses: Seq[Class[Message]] = Seq(reflections.getSubTypesOf(classOf[Message]).toArray: _*)
      .map(_.asInstanceOf[Class[Message]])
      .filter(v => !v.isAnonymousClass)
      .filter(v => !Modifier.isAbstract(v.getModifiers))
    allMessageClasses.foreach(v => {
      Deserializer.register(v)
      println(s"${v.getSimpleName} registered for deserialization")
    })
  }

  def register[T <: Message](metaMessage: T): Unit = register(metaMessage.getClass)

  def register[T <: Message](clazz: Class[T]): Unit = classes = classes :+ clazz

  private def convert(value: Any, clazz: Class[_]): Option[Any] = {
    value match {
      case str: String =>
        Try {
          if (clazz.equals(classOf[Double])) {
            Some(str.toDouble)
          } else if (clazz.equals(classOf[Int])) {
            Some(str.toInt)
          } else if (clazz.equals(classOf[String])) {
            Some(str)
          } else if (clazz.equals(classOf[Boolean])) {
            Some(str.toBoolean)
          } else {
            None
          }
        }.getOrElse(None)
      case _ => None
    }

  }

  private def deserializeMeta(message: QakMessage): Message = {
    val clazz = classes.find(_.getSimpleName.toLowerCase == message.message.id)
    if (clazz.isDefined) {
      val constructor = clazz.get.getConstructors()(0) //TODO: if the message is contained in a class the last parameter of the constructor is the $outer
      val parameters = constructor.getParameterTypes.zipWithIndex.map(a => (a._1, message.message.params(a._2))).map(a => convert(a._2, a._1))
      if (parameters.forall(_.isDefined)) {
        constructor.newInstance(parameters.map(_.get): _*).asInstanceOf[Message]
      } else {
        message.message
      }
    } else {
      message.message
    }
  }

  def apply(str: String, from:Context): Option[QakMessage] = {
    import InteractionType._
    Try {
      val msg = str.drop(4).dropRight(1)
      val par = msg.dropWhile(_ != '(').drop(1).takeWhile(_ != ')').split(',').map(_.trim)
      val inf = msg.split(',').take(4).map(_.trim)
      val interaction = inf(1) match {
        case "event" => Event
        case "dispatch" => Dispatch
        case "request" => Request
        case "reply" => Reply
      }
      val msgTmp = QakMessage(interaction, new Message {
        override def id: String = inf(0)

        override def params: Seq[Any] = par
      }, ExternalQActor(inf(2), from), ExternalQActor(inf(3), from))
      Some(msgTmp.copy(message = deserializeMeta(msgTmp)))
    }.getOrElse(None)
  }


}
