package qactor.message

object InteractionType extends Enumeration {
  type InteractionType = Value
  val Dispatch, Event, Request, Reply = Value
}
