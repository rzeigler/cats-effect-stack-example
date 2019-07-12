package example

sealed trait Token extends Product with Serializable

object Token {
    case object Add extends Token
    case object Subtract extends Token
}
