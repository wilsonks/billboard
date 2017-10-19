package roulette

import roulette.Event._


case class BillboardState(name: String, history: Seq[String], maxSpinCount: Int, min: Int, max: Int) {
  type Transition = Event => BillboardState

  def transition: Transition = {
    case NameChanged(next) => copy(name = next)
    case MaxSpinChanged(next) => copy(maxSpinCount = next)
    case MaxChanged(next) => copy(max = next)
    case MinChanged(next) => copy(min = next)
    case SpinCompleted(num) => copy(history = (num +: history).take(maxSpinCount))
  }

}

object BillboardState {

  val blackNumbers = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
  val redNumbers = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
  val evenNumbers = List.range(2, 37, 2)
  val oddNumbers = List.range(1, 36, 2)

  val zeroSymbols = List(" 0", "00")
  val blackSymbols = blackNumbers.map(i => (" " + i.toString).takeRight(2))
  val redSymbols = redNumbers.map(i => (" " + i.toString).takeRight(2))
  val evenSymbols = evenNumbers.map(i => (" " + i.toString).takeRight(2))
  val oddSymbols = oddNumbers.map(i => (" " + i.toString).takeRight(2))

  val symbols1to18 = List.range(1, 19).map(i => (" " + i.toString).takeRight(2))
  val symbols19to36 = List.range(19, 37).map(i => (" " + i.toString).takeRight(2))

  val symbols1to12 = List.range(1, 13).map(i => (" " + i.toString).takeRight(2))
  val symbols13to24 = List.range(13, 25).map(i => (" " + i.toString).takeRight(2))
  val symbols25to36 = List.range(25, 37).map(i => (" " + i.toString).takeRight(2))

  val symbolToInt = (blackSymbols ++ redSymbols ++ zeroSymbols)
    .groupBy(s => s)
    .mapValues(li => li.head.trim().toInt)


}

sealed trait Event extends Product with Serializable

object Event {

  case class SpinCompleted(num: String) extends Event

  case class NameChanged(name: String) extends Event

  case class MaxSpinChanged(value: Int) extends Event

  case class MinChanged(value: Int) extends Event

  case class MaxChanged(value: Int) extends Event

}