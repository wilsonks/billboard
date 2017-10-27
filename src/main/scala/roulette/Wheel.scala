package roulette

import better.files.File
import roulette.Event._
import rx.{Rx, Var}


sealed trait State extends Product with Serializable {
  type Transition = Event => State

  def name: String

  def history: Seq[String]

  def maxSpinCount: Int

  def min: Int

  def max: Int

  def transition: Transition
}


object State {

  val state: Var[State] = Var(Booting("EURO1", Seq.empty[String], 100, 100, 10000))


  val file = File("/home/wilson/data/state.txt")

  if (file.exists) {
    state() = file.readDeserialized[Booting]
  } else {
    file.createIfNotExists(false, true)
    file.writeSerialized(state.now)
  }

  state() = state.now.transition(Event.FileReadCompleted)

  //Rx Level 0
  val spinResults = state.map(x => x.history)
  val maxSpins = state.map(x => x.maxSpinCount)
  val name = state.map(x => x.name)
  val min = state.map(x => x.min)
  val max = state.map(x => x.max)
  //Rx Level 1
  val lastWinNumber = spinResults.map(x => x.headOption.getOrElse(""))
  val spinHistory = spinResults.map(x => x.take(maxSpins()))
  val spinCount = Rx(if (spinHistory().isEmpty) maxSpins() else spinHistory().length)
  val blackNumbers = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
  val redNumbers = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
  val evenNumbers = List.range(2, 37, 2)
  val oddNumbers = List.range(1, 36, 2)
  //  val zeroSymbols = List(" 0", "00")
  val zeroSymbols = List(" 0")
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
  val evenCount = spinHistory.map(x => x.count(m => evenSymbols.contains(m)))
  val oddCount = spinHistory.map(x => x.count(m => oddSymbols.contains(m)))
  val zeroCount = spinHistory.map(x => x.count(m => zeroSymbols.contains(m)))
  val redCount = spinHistory.map(x => x.count(m => redSymbols.contains(m)))
  val blackCount = spinHistory.map(x => x.count(m => blackSymbols.contains(m)))
  val oneTo18Count = spinHistory.map(x => x.count(m => symbols1to18.contains(m)))
  val nineteenTo36Count = spinHistory.map(x => x.count(m => symbols19to36.contains(m)))
  val oneTo12Count = spinHistory.map(x => x.count(m => symbols1to12.contains(m)))
  val thirteenTo24Count = spinHistory.map(x => x.count(m => symbols13to24.contains(m)))
  val twentyFiveTo36Count = spinHistory.map(x => x.count(m => symbols25to36.contains(m)))
  val greenPercentage = Rx("%d%%".format((100 * zeroCount()) / spinCount()))
  val oddPercentage = Rx("%d%%".format((100 * oddCount()) / spinCount()))
  val evenPercentage = Rx("%d%%".format((100 * evenCount()) / spinCount()))
  val redPercentage = Rx("%d%%".format((100 * redCount()) / spinCount()))
  val blackPercentage = Rx("%d%%".format((100 * blackCount()) / spinCount()))
  val oneTo12Percentage = Rx("%d%%".format((100 * oneTo12Count()) / spinCount()))
  val thirteenTo24Percentage = Rx("%d%%".format((100 * thirteenTo24Count()) / spinCount()))
  val twentyFiveTo36Percentage = Rx("%d%%".format((100 * twentyFiveTo36Count()) / spinCount()))
  val oneTo18Percentage = Rx("%d%%".format((100 * oneTo18Count()) / spinCount()))
  val nineteenTo36Percentage = Rx("%d%%".format((100 * nineteenTo36Count()) / spinCount()))

  val symbols = redSymbols ++ blackSymbols ++ zeroSymbols
  val emptyCounts = symbols.map(_ -> 0).toMap
  val liveCounts = spinHistory.map(xs => (emptyCounts ++ xs.groupBy(s => s).mapValues(_.size)).toSeq.sortBy(_._2))
  val hot = liveCounts.map(_.takeRight(4))
  val cold = liveCounts.map(_.take(4))

  val minWidth = 7.2
  val position0: Float = 310
  val red = Rx(100 * redCount() / spinCount() * minWidth)
  val odd = Rx(100 * oddCount() / spinCount() * minWidth)
  val oneTo18 = Rx(100 * oneTo18Count() / spinCount() * minWidth)
  val green = Rx(100 * zeroCount() / spinCount() * minWidth)


  case class Running(name: String, history: Seq[String], maxSpinCount: Int, min: Int, max: Int) extends State {
    def transition: Transition = {
      case NameChanged(next) => copy(name = next)
      case MaxChanged(next) => copy(max = next)
      case MinChanged(next) => copy(min = next)
      case MaxSpinChanged(next) => copy(maxSpinCount = next)
      case SpinCompleted(num) => copy(history = (num +: history).take(maxSpinCount))
      case _ => copy()
    }

  }


  case class Booting(name: String, history: Seq[String], maxSpinCount: Int, min: Int, max: Int) extends State {
    def transition: Transition = {
      case NameChanged(next) => copy(name = next)
      case MaxChanged(next) => copy(max = next)
      case MinChanged(next) => copy(min = next)
      case MaxSpinChanged(next) => copy(maxSpinCount = next)
      case FileReadCompleted => Running(name, history, maxSpinCount, min, max)
      case _ => copy()
    }

  }


}

sealed trait Event extends Product with Serializable

object Event {

  case class SpinCompleted(num: String) extends Event

  case class NameChanged(name: String) extends Event

  case class MaxSpinChanged(value: Int) extends Event

  case class MinChanged(value: Int) extends Event

  case class MaxChanged(value: Int) extends Event

  case object FileReadCompleted extends Event

}