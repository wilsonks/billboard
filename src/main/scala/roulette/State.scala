package roulette

import device.cammegh.slingshot.Win

sealed trait State extends Product with Serializable {
  type Transition = Win => State

  def transition: Transition
}

object State {

  sealed trait Running extends State {
    def last: String
    def history: List[String]
  }

  case class Empty(historyCount: Int) extends State {
    override def transition: Transition = {
      case Win(num) => Booting(historyCount, num, Nil)
    }
  }

  case class Booting(historyCount: Int, last: String, history: List[String]) extends Running {
    override def transition: Transition = {
      case Win(num) if history.size == historyCount => Booted(num, (num :: history).init)
      case Win(num) => copy(last = num, history = last :: history)
    }
  }

  case class Booted(last: String, history: List[String]) extends Running {

    override def transition: Transition = {
      case Win(num) => copy(last = num, history = (num :: history).init)
    }
  }
}