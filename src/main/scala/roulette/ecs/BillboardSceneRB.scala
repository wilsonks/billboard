package roulette.ecs

import com.badlogic.ashley.core.Entity
import com.badlogic.gdx.graphics.Color
import display.ecs._
import display.io._
import monix.execution.Cancelable
import monix.execution.cancelables.SerialCancelable
import monix.reactive.{Observable, Observer}
import roulette.Event
import roulette.Event.SpinCompleted
import roulette.State._
import rx.{Ctx, Rx, Var}


class BillboardSceneRB extends Scene[Any, Any]("billboard2") {

  implicit def owner: Ctx.Owner = Ctx.Owner.Unsafe

  override def bind(writer: Observer[Any], reader: Observable[Any])(implicit scene: SceneContext): Unit = try {
    scene.loader.loadScene("MainScene")


    reader.foreach {
      case event: SpinCompleted => {
        try {
          state() = state.now.transition(event)
        } catch {
          case t: Throwable => t.printStackTrace()
        }
      }
      case _ =>
    }

    val target = Var(Option.empty[EditText])
    val spinEdit = EditText(scene.root / "maxWin", 3, Var(true))
    val nameEdit = EditText(scene.root / "name", 10, Var(true))
    val minEdit = EditText(scene.root / "min", 5, Var(true))
    val maxEdit = EditText(scene.root / "max", 5, Var(true))

    (scene.root / "maxWin").label.setText(maxSpins.now.toString)
    (scene.root / "name").label.setText(name.now)
    (scene.root / "min").label.setText(min.now.toString)
    (scene.root / "max").label.setText(max.now.toString)

    oddPercentage.map(x => Rx(x).updates(scene.root / "oddPercentage"))
    evenPercentage.map(x => Rx(x).updates(scene.root / "evenPercentage"))

    redPercentage.map(x => Rx(x).updates(scene.root / "redPercentage"))
    blackPercentage.map(x => Rx(x).updates(scene.root / "blackPercentage"))

    oneTo18Percentage.map(x => Rx(x).updates(scene.root / "oneTo18Percentage"))
    nineteenTo36Percentage.map(x => Rx(x).updates(scene.root / "nineteenTo36Percentage"))

    spinResults.trigger {

      //      println(state.now)
      updateEntity(scene.root / "lastWinNumber", lastWinNumber.now, getColor(lastWinNumber.now))

      (scene.root / "red").dimensions.width = red.now.toFloat
      (scene.root / "green1").transform.x = position0 + red.now.toFloat
      (scene.root / "green1").dimensions.width = green.now.toFloat

      (scene.root / "green2").transform.x = position0 + odd.now.toFloat
      (scene.root / "green2").dimensions.width = green.now.toFloat

      (scene.root / "green3").transform.x = position0 + oneTo18.now.toFloat
      (scene.root / "green3").dimensions.width = green.now.toFloat

      file.clear().writeSerialized(state.now)
    }

    hot.map(m => m.zipWithIndex
      .foreach {
        case ((x, y), z) => {
          val index = 4 - z
          (scene.root / s"hc$index").label.setText(s"$y")
          updateEntity(scene.root / s"h$index", x, getColor(x))

        }
      })

    cold.map(m => m.zipWithIndex
      .foreach {
        case ((x, y), z) => {
          val index = z + 1
          (scene.root / s"cc$index").label.setText(s"$y")
          updateEntity(scene.root / s"c$index", x, getColor(x))
        }
      })

    for (i <- 1 to 15) {
      disableEntity(scene.root / s"b$i")
      disableEntity(scene.root / s"r$i")
      disableEntity(scene.root / s"g$i")
    }
    spinResults.map(m => m.take(16).foldLeft(List[(String, String)]()) { (result, x) =>
      x match {
        case m if blackSymbols.contains(m) => if (result.nonEmpty) result.:+((x, "black")) else List((x, "black"))
        case m if redSymbols.contains(m) => if (result.nonEmpty) result.:+((x, "red")) else List((x, "red"))
        case _ => if (result.nonEmpty) result.:+((x, "green")) else List((x, "green"))
      }
    }.zipWithIndex foreach {
      case (e, i) =>
        (i, e._1, e._2) match {
          case (0, _, _) =>
          case (j, num, "black") => {
            disableEntity(scene.root / s"r$j")
            disableEntity(scene.root / s"g$j")
            updateEntity(scene.root / s"b$j", num, getColor(num))
          }
          case (j, num, "red") => {
            disableEntity(scene.root / s"b$j")
            disableEntity(scene.root / s"g$j")
            updateEntity(scene.root / s"r$j", num, getColor(num))

          }
          case (j, num, "green") => {
            disableEntity(scene.root / s"r$j")
            disableEntity(scene.root / s"b$j")
            updateEntity(scene.root / s"g$j", num, getColor(num))

          }
          case (_, _, _) =>
        }
    })

    nameEdit.checked.trigger {
      val current = name.now
      nameEdit.checked.foreach(c => target() = if (c) Some(nameEdit) else None)
      nameEdit.checked.map(if (_) 0.5f else 1f).foreach(nameEdit.entity.tint.color.a = _)
      val keying = SerialCancelable()
      val keys = scene.inputs.collect {
        case KeyTyped(key) => key.toString
      }
      target.foreach(t =>
        keying := t.fold(Cancelable.empty)(e =>
          keys.takeWhile(_ != "\n")
            .doOnTerminate(_ => scene.input() = InputEmpty)
            .scan("")(_.takeRight(e.size - 1) + _).foreach(e.entity.label.setText)))

      val updated = (scene.root / "name").label.text.toString
      updated match {
        case x if x == "" => (scene.root / "name").label.setText(s"$current")
        case _ => state() = state.now.transition(Event.NameChanged(updated))
      }
    }


    spinEdit.checked.trigger {
      val current = maxSpins.now
      spinEdit.checked.foreach(c => target() = if (c) Some(spinEdit) else None)
      spinEdit.checked.map(if (_) 0.5f else 1f).foreach(spinEdit.entity.tint.color.a = _)
      val keying = SerialCancelable()
      val keys = scene.inputs.collect {
        case KeyTyped(key) => key.toString
      }
      target.foreach(t =>
        keying := t.fold(Cancelable.empty)(e =>
          keys.takeWhile(_ != "\n")
            .doOnTerminate(_ => scene.input() = InputEmpty)
            .scan("")(_.takeRight(e.size - 1) + _).foreach(e.entity.label.setText)))

      val updated = (scene.root / "maxWin").label.text.toString
      updated match {
        case x if x == "" => (scene.root / "maxWin").label.setText(s"$current")
        case x if (x forall Character.isDigit) && (x.toInt >= 15)
        => state() = state.now.transition(Event.MaxSpinChanged(updated.toInt))
        case _ => (scene.root / "maxWin").label.setText(s"$current")
      }
    }


    minEdit.checked.trigger {
      val current = min.now
      minEdit.checked.foreach(c => target() = if (c) Some(minEdit) else None)
      minEdit.checked.map(if (_) 0.5f else 1f).foreach(minEdit.entity.tint.color.a = _)
      val keying = SerialCancelable()
      val keys = scene.inputs.collect {
        case KeyTyped(key) => key.toString
      }
      target.foreach(t =>
        keying := t.fold(Cancelable.empty)(e =>
          keys.takeWhile(_ != "\n")
            .doOnTerminate(_ => scene.input() = InputEmpty)
            .scan("")(_.takeRight(e.size - 1) + _).foreach(e.entity.label.setText)))

      val updated = (scene.root / "min").label.text.toString
      updated match {
        case x if x == "" => (scene.root / "min").label.setText(s"$current")
        case x if (x forall Character.isDigit) && (x.toInt > 0)
        => state() = state.now.transition(Event.MinChanged(updated.toInt))
        case _ => (scene.root / "min").label.setText(s"$current")
      }
    }


    maxEdit.checked.trigger {
      val current = max.now
      maxEdit.checked.foreach(c => target() = if (c) Some(maxEdit) else None)
      maxEdit.checked.map(if (_) 0.5f else 1f).foreach(maxEdit.entity.tint.color.a = _)
      val keying = SerialCancelable()
      val keys = scene.inputs.collect {
        case KeyTyped(key) => key.toString
      }
      target.foreach(t =>
        keying := t.fold(Cancelable.empty)(e =>
          keys.takeWhile(_ != "\n")
            .doOnTerminate(_ => scene.input() = InputEmpty)
            .scan("")(_.takeRight(e.size - 1) + _).foreach(e.entity.label.setText)))

      val updated = (scene.root / "max").label.text.toString
      updated match {
        case x if x == "" => (scene.root / "max").label.setText(s"$current")
        case x if (x forall Character.isDigit) && (x.toInt > 0)
        => state() = state.now.transition(Event.MaxChanged(updated.toInt))
        case _ => (scene.root / "max").label.setText(s"$current")
      }
    }


    def disableEntity(e: Entity) = {
      e.item.visible = false
    }

    def updateEntity(e: Entity, x: String, col: Color): Unit = {
      e.item.visible = true
      (e / "number").label.setText(x)
      (e / "color").tint.color = col
    }

    def getColor(s: String): Color = {
      s match {
        case x if blackSymbols.contains(x) => Color.BLACK
        case x if redSymbols.contains(x) => Color.RED
        case _ => Color.GREEN
      }
    }

  } catch {
    case t: Throwable => t.printStackTrace()
  }
}

case class EditText(entity: Entity, size: Int, enabled: Rx[Boolean])(implicit owner: Ctx.Owner, scene: SceneContext) {
  val checked = scene.input.fold(false) {
    case (prev, e: PointerUp) => if (enabled.now && entity.occupies(e.x, e.y)) !prev else prev
    case (_, InputEmpty) => false
    case (prev, _) => prev
  }
}


object BillboardSceneRB {
  def apply(): BillboardSceneRB = new BillboardSceneRB()
}