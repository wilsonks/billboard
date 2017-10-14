package roulette.ecs

import com.badlogic.gdx.graphics.Color
import device.cammegh.slingshot.Win
import display.ecs._
import monix.reactive.{Observable, Observer}
import roulette.State
import rx.{Ctx, Rx, Var}


class BillboardSceneUpdated extends Scene[Any, Any]("billboard") {

  implicit def owner: Ctx.Owner = Ctx.Owner.Unsafe

  override def bind(writer: Observer[Any], reader: Observable[Any])(implicit scene: SceneContext): Unit = {
    scene.loader.loadScene("MainScene")

    val state: Var[State] = Var(State.Empty(10))
    val stateLast = state.map {
      case _: State.Empty => ""
      case s: State.Running => s.last
    }
    val stateHistory = state.map {
      case _: State.Empty => Nil
      case s: State.Running => s.history
    }

    val maxSpins = 100
    val blackNumbers = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
    val redNumbers = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)

    val blacks = List(" 2", " 4", " 6", " 8", "10", "11", "13", "15", "17",
      "02", "04", "06", "08", "20", "22", "24", "26", "28", "29", "31", "33", "35")

    val reds = List(" 1", " 3", " 5", " 7", " 9", "12", "14", "16", "18", "19",
      "01", "03", "05", "05", "09", "21", "23", "25", "27", "30", "32", "34", "38")

    val numberMap: Map[String, Int] = Map("0" -> 0, " 0" -> 0, "00" -> 0, " 1" -> 1, " 2" -> 2, " 3" -> 3, " 4" -> 4,
      " 5" -> 5, " 6" -> 6, " 7" -> 7, " 8" -> 8, " 9" -> 9, "01" -> 1,
      "02" -> 2, "03" -> 3, "04" -> 4, "05" -> 5, "06" -> 6, "07" -> 7,
      "08" -> 8, "09" -> 9, "10" -> 10, "11" -> 11, "12" -> 12, "13" -> 13,
      "14" -> 14, "15" -> 15, "16" -> 16, "17" -> 17, "18" -> 18, "19" -> 19,
      "20" -> 20, "21" -> 21, "22" -> 22, "23" -> 23, "24" -> 24, "25" -> 25,
      "26" -> 26, "27" -> 27, "28" -> 28, "29" -> 29, "30" -> 30, "31" -> 31,
      "32" -> 32, "33" -> 33, "34" -> 34, "35" -> 35, "36" -> 36
    )

    //Level 0
    val spinResults: Var[Seq[String]] = Var(Seq.empty[String])

    //Level 1
    val lastWinNumber = Rx(spinResults().headOption).map(_.getOrElse(""))
    val historyData = Rx(spinResults())
    val spinCount = Rx(spinResults().take(maxSpins).length)

    val evenCount = Rx(spinResults().take(maxSpins).count(x => {
      (numberMap(x) % 2 == 0) & (numberMap(x) != 0)
    }))
    val oddCount = Rx(spinResults().take(maxSpins).count(x => {
      numberMap(x) % 2 != 0
    }))
    val zeroCount = Rx(spinResults().take(maxSpins).count(x => {
      numberMap(x) == 0
    }))

    val redCount = Rx(spinResults().take(maxSpins).count(x => {
      redNumbers.contains(numberMap(x))
    }))
    val blackCount = Rx(spinResults().take(maxSpins).count(x => {
      blackNumbers.contains(numberMap(x))
    }))

    val oneTo12Count = Rx(spinResults().take(maxSpins).count(x => {
      1 to 12 contains numberMap(x)
    }))

    val thirteenTo24Count = Rx(spinResults().take(maxSpins).count(x => {
      13 to 24 contains numberMap(x)
    }))

    val twentyFiveTo36Count = Rx(spinResults().take(maxSpins).count(x => {
      25 to 36 contains numberMap(x)
    }))

    val oneTo18Count = Rx(spinResults().take(maxSpins).count(x => {
      1 to 18 contains numberMap(x)
    }))
    val nineteenTo36Count = Rx(spinResults().take(maxSpins).count(x => {
      19 to 36 contains numberMap(x)
    }))


    val oddPercentage = Rx("%d%%".format((100 * oddCount()) / spinCount()))
    val evenPercentage = Rx("%d%%".format((100 * evenCount()) / spinCount()))
    val zeroPercentage = Rx("%d%%".format((100 * zeroCount()) / spinCount()))

    val redPercentage = Rx("%d%%".format((100 * redCount()) / spinCount()))
    val blackPercentage = Rx("%d%%".format((100 * blackCount()) / spinCount()))
    val greenPercentage = Rx("%d%%".format((100 * zeroCount()) / spinCount()))

    val oneTo12Percentage = Rx("%d%%".format((100 * oneTo12Count()) / spinCount()))
    val thirteenTo24Percentage = Rx("%d%%".format((100 * thirteenTo24Count()) / spinCount()))
    val twentyFiveTo36Percentage = Rx("%d%%".format((100 * twentyFiveTo36Count()) / spinCount()))

    val oneTo18Percentage = Rx("%d%%".format((100 * oneTo18Count()) / spinCount()))
    val nineteenTo36Percentage = Rx("%d%%".format((100 * nineteenTo36Count()) / spinCount()))

    (scene.root / "maxWinHistory").label.setText(s"Based on last $maxSpins games")

    spinResults.triggerLater {
      blacks.contains(lastWinNumber.now) match {
        case true => Rx(lastWinNumber.now)
          .updatesWithColor(scene.root / "lastWinNumber", new Color(0x000000ff))
        case false => {
          reds.contains(lastWinNumber.now) match {
            case true => Rx(lastWinNumber.now)
              .updatesWithColor(scene.root / "lastWinNumber", new Color(0xc70000ff))
            case false => Rx(lastWinNumber.now)
              .updatesWithColor(scene.root / "lastWinNumber", new Color(0x2A7302FF))
          }

        }
      }

      historyData.now.take(11).foldLeft(List[(String, String)]()) { (result, x) =>
        val (p1, p2) = x match {
          case " 2" | " 4" | " 6" | " 8" | "10" | "11" | "13" | "15" | "17" | "20" | "22" |
               "02" | "04" | "06" | "08" |
               "24" | "26" | "28" | "29" | "31" | "33" | "35" => (x, "black")
          case " 1" | " 3" | " 5" | " 7" | " 9" | "12" | "14" | "16" | "18" | "19" | "21" |
               "01" | "03" | "05" | "07" | "09" |
               "23" | "25" | "27" | "30" | "32" | "34" | "36" => (x, "red")
          case "0" | " 0" | "00" => (x, "green")
        }
      {
        if (result.nonEmpty) result :+ (p1, p2) else List((p1, p2))

      }
      }.zipWithIndex foreach {
        case (e, i) =>
          (i, e._1, e._2) match {
            case (0, _, _) =>
            case (j, num, "black") => {
              Rx("").updates(scene.root / s"r$j")
              Rx("").updates(scene.root / s"z$j")
              Rx(num).updates(scene.root / s"b$j")
            }
            case (j, num, "red") => {
              Rx("").updates(scene.root / s"b$j")
              Rx("").updates(scene.root / s"z$j")
              Rx(num).updates(scene.root / s"r$j")
            }
            case (j, num, "green") => {
              Rx("").updates(scene.root / s"b$j")
              Rx("").updates(scene.root / s"r$j")
              Rx(num).updates(scene.root / s"z$j")
            }

          }
      }


      oddPercentage.updates(scene.root / "oddPercentage")
      zeroPercentage.updates(scene.root / "zeroPercentage")
      evenPercentage.updates(scene.root / "evenPercentage")

      redPercentage.updates(scene.root / "redPercentage")
      blackPercentage.updates(scene.root / "blackPercentage")
      greenPercentage.updates(scene.root / "colorZeroPercentage")

      oneTo12Percentage.updates(scene.root / "oneTo12Percentage")
      thirteenTo24Percentage.updates(scene.root / "thirteenTo24Percentage")
      twentyFiveTo36Percentage.updates(scene.root / "twentyFiveTo36Percentage")

      oneTo18Percentage.updates(scene.root / "oneTo18Percentage")
      nineteenTo36Percentage.updates(scene.root / "nineteenTo36Percentage")

    }

    val symbols = (0 to 36).map(i => (" " + i.toString()).takeRight(2))
    val emptyCounts = symbols.map(_ -> 0).toMap
    val liveCounts = spinResults.map(xs => (emptyCounts ++ xs.groupBy(s => s).mapValues(_.size)).toSeq.sortBy(_._2))
    val hot = liveCounts.map(_.takeRight(4))
    val cold = liveCounts.map(_.take(4))
    hot.foreach(h => println(s"hot: $h"))
    cold.foreach(h => println(s"cold: $h"))

    reader.foreach {
      case Win(num) => {
        try {
          spinResults() = (num +: spinResults.now).take(100)
        } catch {
          case t: Throwable => t.printStackTrace()
        }
      }
      case _ =>
    }
  }
}

object BillboardSceneUpdated {
  def apply(): BillboardSceneUpdated = new BillboardSceneUpdated()
}