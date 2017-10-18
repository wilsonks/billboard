package roulette.ecs

import com.badlogic.gdx.graphics.Color
import device.cammegh.slingshot.Win
import display.ecs._
import monix.reactive.{Observable, Observer}
import roulette.State
import rx.{Ctx, Rx, Var}


class BillboardScene extends Scene[Any, Any]("billboard1") {

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

    val maxSpins = 300
    val blackNumbers = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
    val redNumbers = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
    val evenNumbers = List.range(2,37,2)
    val oddNumbers = List.range(1,36,2)

    val zeroSymbols = List(" 0", "00")
    val blackSymbols = blackNumbers.map(i => (" " + i.toString()).takeRight(2))
    val redSymbols = redNumbers.map(i => (" " + i.toString()).takeRight(2))
    val evenSymbols = evenNumbers.map(i => (" " + i.toString()).takeRight(2))
    val oddSymbols = oddNumbers.map(i => (" " + i.toString()).takeRight(2))

    val symbols1to18 = List.range(1,19).map(i => (" " + i.toString()).takeRight(2))
    val symbols19to36 = List.range(19,37).map(i => (" " + i.toString()).takeRight(2))

    val symbols1to12 = List.range(1,13).map(i => (" " + i.toString()).takeRight(2))
    val symbols13to24 = List.range(13,25).map(i => (" " + i.toString()).takeRight(2))
    val symbols25to36 = List.range(25,37).map(i => (" " + i.toString()).takeRight(2))

    val symbolToInt = (blackSymbols ++ redSymbols ++ zeroSymbols)
      .groupBy(s => s)
      .mapValues(li => li.head.trim().toInt)

    //Level 0
    val spinResults: Var[Seq[String]] = Var(Seq.empty[String])

    (scene.root / "maxWinHistory").label.setText(s"Based on last $maxSpins games")

    //Level 1
    val lastWinNumber = spinResults.map(x => x.headOption.getOrElse(""))
    val spinHistory = spinResults.map(x => x.take(maxSpins))

    val spinCount = Rx(if (spinHistory().isEmpty) maxSpins else spinHistory().length)

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

    oddPercentage.map(x => Rx(x).updates(scene.root / "oddPercentage"))
    evenPercentage.map(x => Rx(x).updates(scene.root / "evenPercentage"))

    redPercentage.map(x => Rx(x).updates(scene.root / "redPercentage"))
    blackPercentage.map(x =>  Rx(x).updates(scene.root / "blackPercentage"))
    greenPercentage.updates(scene.root / "colorZeroPercentage")

    oneTo12Percentage.map(x => Rx(x).updates(scene.root / "oneTo12Percentage"))
    thirteenTo24Percentage.map(x => Rx(x).updates(scene.root / "thirteenTo24Percentage"))
    twentyFiveTo36Percentage.map(x => Rx(x).updates(scene.root / "twentyFiveTo36Percentage"))

    oneTo18Percentage.map(x => Rx(x).updates(scene.root / "oneTo18Percentage"))
    nineteenTo36Percentage.map(x => Rx(x).updates(scene.root / "nineteenTo36Percentage"))

    spinResults.trigger {
    println("Spin Result = " + spinResults.now)
    println("Spin Count = " + spinCount.now)
    println("Last Win Number = " + lastWinNumber.now)
    println("Spin History = " + spinHistory.now)
    println("Zero Count = " + zeroCount.now)
    }

    val symbols = (0 to 36).map(i => (" " + i.toString()).takeRight(2))
    val emptyCounts = symbols.map(_ -> 0).toMap
    val liveCounts = spinResults.map(xs => (emptyCounts ++ xs.groupBy(s => s).mapValues(_.size)).toSeq.sortBy(_._2))
    val hot = liveCounts.map(_.takeRight(4))
    val cold = liveCounts.map(_.take(4))

    hot.map(m => m.zipWithIndex
      .foreach {
        case ((x, y), z) => {
          val index = 4 - z
          (scene.root / s"hc$index").label.setText(s"$y")
          blackSymbols.contains(x) match {
            case true => Rx(x)
              .updatesWithColor(scene.root / s"h$index", new Color(0x000000ff))
            case false => {
              redSymbols.contains(x) match {
                case true => Rx(x)
                  .updatesWithColor(scene.root / s"h$index", new Color(0xc70000ff))
                case false => Rx(x)
                  .updatesWithColor(scene.root / s"h$index", new Color(0x2A7302FF))
              }

            }
          }
        }
      })

    cold.map(m => m.zipWithIndex
      .foreach {
        case ((x, y), z) => {
          val index = z + 1
          (scene.root / s"cc$index").label.setText(s"$y")
          blackSymbols.contains(x) match {
            case true => Rx(x)
              .updatesWithColor(scene.root / s"c$index", new Color(0x000000ff))
            case false => {
              redSymbols.contains(x) match {
                case true => Rx(x)
                  .updatesWithColor(scene.root / s"c$index", new Color(0xc70000ff))
                case false => Rx(x)
                  .updatesWithColor(scene.root / s"c$index", new Color(0x2A7302FF))
              }

            }
          }
        }
      })


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
            Rx("").updates(scene.root / s"r$j")
            Rx("").updates(scene.root / s"g$j")
            Rx(num).updates(scene.root / s"b$j")
          }
          case (j, num, "red") => {
            Rx("").updates(scene.root / s"b$j")
            Rx("").updates(scene.root / s"g$j")
            Rx(num).updates(scene.root / s"r$j")
          }
          case (j, num, "green") => {
            Rx("").updates(scene.root / s"b$j")
            Rx("").updates(scene.root / s"r$j")
            Rx(num).updates(scene.root / s"g$j")
          }

          case (_, _, _) =>
        }
    })

    lastWinNumber.map(x =>
      blackSymbols.contains(x) match {
      case true => Rx(x)
        .updatesWithColor(scene.root / "lastWinNumber", new Color(0x000000FF))
      case false => {
        redSymbols.contains(x) match {
          case true => Rx(x)
            .updatesWithColor(scene.root / "lastWinNumber", new Color(0xFF0000FF))
          case false => Rx(x)
            .updatesWithColor(scene.root / "lastWinNumber", new Color(0x00FF00FF))
        }

      }
    })



    reader.foreach {
      case Win(num) => {
        try {
          spinResults() = (num +: spinResults.now).take(maxSpins)
        } catch {
          case t: Throwable => t.printStackTrace()
        }
      }
      case _ =>
    }
  }
}

object BillboardScene {
  def apply(): BillboardScene = new BillboardScene()
}