package roulette.ecs

import com.badlogic.gdx.graphics.Color
import device.cammegh.slingshot.Win
import display.ecs._
import monix.reactive.{Observable, Observer}
import rx.{Ctx, Rx, Var}


class BillboardScene extends Scene[Any, Any]("billboard") {

  implicit def owner: Ctx.Owner = Ctx.Owner.Unsafe

  override def bind(writer: Observer[Any], reader: Observable[Any])(implicit scene: SceneContext): Unit = {
    scene.loader.loadScene("MainScene")

    val maxSpins = 100
    val maxHistoryInfo = Rx("Wins in the Last 100 Spins")
    val blackNumbers = List(2, 4, 6, 8, 10, 11, 13, 15, 17, 20, 22, 24, 26, 28, 29, 31, 33, 35)
    val redNumbers   = List(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)
    val blacks       = List(" 2", " 4", " 6", " 8", "10", "11", "13", "15", "17",
                            "02", "04", "06", "08",
                            "20", "22", "24", "26", "28", "29", "31", "33", "35")
    val reds         = List(" 1", " 3", " 5", " 7", " 9", "12", "14", "16", "18", "19",
                            "01", "03", "05", "05", "09",
                            "21", "23", "25", "27", "30", "32", "34", "38")

    val numberMap: Map[String, Int] = Map(" 0" -> 0, "00" -> 0, " 1" -> 1, " 2" -> 2, " 3" -> 3, " 4" -> 4,
      " 5" -> 5, " 6" -> 6, " 7" -> 7, " 8" -> 8, " 9" -> 9, "01" -> 1,
      "02" -> 2, "03" -> 3, "04" -> 4, "05" -> 5, "06" -> 6, "07" -> 7,
      "08" -> 8, "09" -> 9, "10" -> 10, "11" -> 11, "12" -> 12, "13" -> 13,
      "14" -> 14, "15" -> 15, "16" -> 16, "17" -> 17, "18" -> 18, "19" -> 19,
      "20" -> 20, "21" -> 21, "22" -> 22, "23" -> 23, "24" -> 24, "25" -> 25,
      "26" -> 26, "27" -> 27, "28" -> 28, "29" -> 29, "30" -> 30, "31" -> 31,
      "32" -> 32, "33" -> 33, "34" -> 34, "35" -> 35, "36" -> 36, "37" -> 37
    )

    val state: Var[Seq[String]] = Var(Seq.empty[String])
    val lastWinNumber = Rx(state().headOption)
    val spinCount = Rx(state().take(maxSpins).length)

    def updateOddEvenFields(): Unit = {

      val zeroCount = Rx(state().take(maxSpins).foldLeft(0) { (x, y) =>
        if (numberMap(y) == 0) x + 1 else x
      })

      val evenCount = Rx(state().take(maxSpins).foldLeft(0) { (x, y) =>
        if ((numberMap(y) % 2) == 0) x + 1 else x
      })

      val oddCount = Rx(state().take(maxSpins).foldLeft(0) { (x, y) =>
        if ((numberMap(y) % 2) != 0) x + 1 else x
      })


      val oddPercentage = Rx(Option("%d%%".format((100 * oddCount()) / Option(spinCount()).getOrElse(1))))
      val zeroPercentage = Rx(Option("%d%%".format((100 * zeroCount()) / Option(spinCount()).getOrElse(1))))
      val evenPercentage = Rx(Option("%d%%".format((100 * evenCount()) / Option(spinCount()).getOrElse(1))))

      oddPercentage.map(_.getOrElse("0%")).updates(scene.root / "oddPercentage")
      zeroPercentage.map(_.getOrElse("0%")).updates(scene.root / "zeroPercentage")
      evenPercentage.map(_.getOrElse("0%")).updates(scene.root / "evenPercentage")


    }

    def updateRedBlackFields(): Unit = {
      val redCount = Rx(state().take(maxSpins).count(x => {
        redNumbers.contains(numberMap(x))
      }))
      val blackCount = Rx(state().take(maxSpins).count(x => {
        blackNumbers.contains(numberMap(x))
      }))

      val blackPercentage = Rx(Option("%d%%".format((100 * blackCount()) / spinCount())))
      val colorZeroPercentage = Rx(Option("%d%%".format((100 * (spinCount() - (blackCount() + redCount()))) / spinCount())))
      val redPercentage = Rx(Option("%d%%".format((100 * redCount()) / spinCount())))

      blackPercentage.map(_.getOrElse("")).updates(scene.root / "blackPercentage")
      colorZeroPercentage.map(_.getOrElse("")).updates(scene.root / "colorZeroPercentage")
      redPercentage.map(_.getOrElse("")).updates(scene.root / "redPercentage")

    }

    def updateNumberFields(): Unit = {
      val oneTo18Count = Rx(state().take(maxSpins).count(x => {
        1 to 18 contains numberMap(x)
      }))
      val nineteenTo36Count = Rx(state().take(maxSpins).count(x => {
        19 to 36 contains numberMap(x)
      }))

      val oneTo12Count = Rx(state().take(maxSpins).count(x => {
        1 to 12 contains numberMap(x)
      }))

      val thirteenTo24Count = Rx(state().take(maxSpins).count(x => {
        13 to 24 contains numberMap(x)
      }))

      val twentyFiveTo36Count = Rx(state().take(maxSpins).count(x => {
        25 to 36 contains numberMap(x)
      }))

      val oneTo12Percentage = Rx(Option("%d%%"
        .format((100 * oneTo12Count()) / spinCount())))
      val thirteenTo24Percentage = Rx(Option("%d%%"
        .format((100 * thirteenTo24Count()) / spinCount())))
      val twentyFiveTo36Percentage = Rx(Option("%d%%"
        .format((100 * twentyFiveTo36Count()) / spinCount())))

      val oneTo18Percentage = Rx(Option("%d%%"
        .format((100 * oneTo18Count()) / spinCount())))
      val nineteenTo36Percentage = Rx(Option("%d%%"
        .format((100 * nineteenTo36Count()) / spinCount())))

      oneTo12Percentage.map(_.getOrElse(""))
        .updates(scene.root / "oneTo12Percentage")
      thirteenTo24Percentage.map(_.getOrElse(""))
        .updates(scene.root / "thirteenTo24Percentage")
      twentyFiveTo36Percentage.map(_.getOrElse(""))
        .updates(scene.root / "twentyFiveTo36Percentage")

      oneTo18Percentage.map(_.getOrElse(""))
        .updates(scene.root / "oneTo18Percentage")
      nineteenTo36Percentage.map(_.getOrElse(""))
        .updates(scene.root / "nineteenTo36Percentage")

    }

    def updateHistoryFields(): Unit = {

      val winHistoryMap: Unit = Rx(state().take(11).foldLeft(List[(String, String)]()) { (result, x) =>

        for (x <- 1 to 10) {
          val bEntity = scene.root / s"b$x"
          val rEntity = scene.root / s"r$x"
          val zEntity = scene.root / s"z$x"
          Rx("").updates(bEntity)
          Rx("").updates(rEntity)
          Rx("").updates(zEntity)
        }

        val (p1, p2) = x match {
          case " 2" | " 4" | " 6" | " 8" | "10" | "11" | "13" | "15" | "17" | "20" | "22" |
               "02" | "04" | "06" | "08" |
               "24" | "26" | "28" | "29" | "31" | "33" | "35" => (x, "black")
          case " 1" | " 3" | " 5" | " 7" | " 9" | "12" | "14" | "16" | "18" | "19" | "21" |
               "01" | "03" | "05" | "07" | "09" |
               "23" | "25" | "27" | "30" | "32" | "34" | "36" => (x, "red")
          case " 0" | "00" => (x, "green")
        }
      {
        result :+ (p1, p2)

      }
      }.zipWithIndex foreach {
        case (e, i) =>
          (i, e._1, e._2) match {
            case (j, num, "black") => Rx(num).updates(scene.root / s"b$j")
            case (j, num, "red") => Rx(num).updates(scene.root / s"r$j")
            case (j, num, "green") => Rx(num).updates(scene.root / s"z$j")
          }
      })
    }

    def updateLastWin(): Unit = {
      val lastWin = lastWinNumber
        .map(_.getOrElse("")).now
      blacks.contains(lastWin) match {
        case true => lastWinNumber.map(_.getOrElse(""))
          .updatesWithColor(scene.root / "lastWinNumber", new Color(0x000000ff))
        case false => {
          reds.contains(lastWin) match {
            case true => lastWinNumber.map(_.getOrElse(""))
              .updatesWithColor(scene.root / "lastWinNumber", new Color(0xc70000ff))
            case false => lastWinNumber.map(_.getOrElse(""))
              .updatesWithColor(scene.root / "lastWinNumber", new Color(0x2A7302FF))
          }

        }
      }
    }

    def updateOtherInfo(): Unit = {
      maxHistoryInfo.updates(scene.root / "maxWinHistory")
    }

    def updateMainScene(): Unit = {



    }

    updateMainScene()

    reader.foreach {
      case Win(num) => {
        state() = (num +: state.now).take(100)
        updateLastWin()
        updateHistoryFields()
        updateOddEvenFields()
        updateRedBlackFields()
        updateNumberFields()
        updateOtherInfo()
      }
      case _ =>
    }
  }
}

object BillboardScene {
  def apply(): BillboardScene = new BillboardScene()
}