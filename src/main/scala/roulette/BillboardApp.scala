package roulette

import java.util.concurrent.CountDownLatch

import device.cammegh.slingshot._
import device.io._
import display.io.WindowConfig
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import monix.reactive.Observable
import roulette.ecs.BillboardScene
import scodec.bits.ByteVector

import scala.concurrent.duration._

object BillboardApp extends App {

  implicit val scheduler: SchedulerService = Scheduler.fixedPool("usb", 4)
  val latch = new CountDownLatch(1)

  // display
  val config = WindowConfig(position = (2020, 10), dimensions = (900, 1600))
  val (scene, ui) = display.io.desktop.open(BillboardScene() -> config)

  val device = Observable.interval(2.seconds)
    .map { x => (math.random() * 37).toInt }
    .debug("number")
    .map { s => (" " + s).takeRight(2) }
    .map(s => ByteVector(s.toCharArray.map(_.toByte)).bits)
    .debug("bits")
  //              .startWith("  ")

  // device
  //  val hub = device.io.usb.hub(device.io.usb.pl2303)
  //  hub.scan.foreach {
  //    case DeviceAttached(usb) =>
  //      println(s"device attached $usb")
  //      val (_, wheel) = hub.open(usb)
  //        .pipe(device.io.reader(SlingShotDecoder))
  //        .unicast
  //      wheel.foreach(scene.onNext)
  //    case DeviceDetached(usb) =>
  //      println(s"device detached $usb")
  //  }
  //  val device = Observable.repeatEval(io.StdIn.readLine())
  //    .takeWhile(_.nonEmpty)
  //    .map(s => (if (s.length == 2) s else s + "\r\n").hex.bits)
  //    .doOnTerminate(_ => latch.countDown())
  //    .debug("<")
  //    .debug("<<")

  device.decode(Input.codec)
    .debug("protocol")
    .foreach(scene.onNext)

  ui.doOnTerminate(_ => latch.countDown()).subscribe()
  latch.await()
  scheduler.shutdown()
  scheduler.awaitTermination(2.seconds, Scheduler.global)
}
