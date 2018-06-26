package music

import net.soundmining.Instrument.{EFFECT, TAIL_ACTION, setupNodes}
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.{BusGenerator, MusicPlayer}
import music.Instruments.ARControlInstrumentBuilder.ar
import music.Instruments.LineControlInstrumentBuilder.line
import music.Instruments.{MonoAllpassReplaceInstrumentBuilder, monoVolumeInstrument, panInstrument}
import music.Instruments.SineControlReplaceInstrumentBuilder.sine
import net.soundmining.Utils.absoluteTimeToMillis


object MelodyTwo {

  def melodyTwo(start: Float)(implicit player: MusicPlayer): Unit = {
    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)

    val spectrum = makeSpectrum2(c2, naturalFact, 25)
    val rhythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)

    // Spectrum: Vector(65.4064, 130.8128, 196.21921, 261.6256, 327.032, 392.43842, 457.84482, 523.2512, 588.6576, 654.064, 719.47046, 784.87683, 850.2832, 915.68964, 981.09607, 1046.5024, 1111.9088, 1177.3152, 1242.7217, 1308.128, 1373.5344, 1438.9409, 1504.3473, 1569.7537, 1635.16)
    // Rhythm Spectrum: Vector(0.5109875, 1.021975, 1.5329626, 2.04395, 2.5549376, 3.0659251, 3.5769126, 4.0879, 4.5988874, 5.109875, 5.620863, 6.1318502, 6.6428375, 7.1538253, 7.664813, 8.1758, 8.686788, 9.197775, 9.708763, 10.21975, 10.730738, 11.241726, 11.752713, 12.2637005, 12.774688)

    def printSpectrum(spec: Seq[Float], name: String): Unit = {
      println(s"spectrum $spectrum")
      spec.zipWithIndex.foreach {
        case (e, i) => println(s"$i\t$e")
      }
    }

    println(s"Spectrum: ${spectrum.zipWithIndex}")
    println(s"Rhythm Spectrum: ${rhythmSpectrum.zipWithIndex}")
    val play = Player()

    effect(play.effectBus)

    val dur = rhythmSpectrum(24) * 3

    val dur2 = rhythmSpectrum(24) * 4

    val start2 = rhythmSpectrum(24) * 2

    play(0, dur)
      .fmControl(
        carFreqControl = line(dur, spectrum(2), spectrum(1)),
        modFreqControl = line(dur, spectrum(3), spectrum(3)),
        modIndexControl = ar(dur, 0.66f, (5, 2, 10)),
        0.66f)
      .pan(-0.3f, 1.0f)
      .send()

    play(start2, dur2)
      .fmControl(
        carFreqControl = line(dur2, spectrum(9), spectrum(9)),
        modFreqControl = line(dur2, spectrum(11), spectrum(10)),
        modIndexControl = ar(dur2, 0.33f, (8, 3, 13)),
        0.66f)
      .pan(0.3f, -0.6f)
      .send()

    play(start2 + dur, dur2)
      .sine(spectrum(12), 0.3f)
      .pan(-0.3f, 0.6f)
      .send()

    play(start2 + dur + dur, dur2)
      .sine(spectrum(8), 0.6f)
      .pan(1.0f, 0.3f)
      .send()

    play(start2 + dur + dur + dur, dur2)
      .sine(spectrum(9), 0.3f)
      .pan(-1.0f, -0.3f)
      .send()
  }

  def effect(bus: Int)(implicit player: MusicPlayer): Unit = {
    val tempBus = BusGenerator.nextAudio()

    val dur = 240

    val volume = monoVolumeInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(tempBus)
      .ampBus.control(line(dur, 0.2f, 0.3f))
      .buildInstruments()

    val allpassFilter = new MonoAllpassReplaceInstrumentBuilder()
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .in(tempBus)
      .dur(dur)
      .decayTimeBus.control(line(dur, 8f, 10f, nodeId = EFFECT), sine(dur, 0.01f, 0.01f, 0.01f, 0.01f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.6f, 0.3f, nodeId = EFFECT))
      .maxDelay(0.6f)
      .buildInstruments()

    val pan1 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(tempBus)
      .out(0)
      .panBus.control(line(dur, 1f, -1f, EFFECT))
      .buildInstruments()

    val pan2 = panInstrument
      .nodeId(EFFECT)
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(tempBus)
      .out(0)
      .panBus.control(line(dur, -1f, 1f, EFFECT))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(0), volume ++ allpassFilter ++ pan1 ++ pan2)
  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    melodyTwo(0.0f)
  }
}
