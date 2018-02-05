package music

import net.soundmining.{BusGenerator, Melody, MusicPlayer}
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.Instrument.{EFFECT, TAIL_ACTION, setupNodes}
import music.Instruments._
import music.Instruments.ARControlInstrumentBuilder.ar
import music.Instruments.LineControlInstrumentBuilder.line
import music.Instruments.SineControlReplaceInstrumentBuilder._
import net.soundmining.Utils.absoluteTimeToMillis

object MelodyOne {

  def effect(bus: Int)(implicit player: MusicPlayer): Unit = {
    val tempBus = BusGenerator.nextAudio()

    val dur = 120

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
      .decayTimeBus.control(line(dur, 8f, 10f, nodeId = EFFECT), sine(dur, 0.1f, 0.1f, 0.1f, 0.1f, nodeId = EFFECT))
      .delayBus.control(line(dur, 0.3f, 0.6f, nodeId = EFFECT))
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

  def melodyOne(start: Float)(implicit player: MusicPlayer): Unit = {
    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)

    val spectrum = makeSpectrum2(c2, naturalFact, 25)
    val rhythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)
    val lowSpectrum = makeSpectrum2(noteToHertz('c0), naturalFact, 25)

    println(s"spectrum $spectrum")
    println(s"rhythm spectrum $rhythmSpectrum")
    println(s"low spectrum $lowSpectrum")

    val times = Melody.absolute(start, Seq(
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4), rhythmSpectrum(4), rhythmSpectrum(6),
      rhythmSpectrum(4)
    )).iterator

    val times2 = Melody.absolute(start + rhythmSpectrum(9), Seq(
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15)
    )).iterator

    val durations = Seq(
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3), rhythmSpectrum(3), rhythmSpectrum(3),
      rhythmSpectrum(3)
    ).iterator

    val durations2 = Seq(
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15),
      rhythmSpectrum(15), rhythmSpectrum(15), rhythmSpectrum(15)
    ).iterator

    val pitches = Seq(
      spectrum(12), spectrum(8), spectrum(9),
      spectrum(7), spectrum(6), spectrum(10),
      spectrum(13), spectrum(9), spectrum(7),
      spectrum(6), spectrum(8),
      // Transpose
      spectrum(15), spectrum(10), spectrum(12),
      spectrum(9), spectrum(8), spectrum(13),
      spectrum(15), spectrum(12), spectrum(9),
      spectrum(8), spectrum(10)

    ).iterator


    val modulatorBus = BusGenerator.nextControl()

    val play = Player()
    effect(play.effectBus)

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.7f, modulatorBus).pan(0f, 0f).send()
    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(0.5f, 0.5f).send()

    play(times.next(), durations.next()).sineMod(pitches.next(), 0.3f, modulatorBus).pan(-0.5f, -0.5f).send()

    val lowPitches = Seq(
      lowSpectrum(4), lowSpectrum(5), lowSpectrum(6),
      lowSpectrum(3), lowSpectrum(4), lowSpectrum(5),
      lowSpectrum(4), lowSpectrum(7), lowSpectrum(6),
      lowSpectrum(5), lowSpectrum(3)
    ).iterator

    val carriers = Seq(
      spectrum(12), spectrum(8), spectrum(9),
      spectrum(7), spectrum(6), spectrum(10),
      spectrum(13), spectrum(9), spectrum(7),
      spectrum(6), spectrum(8)
    ).iterator

    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-0.5f, 0.5f).send()

    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-0.5f, 0.5f).send()

    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-0.5f, 0.5f).send()

    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(-1, 0).send()
    play(times2.next(), durations2.next()).fm(lowPitches.next(), carriers.next(), 0.5f, (0, 1, 0), 0.5f).pan(1, 0).send()
  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    melodyOne(0.0f)
  }
}
