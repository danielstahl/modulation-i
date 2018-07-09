package music

import music.Instruments.ARControlInstrumentBuilder.ar
import music.Instruments.LineControlInstrumentBuilder.line
import music.MelodyThree.ValueGenerator.{pattern, _}
import net.soundmining.Instrument.setupNodes
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.{makeFact, makeFmSynthesis, makeSpectrum2}
import net.soundmining.{BusGenerator, Melody, MusicPlayer}
object MelodyThree {


  /* good

  spectrum
  val car = 6
  val mod = 5

  val car = 5
  val mod = 6


  val car = 1
  val mod = 3

  val car = 3
  val mod = 1


  val car = 7
  val mod = 8

  val car = 8
  val mod = 7


  spectrum2
  val car = 24
  val mod = 10

  val car = 10
  val mod = 24

  val car = 24
  val mod = 20

  val car = 20
  val mod = 24
  */



  def effect(bus: Int)(implicit player: MusicPlayer): Unit = {

  }

  trait ValueGenerator[T] {
    def generate(): Seq[T]
  }

  object ValueGenerator {
    def pulse[T](size: Int, value: T): PulseGenerator[T] =
      PulseGenerator(size, value)

    def join[T](generators: ValueGenerator[T]*): ValueGeneratorJoiner[T] =
      ValueGeneratorJoiner(generators)

    def pattern[T](pattern: Seq[Int], gridGenerator: ValueGenerator[T])(implicit num: Numeric[T]): PatternValueGenerator[T] =
      PatternValueGenerator(pattern, gridGenerator: ValueGenerator[T])

    def pattern[T](patternGenerator: ValueGenerator[Int], gridGenerator: ValueGenerator[T])(implicit num: Numeric[T]): PatternValueGenerator[T] =
      pattern(patternGenerator.generate(), gridGenerator)

  }

  case class PulseGenerator[T](size: Int, value: T) extends ValueGenerator[T] {
    override def generate(): Seq[T] = Seq.fill(size)(value)
  }

  case class ValueGeneratorJoiner[T](generators: Seq[ValueGenerator[T]]) extends ValueGenerator[T] {
    override def generate(): Seq[T] = generators.flatMap(_.generate())
  }

  case class PatternValueGenerator[T](pattern: Seq[Int], gridGenerator: ValueGenerator[T])(implicit num: Numeric[T]) extends ValueGenerator[T] {

    def takePatternItem(patternItem: Int, grid: Seq[T]): (T, Seq[T]) = {
      val (patternValues, rest) = grid.splitAt(patternItem)
      (patternValues.sum, rest)
    }

    def generate(): Seq[T] = {
      pattern.foldLeft((Seq[T](), gridGenerator.generate())) {
        case ((result, grid), patternItem) =>
          val (patternValue, gridRest) = takePatternItem(patternItem, grid)
          (result :+ patternValue, gridRest)
      }._1
    }

  }

  def playSideBands(start: Float, times: Seq[Float], freq: Float, attacks: Seq[Float], pans: Seq[(Float, Float)], amps: Seq[Float])(implicit play: Player, player: MusicPlayer): Unit = {
    val startTimes = Melody.absolute(start, times)

    (startTimes zip times zip attacks zip pans zip amps).foreach {
      case ((((startTime, time), attack), (panstart, panend)), amp) =>
        play(startTime, time)
          .sine(freq = freq, attack = attack, amp = amp)
          .pan(panstart, panend)
          .send()
    }
  }

  def melodyThree(start: Float)(implicit player: MusicPlayer): Unit = {
    val c0 = noteToHertz('c0)
    val c1 = noteToHertz('c1)
    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)

    val spectrum = makeSpectrum2(c0, naturalFact, 25)
    val spectrum2 = makeSpectrum2(noteToHertz('c4), naturalFact, 25)
    val rhythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)

    implicit val play: Player = Player()

    effect(play.effectBus)

    val times = Seq(
      rhythmSpectrum(3) * 22,
      rhythmSpectrum(1) * 35,
      rhythmSpectrum(0) * 63,
      rhythmSpectrum(3) * 40,
      rhythmSpectrum(1) * 40,
      rhythmSpectrum.head * 70,
      rhythmSpectrum(3) * 40)


    println(s"times $times")

    val startTimes = Melody.absolute(start, times)
    println(s"startTimes $startTimes")


    melodyThreePart1(start = startTimes.head, spectrum, rhythmSpectrum)
    melodyThreePart2(start = startTimes(1), spectrum, rhythmSpectrum)
    melodyThreePart3(start = startTimes(2), spectrum, rhythmSpectrum)
    melodyThreePart4(start = startTimes(3), spectrum, rhythmSpectrum)
    melodyThreePart5(start = startTimes(4), spectrum, rhythmSpectrum)
    melodyThreePart6(start = startTimes(5), spectrum, rhythmSpectrum)
    melodyThreePart7(start = startTimes(6), spectrum, rhythmSpectrum)

  }

  def melodyThreePart1(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val dur = rhythmSpectrum(2)

    val car = 1
    val mod = 3

    val spec = spectrum

    val grid = pulse(40, rhythmSpectrum(3))

    //22
    val times = pattern(Seq(1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 2, 2, 3), grid).generate()
    val startTimes = Melody.absolute(0f, times)

    startTimes.foreach(time =>
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spec(car), spec(car)),
          modFreqControl = line(dur, spec(mod), spec(mod)),
          modIndexControl = ar(dur, 0.66f, (5, 7, 6)),
          attack = 0.33f,
          amp = 0.5f)
        .pan(0, 0)
        .send())

    /*
    startTimes.foreach(time =>
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spec(0), spec(0)),
          modFreqControl = line(dur, spec(0), spec(0)),
          modIndexControl = ar(dur, 0.66f, (5, 7, 6)),
          attack = 0.33f,
          amp = 0.4f)
        .pan(-0.2f, 0.2f)
        .send())
*/
    val sidebands = makeFmSynthesis(spec(car), spec(mod), 30)

    playSideBands(start = 0.0f,
      times = pattern(Seq(22), grid).generate(),
      freq = sidebands(1)._1,
      pans = Seq((-0.1f, -0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))

    playSideBands(start = 0.0f,
      times = pattern(Seq(22), grid).generate(),
      freq = sidebands(1)._2,
      pans = Seq((0.1f, 0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))


    playSideBands(
      start = 0.0f,
      times = pattern(Seq(13, 8), grid).generate(),
      freq = sidebands(3)._1,
      pans = Seq((0.2f, -0.6f), (-0.2f, 0.6f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.1f, 0.1f))

    playSideBands(
      start = 0.0f,
      times = pattern(Seq(13, 8), grid).generate(),
      freq = sidebands(3)._2,
      pans = Seq((0.3f, -0.7f), (-0.3f, 0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.1f, 0.1f))


    playSideBands(
      start = 0.0f,
      times = pattern(Seq(8, 13), grid).generate(),
      freq = sidebands(5)._1,
      pans = Seq((-0.2f, 0.6f), (0.2f, -0.6f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.05f, 0.05f))

    playSideBands(
      start = 0.0f,
      times = pattern(Seq(8, 13), grid).generate(),
      freq = sidebands(5)._2,
      pans = Seq((-0.1f, 0.7f), (0.1f, -0.7f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.05f, 0.05f))


    playSideBands(
      start = 0.0f,
      times = pattern(Seq(7, 7, 7), grid).generate(),
      freq = sidebands(7)._1,
      pans = Seq((-0.9f, -0.6f), (-0.6f, -0.3f), (-0.3f, 0.3f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.03f, 0.03f, 0.03f))

    playSideBands(
      start = 0.0f,
      times = pattern(Seq(7, 7, 7), grid).generate(),
      freq = sidebands(7)._2,
      pans = Seq((-0.8f, -0.5f), (-0.5f, -0.2f), (-0.2f, 0.2f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.03f, 0.03f, 0.03f))


    playSideBands(
      start = 0.0f,
      times = pattern(Seq(4, 3, 3, 4, 4, 3), grid).generate(),
      freq = sidebands(9)._1,
      pans = Seq((0.9f, 0.3f), (0.4f, -0.2f), (-0.1f, -0.4f), (-0.3f, -0.6f), (-0.7f, -0.4f), (-0.3f, 0.0f)),
      attacks = Seq(0.33f, 0.5f, 0.66f, 0.33f, 0.5f, 0.66f),
      amps = Seq(0.02f, 0.02f, 0.02f, 0.02f, 0.02f, 0.02f))

    playSideBands(
      start = 0.0f,
      times = pattern(Seq(4, 3, 3, 4, 4, 3), grid).generate(),
      freq = sidebands(9)._2,
      pans = Seq((0.8f, 0.2f), (0.5f, -0.1f), (-0.2f, -0.5f), (-0.2f, -0.7f), (-0.8f, -0.3f), (-0.4f, 0.0f)),
      attacks = Seq(0.33f, 0.5f, 0.66f, 0.33f, 0.5f, 0.66f),
      amps = Seq(0.02f, 0.02f, 0.02f, 0.02f, 0.02f, 0.02f))
  }

  def melodyThreePart2(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val grid = pulse(40, rhythmSpectrum(1))

    val dur = rhythmSpectrum(1)

    // 35
    val times = pattern(Seq(3, 3, 2, 2, 3, 1, 2, 1, 3, 3, 2, 3, 2, 2, 3), grid).generate()
    val startTimes = Melody.absolute(start, times)

    val car = 6
    val mod = 5

    startTimes.foreach(time => {
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spectrum(car), spectrum(car)),
          modFreqControl = line(dur, spectrum(mod), spectrum(mod)),
          modIndexControl = ar(dur, 0.66f, (5, 7, 6)),
          attack = 0.33f,
          amp = 0.5f)
        .pan(-0.1f, -0.1f)
        .send()

      play(time, rhythmSpectrum(2))
        .fmControl(
          carFreqControl = line(dur, spectrum(5), spectrum(5)),
          modFreqControl = line(dur, spectrum(6), spectrum(6)),
          modIndexControl = ar(dur, 0.33f, (5, 7, 6)),
          attack = 0.66f,
          amp = 0.1f)
        .pan(0.1f, 0.1f)
        .send()
    })

    val sidebands = makeFmSynthesis(spectrum(car), spectrum(mod), 30)
    val sidebands2 = makeFmSynthesis(spectrum(mod), spectrum(car), 30)

    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands(0)._1,
      pans = Seq((-0.1f, -0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))

    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands2(0)._1,
      pans = Seq((0.1f, 0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))


    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands(2)._1,
      pans = Seq((-0.9f, -0.9f)),
      attacks = Seq(0.33f),
      amps = Seq(0.1f))

    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands(2)._2,
      pans = Seq((-0.8f, -0.8f)),
      attacks = Seq(0.33f),
      amps = Seq(0.1f))

    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands2(2)._1,
      pans = Seq((0.9f, 0.9f)),
      attacks = Seq(0.66f),
      amps = Seq(0.1f))

    playSideBands(start = start,
      times = pattern(Seq(35), grid).generate(),
      freq = sidebands2(2)._2,
      pans = Seq((0.8f, 0.8f)),
      attacks = Seq(0.66f),
      amps = Seq(0.1f))


    playSideBands(
      start = start,
      times = pattern(Seq(23, 12), grid).generate(),
      freq = sidebands(5)._1,
      pans = Seq((0.2f, -0.6f), (-0.2f, 0.6f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.05f, 0.05f))

    playSideBands(
      start = start,
      times = pattern(Seq(12, 23), grid).generate(),
      freq = sidebands(5)._2,
      pans = Seq((0.3f, -0.7f), (-0.3f, 0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.05f, 0.05f))


    playSideBands(
      start = start,
      times = pattern(Seq(23, 12), grid).generate(),
      freq = sidebands2(9)._1,
      pans = Seq((0.1f, 0.9f), (0.9f, 0.1f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.03f, 0.03f))

    playSideBands(
      start = start,
      times = pattern(Seq(12, 23), grid).generate(),
      freq = sidebands2(9)._2,
      pans = Seq((-0.1f, -0.9f), (-0.9f, 0.1f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.03f, 0.03f))
  }

  def melodyThreePart3(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val grid = pulse(70, rhythmSpectrum(0))

    val dur = rhythmSpectrum(0)

    // 63
    val times = pattern(Seq(8, 5, 13, 8, 5, 3, 8, 13), grid).generate()

    val startTimes = Melody.absolute(start, times)

    val car = 7
    val mod = 8

    startTimes.foreach(time => {
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spectrum(car), spectrum(car)),
          modFreqControl = line(dur, spectrum(mod), spectrum(mod)),
          modIndexControl = ar(dur, 0.66f, (5, 7, 6)),
          attack = 0.33f,
          amp = 0.5f)
        .pan(-0.1f, -0.1f)
        .send()
    })

    val times2 = pattern(Seq(1, 8, 5, 13, 8, 5, 3, 8, 13), grid).generate()

    val startTimes2 = Melody.absolute(start, times2).drop(1)

    startTimes2.foreach(time => {
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spectrum(8), spectrum(8)),
          modFreqControl = line(dur, spectrum(7), spectrum(7)),
          modIndexControl = ar(dur, 0.33f, (6, 5, 7)),
          attack = 0.33f,
          amp = 0.1f)
        .pan(0.1f, 0.1f)
        .send()
    })

    val sidebands = makeFmSynthesis(spectrum(car), spectrum(mod), 30)
    val sidebands2 = makeFmSynthesis(spectrum(mod), spectrum(car), 30)

    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands(0)._1,
      pans = Seq((-0.7f, -0.9f)),
      attacks = Seq(0.5f),
      amps = Seq(0.1f))

    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands2(0)._1,
      pans = Seq((0.7f, 0.9f)),
      attacks = Seq(0.5f),
      amps = Seq(0.05f))


    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands(3)._1,
      pans = Seq((-0.9f, -0.6f)),
      attacks = Seq(0.33f),
      amps = Seq(0.05f))

    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands(3)._2,
      pans = Seq((-0.7f, -0.8f)),
      attacks = Seq(0.33f),
      amps = Seq(0.05f))

    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands2(2)._1,
      pans = Seq((0.9f, 0.6f)),
      attacks = Seq(0.66f),
      amps = Seq(0.03f))

    playSideBands(start = start,
      times = pattern(Seq(63), grid).generate(),
      freq = sidebands2(2)._2,
      pans = Seq((0.7f, 0.8f)),
      attacks = Seq(0.66f),
      amps = Seq(0.03f))


    playSideBands(
      start = start,
      times = pattern(Seq(31, 32), grid).generate(),
      freq = sidebands(5)._1,
      pans = Seq((0.9f, 0.6f), (0.5f, 0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.04f, 0.04f))

    playSideBands(
      start = start,
      times = pattern(Seq(32, 31), grid).generate(),
      freq = sidebands(5)._2,
      pans = Seq((0.7f, 0.3f), (0.2f, 0.8f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.04f, 0.04f))


    playSideBands(
      start = start,
      times = pattern(Seq(31, 32), grid).generate(),
      freq = sidebands2(5)._1,
      pans = Seq((-0.9f, -0.6f), (-0.5f, -0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.02f, 0.02f))

    playSideBands(
      start = start,
      times = pattern(Seq(32, 31), grid).generate(),
      freq = sidebands2(5)._2,
      pans = Seq((-0.7f, -0.3f), (-0.2f, -0.8f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.02f, 0.02f))


    playSideBands(
      start = start,
      times = pattern(Seq(21, 21, 21), grid).generate(),
      freq = sidebands(7)._1,
      pans = Seq((0.6f, 0.2f), (0.1f, 0.7f), (0.6f, 0.2f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.03f, 0.03f, 0.03f))

    playSideBands(
      start = start,
      times = pattern(Seq(21, 21, 21), grid).generate(),
      freq = sidebands(7)._2,
      pans = Seq((0.5f, 0.3f), (0.2f, 0.8f), (0.5f, 0.3f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.03f, 0.04f, 0.03f))


    playSideBands(
      start = start,
      times = pattern(Seq(21, 21, 21), grid).generate(),
      freq = sidebands2(7)._1,
      pans = Seq((-0.6f, -0.2f), (-0.1f, -0.7f), (-0.6f, -0.2f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.01f, 0.01f, 0.01f))

    playSideBands(
      start = start,
      times = pattern(Seq(21, 21, 21), grid).generate(),
      freq = sidebands2(7)._2,
      pans = Seq((-0.5f, -0.3f), (-0.2f, -0.8f), (-0.5f, -0.3f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.01f, 0.01f, 0.01f))

  }

  def melodyThreePart4(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val dur = rhythmSpectrum(2)

    val car = 3
    val mod = 1

    val spec = spectrum

    val grid = pulse(40, rhythmSpectrum(3))

    val times = pattern(Seq(1, 1, 5, 1, 5, 3, 2, 5, 2, 2, 5, 5), grid).generate()

    val startTimes = Melody.absolute(start, times)

    startTimes.foreach(time =>
      play(time, dur)
        .fmControl(
          carFreqControl = line(dur, spec(car), spec(car)),
          modFreqControl = line(dur, spec(mod), spec(mod)),
          modIndexControl = ar(dur, 0.66f, (5, 7, 6)),
          attack = 0.33f,
          amp = 0.5f)
        .pan(0, 0)
        .send())

    val sidebands = makeFmSynthesis(spec(car), spec(mod), 30)

    playSideBands(start = start,
      times = pattern(Seq(37), grid).generate(),
      freq = sidebands(1)._1,
      pans = Seq((-0.1f, -0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))

    playSideBands(start = start,
      times = pattern(Seq(37), grid).generate(),
      freq = sidebands(1)._2,
      pans = Seq((0.1f, 0.1f)),
      attacks = Seq(0.5f),
      amps = Seq(0.2f))


    playSideBands(
      start = start,
      times = pattern(Seq(23, 15), grid).generate(),
      freq = sidebands(3)._1,
      pans = Seq((0.2f, -0.6f), (-0.2f, 0.6f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.1f, 0.1f))

    playSideBands(
      start = start,
      times = pattern(Seq(23, 15), grid).generate(),
      freq = sidebands(3)._2,
      pans = Seq((0.3f, -0.7f), (-0.3f, 0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.1f, 0.1f))


    playSideBands(
      start = start,
      times = pattern(Seq(15, 23), grid).generate(),
      freq = sidebands(5)._1,
      pans = Seq((-0.2f, 0.6f), (0.2f, -0.6f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.05f, 0.05f))

    playSideBands(
      start = start,
      times = pattern(Seq(15, 23), grid).generate(),
      freq = sidebands(5)._2,
      pans = Seq((-0.1f, 0.7f), (0.1f, -0.7f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.05f, 0.05f))


    playSideBands(
      start = start,
      times = pattern(Seq(13, 13, 13), grid).generate(),
      freq = sidebands(7)._1,
      pans = Seq((-0.9f, -0.6f), (-0.6f, -0.3f), (-0.3f, 0.3f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.03f, 0.03f, 0.03f))

    playSideBands(
      start = start,
      times = pattern(Seq(13, 13, 13), grid).generate(),
      freq = sidebands(7)._2,
      pans = Seq((-0.8f, -0.5f), (-0.5f, -0.2f), (-0.2f, 0.2f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.03f, 0.03f, 0.03f))


    playSideBands(
      start = start,
      times = pattern(Seq(6, 6, 6, 7, 6, 6), grid).generate(),
      freq = sidebands(9)._1,
      pans = Seq((0.9f, 0.3f), (0.4f, -0.2f), (-0.1f, -0.4f), (-0.3f, -0.6f), (-0.7f, -0.4f), (-0.3f, 0.0f)),
      attacks = Seq(0.33f, 0.5f, 0.66f, 0.33f, 0.5f, 0.66f),
      amps = Seq(0.02f, 0.02f, 0.02f, 0.02f, 0.02f, 0.02f))

    playSideBands(
      start = start,
      times = pattern(Seq(6, 6, 6, 7, 6, 6), grid).generate(),
      freq = sidebands(9)._2,
      pans = Seq((0.8f, 0.2f), (0.5f, -0.1f), (-0.2f, -0.5f), (-0.2f, -0.7f), (-0.8f, -0.3f), (-0.4f, 0.0f)),
      attacks = Seq(0.33f, 0.5f, 0.66f, 0.33f, 0.5f, 0.66f),
      amps = Seq(0.02f, 0.02f, 0.02f, 0.02f, 0.02f, 0.02f))
  }

  def melodyThreePart5(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {

    //1, 1, 2, 3, 5, 8, 13, 21, 34, 55
    // based on part2 sidebands 1
    val grid = pulse(40, rhythmSpectrum(1))

    val dur = rhythmSpectrum(1)

    val car = 6
    val mod = 5

    val sidebands = makeFmSynthesis(spectrum(car), spectrum(mod), 30)

    playSideBands(start = start,
      times = pattern(Seq(34), grid).generate(),
      freq = sidebands(1)._1,
      pans = Seq((0.0f, 0.0f)),
      attacks = Seq(0.5f),
      amps = Seq(0.5f))


    playSideBands(start = start,
      times = pattern(Seq(21, 13), grid).generate(),
      freq = sidebands(3)._1,
      pans = Seq((0.3f, 0.3f), (0.3f, 0.3f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.3f, 0.3f))

    playSideBands(start = start,
      times = pattern(Seq(13, 21), grid).generate(),
      freq = sidebands(3)._2,
      pans = Seq((-0.3f, -0.3f), (-0.3f, -0.3f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.3f, 0.3f))


    playSideBands(start = start,
      times = pattern(Seq(13, 13, 13), grid).generate(),
      freq = sidebands(6)._1,
      pans = Seq((0.6f, 0.6f), (0.6f, 0.6f), (0.6f, 0.6f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    playSideBands(start = start,
      times = pattern(Seq(13, 13, 13), grid).generate(),
      freq = sidebands(6)._2,
      pans = Seq((-0.6f, -0.6f), (-0.6f, -0.6f), (-0.6f, -0.6f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))



    playSideBands(start = start,
      times = pattern(Seq(8, 8, 8, 8, 8), grid).generate(),
      freq = sidebands(10)._1,
      pans = Seq((0.8f, 0.8f), (0.8f, 0.8f), (0.8f, 0.8f), (0.8f, 0.8f), (0.8f, 0.8f)),
      attacks = Seq(0.5f, 0.33f, 0.5f, 0.66f, 0.5f),
      amps = Seq(0.1f, 0.1f, 0.1f, 0.1f, 0.1f))

    playSideBands(start = start,
      times = pattern(Seq(8, 8, 8, 8, 8), grid).generate(),
      freq = sidebands(10)._2,
      pans = Seq((-0.8f, -0.8f), (-0.8f, -0.8f), (-0.8f, -0.8f), (-0.8f, -0.8f), (-0.8f, -0.8f)),
      attacks = Seq(0.5f, 0.33f, 0.5f, 0.66f, 0.5f),
      amps = Seq(0.1f, 0.1f, 0.1f, 0.1f, 0.1f))


    playSideBands(start = start,
      times = pattern(Seq(5, 5, 5, 5, 5, 5, 5, 5), grid).generate(),
      freq = sidebands(15)._1,
      pans = Seq((0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f), (0.9f, 0.9f)),
      attacks = Seq(0.66f, 0.5f, 0.33f, 0.5f, 0.66f, 0.5f, 0.33f, 0.5f),
      amps = Seq(0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f))

    playSideBands(start = start,
      times = pattern(Seq(5, 5, 5, 5, 5, 5, 5, 5), grid).generate(),
      freq = sidebands(15)._2,
      pans = Seq((-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f), (-0.9f, -0.9f)),
      attacks = Seq(0.66f, 0.5f, 0.33f, 0.5f, 0.66f, 0.5f, 0.33f, 0.5f),
      amps = Seq(0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f, 0.05f))
  }

  def melodyThreePart6(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val grid = pulse(70, rhythmSpectrum(0))

    val car = 7
    val mod = 8

    val sidebands = makeFmSynthesis(spectrum(car), spectrum(mod), 30)

    playSideBands(start = start,
      times = pattern(Seq(76), grid).generate(),
      freq = sidebands(1)._1,
      pans = Seq((0.0f, 0.0f)),
      attacks = Seq(0.5f),
      amps = Seq(0.5f))


    playSideBands(start = start,
      times = pattern(Seq(29, 47), grid).generate(),
      freq = sidebands(2)._1,
      pans = Seq((0.7f, 0.7f), (0.7f, 0.7f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.3f, 0.3f))

    playSideBands(start = start,
      times = pattern(Seq(47, 29), grid).generate(),
      freq = sidebands(3)._2,
      pans = Seq((-0.7f, -0.7f), (-0.7f, -0.7f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.3f, 0.3f))



    playSideBands(start = start,
      times = pattern(Seq(18, 29, 18), grid).generate(),
      freq = sidebands(5)._2,
      pans = Seq((0.5f, 0.5f), (0.5f, 0.5f), (0.5f, 0.5f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    playSideBands(start = start,
      times = pattern(Seq(29, 18, 18), grid).generate(),
      freq = sidebands(7)._1,
      pans = Seq((-0.1f, 0.1f), (0.1f, -0.1f), (-0.1f, 0.1f)),
      attacks = Seq(0.5f, 0.66f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    playSideBands(start = start,
      times = pattern(Seq(18, 18, 29), grid).generate(),
      freq = sidebands(9)._2,
      pans = Seq((-0.5f, -0.5f), (-0.5f, -0.5f), (-0.5f, -0.5f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    // 18 29 18
  }

  def melodyThreePart7(start: Float, spectrum: Seq[Float], rhythmSpectrum: Seq[Float])(implicit player: MusicPlayer, play: Player): Unit = {
    val grid = pulse(40, rhythmSpectrum(3))

    val car = 1
    val mod = 3

    val sidebands = makeFmSynthesis(spectrum(car), spectrum(mod), 30)

    playSideBands(start = start,
      times = pattern(Seq(29), grid).generate(),
      freq = sidebands(1)._1,
      pans = Seq((0.0f, 0.0f)),
      attacks = Seq(0.5f),
      amps = Seq(0.5f))


    playSideBands(start = start,
      times = pattern(Seq(11, 18), grid).generate(),
      freq = sidebands(3)._1,
      pans = Seq((-0.2f, -0.2f), (-0.2f, -0.2f)),
      attacks = Seq(0.33f, 0.66f),
      amps = Seq(0.3f, 0.3f))

    playSideBands(start = start,
      times = pattern(Seq(18, 11), grid).generate(),
      freq = sidebands(5)._1,
      pans = Seq((0.2f, 0.2f), (0.2f, 0.2f)),
      attacks = Seq(0.66f, 0.33f),
      amps = Seq(0.3f, 0.3f))



    playSideBands(start = start,
      times = pattern(Seq(11, 18, 11), grid).generate(),
      freq = sidebands(7)._1,
      pans = Seq((0.5f, 0.7f), (0.7f, 0.9f), (0.9f, 0.6f)),
      attacks = Seq(0.66f, 0.5f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    playSideBands(start = start,
      times = pattern(Seq(18, 11, 11), grid).generate(),
      freq = sidebands(9)._1,
      pans = Seq((-0.1f, 0.1f), (0.1f, -0.1f), (-0.1f, 0.1f)),
      attacks = Seq(0.5f, 0.66f, 0.33f),
      amps = Seq(0.2f, 0.2f, 0.2f))

    playSideBands(start = start,
      times = pattern(Seq(11, 11, 18), grid).generate(),
      freq = sidebands(11)._1,
      pans = Seq((-0.5f, -0.7f), (-0.7f, -0.9f), (-0.9f, -0.6f)),
      attacks = Seq(0.33f, 0.5f, 0.66f),
      amps = Seq(0.2f, 0.2f, 0.2f))
  }

  def main(args: Array[String]): Unit = {
    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)
    val rhythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)

    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    melodyThree(0)

  }
}
