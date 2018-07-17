package music

import music.Instruments.ARControlInstrumentBuilder.ar
import music.Instruments.LineControlInstrumentBuilder.line
import music.Instruments.SineControlInstrumentBuilder._
import music.Instruments._
import net.soundmining.Instrument.{SOURCE, TAIL_ACTION}
import net.soundmining.{BusGenerator, ControlInstrumentBuilder, MusicPlayer}
import net.soundmining.Utils.absoluteTimeToMillis

case class Player(start: Float, dur: Float, effectBus: Int, bus: Int = BusGenerator.nextAudio()) {
  var instrumentBundle: Seq[Seq[Object]] = Seq()
  var panBundle: Seq[Seq[Object]] = Seq()

  def sine(freq: Float, attack: Float, amp: Float = 0.2f): Player = {
    instrumentBundle = new SineInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, freq, freq))
      .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
      .buildInstruments()
    this
  }

  def sineMod(freq: Float, attack: Float, freqBus: Int, amp: Float = 0.2f): Player = {
    val freqMod = new LineControlInstrumentBuilder()
      .out(freqBus)
      .control(freq, freq)
      .dur(dur)
      .nodeId(SOURCE)
      .buildInstruments()

    instrumentBundle = freqMod ++
      new SineInstrumentBuilder()
        .addAction(TAIL_ACTION)
        .out(bus)
        .dur(dur)
        .freqBus.bus(freqBus)
        .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
        .buildInstruments()
    this
  }

  def sineSineAmp(freq: Float, amp: (Float, Float), ampSpeed: (Float, Float)): Player = {
    instrumentBundle = new SineInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, freq, freq))
      .ampBus.control(sin(dur, ampSpeed._1/dur/2, ampSpeed._2/dur/2, mulStart = amp._1, mulEnd = amp._2))
      .buildInstruments()
    this
  }

  def pan(startPan: Float, endPan: Float): Player = {
    panBundle = new PanInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(0)
      .panBus.control(line(dur, startPan, endPan))
      .buildInstruments()
    this
  }

  def send()(implicit player: MusicPlayer): Unit = {

    val effect = monoVolumeInstrument
      .addAction(TAIL_ACTION)
      .dur(dur)
      .in(bus)
      .out(effectBus)
      .ampBus.control(line(dur, 1, 1))
      .buildInstruments()

    player.sendNew(absoluteTimeToMillis(start), instrumentBundle ++ panBundle ++ effect)
  }

  def fmControl(carFreqControl: ControlInstrumentBuilder, modFreqControl: ControlInstrumentBuilder, modIndexControl: ControlInstrumentBuilder, attack: Float, amp: Float = 0.2f): Player = {
    instrumentBundle = new FmInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .carFreqBus.control(carFreqControl)
      .modFreqBus.control(modFreqControl)
      .modIndexBus.control(modIndexControl)
      .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
      .buildInstruments()
    this
  }

  def fm(carFreq: Float, modFreq: Float, indexAttack: Float, index: (Float, Float, Float), attack: Float, amp: Float = 0.2f): Player = {
    fmControl(line(dur, carFreq, carFreq), line(dur, modFreq, modFreq), ar(dur, indexAttack, index), attack, amp)
  }

  def fmMod(carFreq: Float, modBus: Int, indexAttack: Float, index: (Float, Float, Float), attack: Float, amp: Float = 0.2f): Player = {
    instrumentBundle = new FmInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .carFreqBus.control(line(dur, carFreq, carFreq))
      .modFreqBus.bus(modBus)
      .modIndexBus.control(ar(dur, indexAttack, index))
      .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
      .buildInstruments()
    this
  }

  def pulse(freq: Float, width: (Float, Float), attack: Float, amp: Float = 0.2f)(implicit player: MusicPlayer): Player = {
    instrumentBundle = new  PulseInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .freqBus.control(line(dur, freq, freq))
      .widthBus.control(line(dur, width._1, width._2))
      .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
      .buildInstruments()
    this
  }

  def subtractiveChord(attack: Float, freqs: Seq[(Float, Float)], amp: Float = 0.2f): Player = {
    val noise = new WhiteNoiseInstrumentBuilder()
      .addAction(TAIL_ACTION)
      .out(bus)
      .dur(dur)
      .ampBus.control(ar(dur, attack, (0f, amp, 0f)))
      .buildInstruments()

    val filters = freqs.flatMap {
      case (freq, attackPoint) =>
        new FilterReplaceInstrumentBuilder()
          .addAction(TAIL_ACTION)
          .in(bus)
          .dur(dur)
          .ampBus.control(ar(dur, attackPoint, (0f, amp, 0f)))
          .bwBus.control(line(dur, 0.00000001f, 0.000000001f))
          .freqBus.control(line(dur, freq, freq))
          .buildInstruments()
    }

    val volume = new MonoVolumeReplaceBuilder()
      .addAction(TAIL_ACTION)
      .in(bus)
      .dur(dur)
      .ampBus.control(line(dur, 1f, 1f))
      .buildInstruments()

    instrumentBundle = noise ++ filters ++ volume
    this
  }
}