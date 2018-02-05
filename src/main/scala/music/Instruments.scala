package music

import java.{lang => jl}

import net.soundmining.Instrument._
import net.soundmining._

/**
 * Instruments
 */
object Instruments {
  class SineControlReplaceInstrumentBuilder extends AbstractInstrumentBuilder with ControlReplaceInstrumentBuilder {
    type SelfType = SineControlReplaceInstrumentBuilder

    def self(): SelfType = this

    var startFreq: jl.Float = _
    var endFreq: jl.Float = _

    def freq(start: Float, end: Float): SelfType = {
      startFreq = buildFloat(start)
      endFreq = buildFloat(end)
      self()
    }

    var startAmp: jl.Float = _
    var endAmp: jl.Float = _

    def amp(start: Float, end: Float): SelfType = {
      startAmp = buildFloat(start)
      endAmp = buildFloat(end)
      self()
    }

    val instrumentName: String = "sineControlReplace"

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        Seq(
          "startFreq", startFreq,
          "endFreq", endFreq,
          "startAmp", startAmp,
          "endAmp", endAmp
        )
  }

  object SineControlReplaceInstrumentBuilder {
    def sine(dur: Float, startFreq: Float, endFreq: Float, startAmp: Float, endAmp: Float, nodeId: Node = SOURCE): SineControlReplaceInstrumentBuilder = {
      new SineControlReplaceInstrumentBuilder()
        .dur(dur)
        .freq(startFreq, endFreq)
        .amp(startAmp, endAmp)
        .nodeId(nodeId)
    }
  }

  class LineControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
    type SelfType = LineControlInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "lineControl"

    var startValue: jl.Float = _
    var endValue: jl.Float = _

    def control(start: Float, end: Float): SelfType = {
      startValue = buildFloat(start)
      endValue = buildFloat(end)
      self()
    }

    def reverse: LineControlInstrumentBuilder =
      LineControlInstrumentBuilder.line(dur.floatValue(), endValue.floatValue(), startValue.floatValue())

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "startValue", startValue,
          "endValue", endValue
        )
  }

  object LineControlInstrumentBuilder {
    def line(dur: Float, start: Float, end: Float, nodeId: Node = SOURCE): LineControlInstrumentBuilder = {
      new LineControlInstrumentBuilder()
        .control(start, end)
        .dur(dur)
        .nodeId(nodeId)
    }
  }

  class ASRControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
    type SelfType = ASRControlInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "asrControl"

    var attackStart: jl.Float = buildFloat(1.0f)
    var sustainStart: jl.Float = buildFloat(1.0f)
    var decayStart: jl.Float = buildFloat(1.0f)
    var decayEnd: jl.Float = buildFloat(1.0f)

    def values(attackStartValue: Float, sustainStartValue: Float, decayStartValue: Float, decayEndValue: Float): SelfType = {
      attackStart = buildFloat(attackStartValue)
      sustainStart = buildFloat(sustainStartValue)
      decayStart = buildFloat(decayStartValue)
      decayEnd = buildFloat(decayEndValue)
      self()
    }

    var attackTime: jl.Float = buildFloat(1.0f)
    var sustainTime: jl.Float = buildFloat(1.0f)
    var decayTime: jl.Float = buildFloat(1.0f)

    def times(attackTimeValue: Float, sustainTimeValue: Float, decayTimeValue: Float): SelfType = {
      attackTime = buildFloat(attackTimeValue)
      sustainTime = buildFloat(sustainTimeValue)
      decayTime = buildFloat(decayTimeValue)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "attackStart", attackStart,
          "sustainStart", sustainStart,
          "decayStart", decayStart,
          "decayEnd", decayEnd,
          "attackTime", attackTime,
          "sustainTime", sustainTime,
          "decayTime", decayTime)
  }

  object ASRControlInstrumentBuilder {
    def asr(dur: Float, values: (Float, Float, Float, Float), times: (Float, Float, Float), nodeId: Node = SOURCE): ASRControlInstrumentBuilder = {
      new ASRControlInstrumentBuilder()
        .nodeId(nodeId)
        .values(values._1, values._2, values._3, values._4)
        .times(times._1, times._2, times._3)
        .dur(dur)
    }
  }

  object ARControlInstrumentBuilder {
    def ar(dur: Float, attackTime: Float, values: (Float, Float, Float), arType: (EnvCurve, EnvCurve) = (LINEAR, LINEAR), nodeId: Node = SOURCE): ARControlInstrumentBuilder = {
      new ARControlInstrumentBuilder()
        .nodeId(nodeId)
        .values(values._1, values._2, values._3)
        .types(arType._1, arType._2)
        .attackTime(attackTime)
        .dur(dur)
    }
  }

  class ARControlInstrumentBuilder extends AbstractInstrumentBuilder with ControlInstrumentBuilder {
    type SelfType = ARControlInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "arControl"

    var attackStart: jl.Float = buildFloat(1.0f)
    var releaseStart: jl.Float = buildFloat(1.0f)
    var releaseEnd: jl.Float = buildFloat(1.0f)

    def values(attackStartValue: Float, releaseStartValue: Float, releaseEndValue: Float): SelfType = {
      attackStart = buildFloat(attackStartValue)
      releaseStart = buildFloat(releaseStartValue)
      releaseEnd = buildFloat(releaseEndValue)
      self()
    }

    var attackTime: jl.Float = buildFloat(1.0f)

    def attackTime(attackTimeValue: Float): SelfType = {
      attackTime = buildFloat(attackTimeValue)
      self()
    }

    var attackType: EnvCurve = LINEAR
    var releaseType: EnvCurve = LINEAR

    def types(attackTypeValue: EnvCurve, releaseTypeValue: EnvCurve): SelfType = {
      attackType = attackTypeValue
      releaseType = releaseTypeValue
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        Seq(
          "attackStart", attackStart,
          "releaseStart", releaseStart,
          "releaseEnd", releaseEnd,
          "attackTime", attackTime,
          "attackType", attackType.name,
          "releaseType", releaseType.name)
  }

  class FilterInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "filt"

    val ampBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterRejectInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = FilterRejectInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "filtReject"

    val ampBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterRejectInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = FilterReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "filtReplace"

    val ampBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterReplaceInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class FilterRejectReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = FilterRejectReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "filtRejectReplace"

    val ampBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "freqBus")
    val bwBus = ControlArgumentBuilder[FilterRejectReplaceInstrumentBuilder](this, "bwBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        bwBus.buildBus()
  }

  class HighpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = HighpassInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "highPass"

    val freqBus = ControlArgumentBuilder[HighpassInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class HighpassReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = HighpassReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "highPassReplace"

    val freqBus = ControlArgumentBuilder[HighpassReplaceInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class LowpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = LowpassInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "lowPass"

    val freqBus = ControlArgumentBuilder[LowpassInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildOut() ++
        buildDur() ++
        freqBus.buildBus()
  }

  class LowpassReplaceInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    type SelfType = LowpassReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "lowPassReplace"

    val freqBus = ControlArgumentBuilder[LowpassReplaceInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        freqBus.buildBus()
  }

  abstract class CommonNoiseInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        ampBus.buildBus() ++
        buildDur()
  }

  class PinkNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
    type SelfType = PinkNoiseInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "pinkNoise"
  }

  class WhiteNoiseInstrumentBuilder extends CommonNoiseInstrumentBuilder {
    type SelfType = WhiteNoiseInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "whiteNoise"
  }

  class PulseInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    type SelfType = PulseInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "pulse"

    val ampBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "freqBus")
    val widthBus = ControlArgumentBuilder[PulseInstrumentBuilder](this, "widthBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus() ++
        widthBus.buildBus()
  }

  class SineInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    type SelfType = SineInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "sine"

    val ampBus = ControlArgumentBuilder[SineInstrumentBuilder](this, "ampBus")
    val freqBus = ControlArgumentBuilder[SineInstrumentBuilder](this, "freqBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        freqBus.buildBus()
  }

  class FmInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with OutputBuilder {
    type SelfType = FmInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "fm"

    val ampBus = ControlArgumentBuilder[FmInstrumentBuilder](this, "ampBus")
    val carFreqBus = ControlArgumentBuilder[FmInstrumentBuilder](this, "carFreqBus")
    val modFreqBus = ControlArgumentBuilder[FmInstrumentBuilder](this, "modFreqBus")
    val modIndexBus = ControlArgumentBuilder[FmInstrumentBuilder](this, "modIndexBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildOut() ++
        buildDur() ++
        ampBus.buildBus() ++
        carFreqBus.buildBus() ++
        modFreqBus.buildBus() ++
        modIndexBus.buildBus()
  }

  abstract class CommonVolumeBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {

    val ampBus = ControlArgumentBuilder[SelfType](self(), "ampBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        ampBus.buildBus()
  }

  class StereoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
    type SelfType = StereoVolumeBuilder

    def self(): SelfType = this

    val instrumentName: String = "stereoVolume"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeBuilder extends CommonVolumeBuilder with OutputBuilder {
    type SelfType = MonoVolumeBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoVolume"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoVolumeReplaceBuilder extends CommonVolumeBuilder {
    type SelfType = MonoVolumeReplaceBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoVolumeReplace"
  }

  class PanInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder with OutputBuilder {
    type SelfType = PanInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "pan"

    val panBus = ControlArgumentBuilder[PanInstrumentBuilder](self(), "panBus")

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        buildOut() ++
        panBus.buildBus()
  }

  abstract class CommonMonoDelayInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        Seq("maxDelay", maxDelay)
  }

  class MonoDelayInstrumentBuilder extends CommonMonoDelayInstrumentBuilder with OutputBuilder {
    type SelfType = MonoDelayInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoDelay"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoDelayReplaceInstrumentBuilder extends CommonMonoDelayInstrumentBuilder {
    type SelfType = MonoDelayReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoDelayReplace"
  }

  abstract class CommonMonoCombInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    val decayTimeBus = ControlArgumentBuilder[SelfType](self(), "decayTimeBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        decayTimeBus.buildBus() ++
        Seq("maxDelay", maxDelay)
  }

  class MonoCombInstrumentBuilder extends CommonMonoCombInstrumentBuilder with OutputBuilder {
    type SelfType = MonoCombInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoComb"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoCombReplaceInstrumentBuilder extends CommonMonoCombInstrumentBuilder {
    type SelfType = MonoCombReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoCombReplace"
  }

  abstract class CommonMonoAllpassInstrumentBuilder extends AbstractInstrumentBuilder with DurBuilder with InputBuilder {
    val delayBus = ControlArgumentBuilder[SelfType](self(), "delayBus")

    val decayTimeBus = ControlArgumentBuilder[SelfType](self(), "decayTimeBus")

    var maxDelay: jl.Float = buildFloat(0f)

    def maxDelay(value: Float): SelfType = {
      maxDelay = buildFloat(value)
      self()
    }

    override def build(): Seq[Object] =
      super.build() ++
        buildIn() ++
        buildDur() ++
        delayBus.buildBus() ++
        decayTimeBus.buildBus() ++
        Seq("maxDelay", maxDelay)
  }

  class MonoAllpassInstrumentBuilder extends CommonMonoCombInstrumentBuilder with OutputBuilder {
    type SelfType = MonoAllpassInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoAllpass"

    override def build(): Seq[Object] =
      super.build() ++
        buildOut()
  }

  class MonoAllpassReplaceInstrumentBuilder extends CommonMonoCombInstrumentBuilder {
    type SelfType = MonoAllpassReplaceInstrumentBuilder

    def self(): SelfType = this

    val instrumentName: String = "monoAllpassReplace"
  }

  def lineControlInstrument = new LineControlInstrumentBuilder

  def pulseInstrument = new PulseInstrumentBuilder

  def filterInstrument = new FilterInstrumentBuilder

  def filterRejectInstrument = new FilterRejectInstrumentBuilder

  def filterReplaceInstrument = new FilterReplaceInstrumentBuilder

  def filterRejectReplaceInstrument = new FilterRejectReplaceInstrumentBuilder

  def highPassInstrument = new HighpassInstrumentBuilder

  def highPassReplaceInstrument = new HighpassReplaceInstrumentBuilder

  def lowPassInstrument = new LowpassInstrumentBuilder

  def lowPassReplaceInstrument = new LowpassReplaceInstrumentBuilder

  def whiteNoiseInstrument = new WhiteNoiseInstrumentBuilder

  def pinkNoiseInstrument = new PinkNoiseInstrumentBuilder

  def panInstrument = new PanInstrumentBuilder

  def monoDelayInstrument = new MonoDelayInstrumentBuilder

  def monoDelayReplaceInstrument = new MonoDelayReplaceInstrumentBuilder

  def monoVolumeInstrument = new MonoVolumeBuilder

  def monoReplaceVolumeInstrument = new MonoVolumeReplaceBuilder

  def allpassInstrument = new MonoAllpassInstrumentBuilder

  def allpassReplaceInstrument = new MonoAllpassReplaceInstrumentBuilder
}
