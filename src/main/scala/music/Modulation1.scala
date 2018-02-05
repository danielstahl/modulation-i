package music

import java.{lang => jl}

import music.Instruments._
import net.soundmining.Instrument.{SOURCE, TAIL_ACTION, setupNodes}
import net.soundmining.Utils.absoluteTimeToMillis
import net.soundmining.{BusGenerator, Melody, MusicPlayer}
import net.soundmining.Spectrum._
import net.soundmining.Note._
import LineControlInstrumentBuilder._
import ARControlInstrumentBuilder._

/**
 * Main class for the piece
  *
  * https://en.wikipedia.org/wiki/Mystic_chord
  * https://en.wikipedia.org/wiki/Acoustic_scale
  *
 */
object Modulation1 {

  val C2 = 65.41f
  val FISS2 = 92.50f
  val BESS2 = 116.54f
  val E3 = 164.81f
  val A3 = 220f
  val D4 = 293.66f

  val mysticChord = Seq(
      C2, FISS2, BESS2, E3, A3, D4
  )

  // http://www.phy.mtu.edu/~suits/notefreqs.html
  // https://en.wikipedia.org/wiki/Mystic_chord
  // https://en.wikipedia.org/wiki/Acoustic_scale

  def main(args: Array[String]): Unit = {
    import PlayClass._

    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)

    val naturalSpectrum = makeSpectrum2(c2, naturalFact, 25)
    val naturalRythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)

    val mysticAcousticScale = Seq(1, 3, 4, 6, 7, 8, 9, 10, 12, 13, 15)

    println("Mystic chord")
    mysticAcousticScale.foreach(i => {
      val specFreq = naturalSpectrum(i)
      val specNote = hertzToNote(specFreq)
      println(s"$i = $specNote ($specFreq)")
    })

    println("Mystic chord rythm")
    mysticAcousticScale.foreach(i => {
      val specFreq = naturalRythmSpectrum(i)
      println(s"$i = $specFreq")
    })

    val fissFact = makeFact(naturalSpectrum(7), naturalSpectrum(10))
    val hessFact = makeFact(naturalSpectrum(7), naturalSpectrum(13))
    val eFact = makeFact(naturalSpectrum(3), naturalSpectrum(9))
    val aFact = makeFact(naturalSpectrum(7), naturalSpectrum(12))
    val dFact = makeFact(naturalSpectrum(3), naturalSpectrum(8))

    println(s"Fact Fiss $fissFact" )
    println(s"Fact hess $hessFact" )
    println(s"Fact e $eFact" )
    println(s"Fact a $aFact" )
    println(s"Fact d $dFact" )


  /*
    mysticAcousticScale.indices.foreach {
      i => play(i, 5).sine(spectrum(i), 0.7f).pan(0, 0)
    }
*/

/*
    Mystic chord
    1 = 'c3 (130.8128)
    3 = 'c4 (261.6256)
    4 = 'e4 (327.032)
    6 = 'hess4 (457.84482)
    7 = 'c5 (523.2512)
    8 = 'd5 (588.6576)
    9 = 'e5 (654.064)
    10 = 'fiss5 (719.47046)
    12 = 'giss5 (850.2832)
    13 = 'hess5 (915.68964)
    15 = 'c6 (1046.5024)

    Mystic chord rythm
    1 = 1.021975
    3 = 2.04395
    4 = 2.5549376
    6 = 3.5769126
    7 = 4.0879
    8 = 4.5988874
    9 = 5.109875
    10 = 5.620863
    12 = 6.6428375
    13 = 7.1538253
    15 = 8.1758
    */


    /*
    Melody1 implement a low fm that has a low carrier and
    the mod freq from the sine melody. Make it so that
    sine melody outputs into a hidden bus, 16 and then
    the fm takes the mod freq from there.
    */
    //melodyOne(naturalSpectrum, naturalRythmSpectrum, 0.0f)

    //melodyTwo(naturalSpectrum, naturalRythmSpectrum, 0.0f)

    // melodyTwo
    // Rythm
    // 10 12 10 8
    // Durations
    // 15 13 15 13

  }
}
