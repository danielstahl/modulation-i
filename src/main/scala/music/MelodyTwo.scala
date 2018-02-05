package music

import net.soundmining.Instrument.setupNodes
import net.soundmining.Note.noteToHertz
import net.soundmining.Spectrum.{makeFact, makeSpectrum2}
import net.soundmining.{BusGenerator, Melody, MusicPlayer}

object MelodyTwo {

  def melodyTwo(start: Float)(implicit player: MusicPlayer): Unit = {
    val c2 = noteToHertz('c2)
    val c3 = noteToHertz('c3)
    val cminus5 = noteToHertz('c0) / 2f / 2f / 2f / 2f / 2f

    val naturalFact = makeFact(c2, c3)

    val spectrum = makeSpectrum2(c2, naturalFact, 25)
    val rhythmSpectrum = makeSpectrum2(cminus5, naturalFact, 25)

    val times = Melody.absolute(start, Seq(
      rhythmSpectrum(4), rhythmSpectrum(9), rhythmSpectrum(3), rhythmSpectrum(9)))
      .iterator

    val durations = Seq(
      rhythmSpectrum(15), rhythmSpectrum(13), rhythmSpectrum(15), rhythmSpectrum(13))
      .iterator

    val play = Player()
    play(times.next(), durations.next()).fm(spectrum(4), spectrum(7), 0.7f, (0, 3, 0), 0.3f).pan(-1f, -0.5f).send()

    play(times.next(), durations.next()).sine(spectrum(10), 0.7f).pan(1f, 0.5f).send()

    play(times.next(), durations.next()).fm(spectrum(8), spectrum(12), 0.3f, (0, 3, 0), 0.7f).pan(1f, 0.5f).send()

    play(times.next(), durations.next()).pulse(spectrum(9), (0.01f, 0.99f), 0.7f).pan(-1f, -0.5f).send()

    //play(times.next(), durations.next()).sine(spectrum(9), 0.3f).pan(-1f, -0.5f)
  }

  def main(args: Array[String]): Unit = {
    BusGenerator.reset()
    implicit val player: MusicPlayer = MusicPlayer()

    player.startPlay()

    setupNodes(player)

    melodyTwo(0.0f)
  }
}
