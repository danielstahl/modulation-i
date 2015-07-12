package music

import java.lang.Float
import java.{lang => jl}

import music.Instruments._
import net.soundmining.Spectrum._

/**
 * Main class for the piece
 */
object Piece {
  def makeSeqWithIndex[T](seq: Seq[T]) = {
    0 until seq.size map {
      i => (i, seq(i))
    }
  }

  def main(args: Array[String]): Unit = {
  
  }
}
