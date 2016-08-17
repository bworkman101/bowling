import scala.annotation.tailrec

object Bowling {

  def score(bowls: String): Int = {
    return scoreFrame(bowls.toCharArray)
  }

  @tailrec
  def scoreFrame(bowls: Array[Char], frame: Int = 0, scoreSum: Int = 0): Int = {

    if (frame == 10) {
      return scoreSum
    }
    else {

      val frameOffest = frame * 2

      val bowl1Char = bowls(frameOffest)
      val bowl2Char = bowls(frameOffest + 1)

      if (bowl1Char == 'X') {
        // The next character will be blank, so get the character after the blank
        val bowl3Char =
          if (frame == 9) {
            bowls(frameOffest + 1)
          }
          else chars3rdBowl(bowls, frameOffest)

        val bowl3 = val3rdBowl(bowl3Char)

        // The next character could be blank if the previous was blank
        val bowl4Char = chars4thBowl(bowls, frame, frameOffest)
        val bowl4 = val4thBowl(bowl3, bowl4Char)

        return scoreFrame(bowls, frame + 1, scoreSum + 10 + bowl3 + bowl4)
      }
      else if (bowl2Char == '/') {
        // Look at next 1 frame,  use 0 if no next frames
        val bowl3Char = chars3rdBowl(bowls, frameOffest)
        val bowl3 = val3rdBowl(bowl3Char)
        return scoreFrame(bowls, frame + 1, scoreSum + 10 + bowl3)
      }
      else {
        val bowl1 = bowl1Char.asDigit
        val bowl2 = bowl2Char.asDigit
        return scoreFrame(bowls, frame + 1, scoreSum + bowl1 + bowl2)
      }
    }
  }

  def chars4thBowl(bowls: Array[Char], frame: Int, frameOffest: Int): Char = {
    if (frame == 9) {
      bowls(frameOffest + 2)
    }
    else if (bowls.size > frameOffest + 3) {
      if (bowls(frameOffest + 3) == ' ') {
        if (bowls.size > frameOffest + 4) {
          bowls(frameOffest + 4)
        }
        else {
          '0'
        }
      }
      else {
        bowls(frameOffest + 3)
      }
    }
    else {
      '0'
    }
  }

  def val4thBowl(bowl3: Int, bowl4Char: Char): Int = {
    if (bowl4Char == 'X') {
      10
    }
    else if (bowl4Char == '/') {
      10 - bowl3
    }
    else {
      bowl4Char.asDigit
    }
  }

  def chars3rdBowl(bowls: Array[Char], frameOffest: Int): Char = {
    if (bowls.size > frameOffest + 2) {
      bowls(frameOffest + 2)
    } else {
      '0'
    }
  }

  def val3rdBowl(bowl3Char: Char): Int = {
    if (bowl3Char == '/' || bowl3Char == 'X') 10 else bowl3Char.asDigit
  }
}
