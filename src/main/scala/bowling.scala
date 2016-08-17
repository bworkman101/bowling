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

      val bowl1Char = bowls(frame * 2)
      val bowl2Char = bowls(frame * 2 + 1)

      if (bowl1Char == 'X') {
        // The next character will be blank, so get the character after the blank
        val bowl3Char =
          if (frame == 9) {
            bowls(frame * 2 + 1)
          }
          else if (bowls.size > frame * 2 + 2) {
            bowls(frame * 2 + 2)
          } else {
            '0'
          }

        val bowl3 = if (bowl3Char == 'X') 10 else bowl3Char.asDigit

        // The next character could be blank if the previous was blank
        val bowl4Char =
          if (frame == 9) {
            bowls(frame * 2 + 2)
          }
          else if (bowls.size > frame * 2 + 3) {
            if (bowls(frame * 2 + 3) == ' ') {
              if (bowls.size > frame * 2 + 4) {
                bowls(frame * 2 + 4)
              }
              else {
                '0'
              }
            }
            else {
              bowls(frame * 2 + 3)
            }
          }
          else {
            '0'
          }

        val bowl4 =
          if (bowl4Char == 'X') {
            10
          }
          else if (bowl4Char == '/') {
            10 - bowl3
          }
          else {
            bowl4Char.asDigit
          }

        return scoreFrame(bowls, frame + 1, scoreSum + 10 + bowl3 + bowl4)
      }
      else if (bowl2Char == '/') {
        // Look at next 1 frame,  use 0 if no next frames
        val bowl3Char = if (bowls.size > frame * 2 + 2) bowls(frame * 2 + 2) else '0'
        val bowl3 = if (bowl3Char == '/' || bowl3Char == 'X') 10 else bowl3Char.asDigit
        return scoreFrame(bowls, frame + 1, scoreSum + 10 + bowl3)
      }
      else {
        val bowl1 = bowl1Char.asDigit
        val bowl2 = bowl2Char.asDigit
        return scoreFrame(bowls, frame + 1, scoreSum + bowl1 + bowl2)
      }
    }
  }
}
