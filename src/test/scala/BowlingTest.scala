import Bowling._
import org.scalatest._

class BowlingTest extends FlatSpec with Matchers {

  "A score of 100" should "happen for bowl frames         '039/219/X 2380X 2/12'" in {
    score("039/219/X 2380X 2/12") should be (100);
  }

  "A perfect score of 300" should "happen for bowl frames 'X X X X X X X X X XXX'" in {
    score("X X X X X X X X X XXX") should be (300);
  }

  "A score of 166" should "happen for bowl frames         '07X 43217/443/X X XXX'" in {
    score("07X 43217/443/X X XXX") should be (166);
  }

  "A score of 0" should "happen for bowl frames           '00000000000000000000'" in {
    score("00000000000000000000") should be (0);
  }

  "A score of 272" should "happen for bowl frames           'X X X X X X X X X 2/X'" in {
    score("X X X X X X X X X 2/X") should be (272);
  }

  "A score of 103" should "happen for bowl frames           '128136259/X 326/434/4'" in {
    score("128136259/X 326/434/4") should be (103);
  }

}
