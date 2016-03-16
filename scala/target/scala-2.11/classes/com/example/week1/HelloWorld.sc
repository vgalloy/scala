import scala.annotation.tailrec

"Hello, world"

val x = 1

def increase(i: Int) = i + 1

increase(3)


def sqrt(x: Double) = {
  def abs(x: Double) = if(x > 0) x else -x

  def isGoodEnough(y: Double) = abs(y * y - x)/x < 0.001

  def newStep(currentStep: Double) = (x / currentStep + currentStep) / 2

  @tailrec
  def sqrtReq(y: Double):Double = if(isGoodEnough(y)) y else sqrtReq(newStep(y))

  sqrtReq(1)
}


sqrt(2)


