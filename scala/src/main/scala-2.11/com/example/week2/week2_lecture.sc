def test(a: Int)(b: Int): Int = {
  a + b
}

test(1)(2)

class Rational(a: Int, b: Int) {
  require(b > 0)
  def num = a;

  def den = b;

  def + (other: Rational) = new Rational(this.num * other.den + this.den * other.num, this.den * other.den)

  def unary_- : Rational = new Rational(-this.num, this.den)

  override def toString = num + "/" + den
}

def firstRational = new Rational(1,2)
firstRational
firstRational.num
firstRational.den

def tmp: Rational = new Rational(1, 2) + new Rational(1, 2)
tmp
def negtmp: Rational = -tmp
negtmp

