def pascal(c: Int, r: Int): Int = {
  if (c == r) 1
  else if (c == 0) 1
  else pascal(c, r - 1) + pascal(c - 1, r - 1)
}

pascal(0, 2)
pascal(1, 2)
pascal(1, 3)

def balance(chars: List[Char]): Boolean = {
  def balanceReq(chars: List[Char], value: Int): Boolean = {
    if (chars.isEmpty) value == 0
    else if (value < 0) {
      return false;
    } else if (chars.head == '(') {
      balanceReq(chars.tail, value + 1)
    } else if (chars.head == ')') {
      balanceReq(chars.tail, value - 1)
    } else {
      balanceReq(chars.tail, value)
    }
  }
  balanceReq(chars, 0)
}


def test1 = "(if (zero? x) max (/ 1 x))".toList
balance(test1)

def test2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)".toList
balance(test2)

def test3 = ":-)".toList
balance(test3)

def test4 = "())(".toList
balance(test4)


def countChange(money: Int, coins: List[Int]) = {
  def countChangeReq(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money < 0) 0
    else if (money == 0) 1
    else countChangeReq(money - coins.head, coins) + countChangeReq(money, coins.tail)
  }

  def sortAsc(coins: List[Int]) = coins sortWith (_ > _)

  countChangeReq(money, sortAsc(coins))
}

countChange(1, List(1, 2))
countChange(2, List(1, 2))
countChange(3, List(1, 2))
countChange(4, List(1, 2))
countChange(5, List(1, 2))
countChange(6, List(1, 2))
countChange(6, List(1, 2, 3))
countChange(6, List())