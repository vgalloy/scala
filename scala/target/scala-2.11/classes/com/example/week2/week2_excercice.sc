import scala.annotation.tailrec

type Set = Int => Boolean

def multipleOfTwo: Set = (test: Int) => test % 2 == 0
def multipleOfThree: Set = (test: Int) => test % 3 == 0

def singletonSet(elem: Int): Set = (test: Int) => test == elem
assert(singletonSet(2)(2))
assert(!singletonSet(2)(1))

def union(s: Set, t: Set): Set = (test: Int) => s(test) || t(test)
def uni = union(multipleOfTwo, multipleOfThree)
assert(!uni(1))
assert(uni(2))
assert(uni(3))
assert(uni(4))
assert(!uni(5))
assert(uni(6))

def intersect(s: Set, t: Set): Set = (test: Int) => s(test) && t(test)
def int = intersect(multipleOfTwo, multipleOfThree)
assert(!int(1))
assert(!int(2))
assert(!int(3))
assert(!int(4))
assert(!int(5))
assert(int(6))

def diff(s: Set, t: Set): Set = (test: Int) => s(test) ^ t(test)
def dif = diff(multipleOfTwo, multipleOfThree)
assert(!dif(1))
assert(dif(2))
assert(dif(3))
assert(dif(4))
assert(!dif(5))
assert(!dif(6))

def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)
def fil = filter(multipleOfTwo, multipleOfThree)
assert(!fil(1))
assert(!fil(2))
assert(!fil(3))
assert(!fil(4))
assert(!fil(5))
assert(fil(6))


def forall(s: Set, p: Int => Boolean): Boolean = {
  @tailrec
  def iter(a: Int): Boolean = {
    if (a > 1000) true
    else if (s(a) && !p(a)) false
    else iter(a + 1)
  }
  iter(-1000)
}

assert(forall(int, (test: Int) => test % 2 == 0))
assert(!forall(multipleOfThree, (test: Int) => test % 2 == 0))

def exists(s: Set, p: Int => Boolean): Boolean = {
  !forall(intersect(s, p), (test: Int) => false)
}

assert(exists(int, (test: Int) => test == 30))
assert(!exists(int, (test: Int) => test == 50))

def map(s: Set, f: Int => Int): Set =  (test: Int) => s(f(test))


assert(map(multipleOfTwo, (test: Int) => test * 2)(3))
assert(!map(multipleOfThree, (test: Int) => test * 2)(2))
