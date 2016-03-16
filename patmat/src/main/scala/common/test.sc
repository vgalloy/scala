def isPrime(n: Int): Boolean = {
  !((2 until n) exists (x => n % x == 0))
}

assert(!isPrime(10))
assert(isPrime(11))
assert(!isPrime(12))
assert(isPrime(13))
assert(!isPrime(14))


