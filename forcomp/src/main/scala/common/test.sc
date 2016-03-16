val list = List("ab", "cd", "ef")

list.foldRight(""){
  (num, acc) => (num ++ acc)
}

list.foldLeft(""){
  (acc, add) => (acc ++ add)
}


def fun (n: Int): List[Int] = {
  (1 to n).toSeq.toList
}

fun(2)


def merge(list1: List[Int], list2: List[Int]): List[(Int, Int)] = {
  for(a <- list1;
    b <-list2) yield (a,b)
}

merge(List(1,2,3), List(2,4, 6))

list.filter(p => false)