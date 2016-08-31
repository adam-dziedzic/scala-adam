//:load list.scala

val list = List(1,2,3,4,5)

import List._
val list2 = dropWhile(list, (x: Int) => x < 4)
println("list after drop while: " + list2)