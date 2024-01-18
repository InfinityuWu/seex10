package functional

import functional.data.Graph
import functional.data.Tree.{Empty, Node}

@main def Main(): Unit =
  val tuple1 = (1, "foo")
  val tuple2 = (2, "bar")

  val list1 = List(1, 2)
  val list2 = List(1, 2, 3)

  val tree1 = Node(Empty, 1, Empty)

  val tree2 = Node(
                Node(Empty, 2, Empty),
                1,
                Node(Empty, 3, Empty)
              )

  val graph1 = Graph(1, Nil)
  val graph2 = Graph(1, List(Graph(2, Nil)))