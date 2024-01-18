package functional.data

case class Graph[A](element: A, neighbors: List[Graph[A]])