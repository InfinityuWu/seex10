package functional.data

enum Tree[+A]:
  case Empty extends Tree[Nothing]
  case Node(left: Tree[A], element: A, right: Tree[A])