package tools

import scala.reflect.ClassTag


object CustomArray {

  implicit def toCustomArray[T: ClassTag](a: Array[T]): CustomArray[T] = new CustomArray(a)
}

class CustomArray[T: ClassTag](l: Array[T]) {

  def dropAtIndex(index: Int): Array[T] = {

    l.take(index) ++ l.drop(index + 1)
  }

  def maxOptionBy[B: Ordering](f: T => B): Option[T] =
    l reduceOption Ordering.by(f).max

}
