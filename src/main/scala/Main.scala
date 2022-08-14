import MyList.*
import scala.collection.mutable.StringBuilder
import scala.annotation.tailrec

enum MyList[+A]:
  case MyCons(head: A, tail: MyList[A])
  case MyNil

  override def toString: String =
    @scala.annotation.tailrec 
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyList.MyNil => sb.append(']').result
        case MyList.MyCons(head, MyList.MyNil) => sb.append(head).append(']').result
        case MyList.MyCons(head, tail) => go(sb.append(head).append(", "), tail)
      }
    }
    go(new StringBuilder("["), this)
    
  def length[A](): Long =
    @tailrec
    def go[A](xs: MyList[A], len: Long): Long =
      xs match
        case MyList.MyNil => len
        case MyList.MyCons(x, tail) =>
          go(tail, 1 + len)
    
    go(this, 0)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }


  def foldLeft[A,B](xs: MyList[A], z: B)(f: (A, B) => B): B = {
    @scala.annotation.tailrec
    def go(xs: MyList[A], acc: B)(f: (A, B) => B): B =  {
      xs match {
        case MyList.MyNil => acc
        case MyList.MyCons(head, tail) => go(tail, f(head, acc))(f)
      }
    }
    go(xs, z)(f)
  } 
   
   def distinct[A](xs: MyList[A]) = {
    def go(set: Set[A], acc: MyList[A]): MyList[A] = {
      acc match {
        case MyList.MyNil => MyList.MyNil
        case MyList.MyCons(head,tail) if set contains head => go(set, tail)
        case MyList.MyCons(head,tail) => MyList.MyCons(head, go(set + head, tail))
      }
    }
    go(Set(), xs)
  }

  def mkString[A](xs: MyList[A],prefix: String = "", separator: String = "", postfix: String = ""): String =
    @scala.annotation.tailrec 
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyList.MyNil => sb.append(postfix).result
        case MyList.MyCons(head, MyNil) => sb.append(head).append(postfix).result
        case MyList.MyCons(head, tail) => go(sb.append(head).append(separator), tail)
      }
    }
    go(new StringBuilder(prefix), xs)

  def elementAt[A](xs: MyList[A], n: Int): Option[A] =
    foldLeft(xs, (0, None): (Int, Option[A]))((a, b) => {
    if (b._2.isEmpty)
      val i = b._1
      if (i == n)
        (n, Some(a))
      else
        (i + 1, None)
    else
      b
  })._2


@main def main = 
  println("Hello World!")
