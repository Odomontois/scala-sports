/**
 * Author: Oleg Nizhnik
 * Date  : 22.05.2015
 * Time  : 16:03
 */
package stackoverflow

object ExtendingTraits  extends App{
  type CanCopyActions[A <: Widget[A]] = {
    def copy(actions: List[Action[A]]): A
  }
  trait Widget[A <: Widget[A]] {
    self: A with CanCopyActions[A] =>

    def actions: List[Action[A]]
    def add[B](action: B)(implicit conv: B => Action[A]) = copy(conv(action) :: actions)
    def runall = actions foreach (_(self))
  }
case class WidgetA(actions: List[Action[WidgetA]]) extends Widget[WidgetA]
case class WidgetB(actions: List[Action[WidgetB]]) extends Widget[WidgetB]
case class WidgetC(actions: List[Action[WidgetC]]) extends Widget[WidgetC]

  trait Action[-A <: Widget[_]] {
    def apply(w: A) {}
  }

object MyActionA extends Action[WidgetA] {
  override def apply(w: WidgetA) = println("action A")
}
object MyActionB extends Action[WidgetB] {
  override def apply(w: WidgetB) = println("action B")
}
object MyActionC extends Action[WidgetC] {
  override def apply(w: WidgetC) = println("action C")
}
object MyAction extends Action[Widget[_]] {
  override def apply(w: Widget[_]) = println("common action")
}
object MyActionAB extends Action[WidgetA \/ WidgetB] {
  override def apply(w: WidgetA \/ WidgetB) = w.widget match {
    case Left(widgetA) => println("dual action A")
    case Right(widgetB) => println("dual action B")
  }
}

  case class \/[A <: Widget[A], B <: Widget[B]](widget: Either[A, B], actions: List[Action[A \/ B]]) extends Widget[A \/ B] {
    def copy(actions: List[Action[A \/ B]]) = \/(widget, actions)
  }

  implicit def eitherLeftAction[A <: Widget[A], B <: Widget[B]](action: Action[A \/ B]): Action[A] = new Action[A] {
    override def apply(widget: A) = action.apply(\/(Left(widget), Nil))
  }
  implicit def eitherRightAction[A <: Widget[A], B <: Widget[B]](action: Action[A \/ B]): Action[B] = new Action[B] {
    override def apply(widget: B) = action.apply(\/(Right(widget), Nil))
  }

  WidgetA(List(MyActionA)).add(MyAction).add(MyActionAB).runall

  WidgetB(List(MyActionB)).add(MyAction).add(MyActionAB).runall

}
