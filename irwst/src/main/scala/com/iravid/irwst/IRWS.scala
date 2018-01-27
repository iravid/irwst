package com.iravid.irwst

sealed abstract class IRWS[E, SA, SB, L, A] { self =>
  def tag: Int

  final def map[B](f: A => B): IRWS[E, SA, SB, L, B] = IRWS.FlatMap(self, f.andThen(IRWS.Pure[E, SB, L, B](_)))

  final def flatMap[SC, B](f: A => IRWS[E, SB, SC, L, B]): IRWS[E, SA, SC, L, B] =
    IRWS.FlatMap(self, f)
}

object IRWS {
  final def pure[E, S, L, A](a: A): IRWS[E, S, S, L, A] = Pure(a)

  final def get[E, S, L]: IRWS[E, S, S, L, S] = Get()

  final def set[E, SA, SB, L](s: SB): IRWS[E, SA, SB, L, Unit] = Set(s)

  final def ask[E, S, L]: IRWS[E, S, S, L, E] = Ask()

  final def tell[E, S, L](l: L): IRWS[E, S, S, L, Unit] = Tell(l)

  final def logged[E, S, L, A](l: L, a: A): IRWS[E, S, S, L, A] = Logged(l, a)

  final def apply[E, SA, SB, L, A](f: (E, SA) => (SB, L, A)): IRWS[E, SA, SB, L, A] = Wrap(f)

  final def modify[E, SA, SB, L](f: SA => SB): IRWS[E, SA, SB, L, Unit] =
    get.flatMap(s => set(f(s)))

  final def inspect[E, SA, L, SB](f: SA => SB): IRWS[E, SA, SA, L, SB] =
    get.map(f)

  object Tags {
    final val Pure = 0
    final val FlatMap = 1
    final val Map = 2
    final val Get = 3
    final val Set = 4
    final val Ask = 5
    final val Tell = 6
    final val Logged = 7
    final val Wrap = 8
  }

  final case class Pure[E, S, L, A](a: A) extends IRWS[E, S, S, L, A] {
    override final def tag = Tags.Pure
  }

  final case class FlatMap[E, SA, SB, SC, L, A, B](fa: IRWS[E, SA, SB, L, A], f: A => IRWS[E, SB, SC, L, B]) 
      extends IRWS[E, SA, SC, L, B] {
    override final def tag = Tags.FlatMap
  }

  final case class Get[E, S, L]() extends IRWS[E, S, S, L, S] {
    override final def tag = Tags.Get
  }

  final case class Set[E, SA, SB, L](s: SB) extends IRWS[E, SA, SB, L, Unit] {
    override final def tag = Tags.Set
  }

  final case class Ask[E, S, L]() extends IRWS[E, S, S, L, E] {
    override final def tag = Tags.Ask
  }

  final case class Tell[E, S, L](l: L) extends IRWS[E, S, S, L, Unit] {
    override final def tag = Tags.Tell
  }

  final case class Logged[E, S, L, A](l: L, a: A) extends IRWS[E, S, S, L, A] {
    override final def tag = Tags.Logged
  }

  final case class Wrap[E, SA, SB, L, A](f: (E, SA) => (SB, L, A)) extends IRWS[E, SA, SB, L, A] {
    override final def tag = Tags.Wrap
  }
}

object Interpreter {
  def runOptimized[E, SA, SB, L, A](env: E, init: SA, initLog: L)(fa: IRWS[E, SA, SB, L, A])(combine: (L, L) => L): (SB, L, A) = {
    var currOp: IRWS[E, Any, Any, L, Any] = fa.asInstanceOf[IRWS[E, Any, Any, L, Any]]
    var done: Boolean = false

    val conts: java.util.ArrayDeque[Any => IRWS[E, Any, Any, L, Any]] = new java.util.ArrayDeque

    var log: L = initLog

    var state: Any = init
    var res: Any = null

    // Put () on the stack to avoid field access
    val unit = ()

    do {
      currOp.tag match {
        case IRWS.Tags.Pure =>
          res = currOp.asInstanceOf[IRWS.Pure[E, Any, L, A]].a

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Get =>
          res = state

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Set =>
          val op = currOp.asInstanceOf[IRWS.Set[E, Any, Any, L]]

          state = op.s
          res = unit

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Ask =>
          res = env

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Tell =>
          val op = currOp.asInstanceOf[IRWS.Tell[E, Any, L]]

          log = combine(log, op.l)

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Logged =>
          val op = currOp.asInstanceOf[IRWS.Logged[E, Any, L, A]]

          res = op.a
          log = combine(log, op.l)

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.Wrap =>
          val op = currOp.asInstanceOf[IRWS.Wrap[E, Any, Any, L, A]]

          val wrapResult = op.f(env, state)

          state = wrapResult._1
          log = combine(log, wrapResult._2)
          res = wrapResult._3

          if (conts.isEmpty())
            done = true
          else
            currOp = conts.pollFirst()(res)

        case IRWS.Tags.FlatMap =>
          val op = currOp.asInstanceOf[IRWS.FlatMap[E, Any, Any, Any, L, Any, Any]]

          op.fa.tag match {
            case IRWS.Tags.Pure =>
              val nested = op.fa.asInstanceOf[IRWS.Pure[E, Any, L, Any]]

              res = nested.a
              currOp = op.f(nested.a)

            case IRWS.Tags.Get =>
              res = state
              currOp = op.f(state)

            case IRWS.Tags.Set =>
              val nested = op.fa.asInstanceOf[IRWS.Set[E, Any, Any, L]]

              state = nested.s
              res = unit
              currOp = op.f(unit)

            case IRWS.Tags.Ask =>
              res = env
              currOp = op.f(env)

            case IRWS.Tags.Tell =>
              val nested = op.fa.asInstanceOf[IRWS.Tell[E, Any, L]]

              res = unit
              log = combine(log, nested.l)
              currOp = op.f(unit)

            case IRWS.Tags.Logged =>
              val nested = currOp.asInstanceOf[IRWS.Logged[E, Any, L, A]]

              res = nested.a
              log = combine(log, nested.l)

              currOp = op.f(res)

            case IRWS.Tags.Wrap =>
              val nested = currOp.asInstanceOf[IRWS.Wrap[E, Any, Any, L, A]]

              val wrapResult = nested.f(env, state)

              state = wrapResult._1
              log = combine(log, wrapResult._2)
              res = wrapResult._3

              currOp = op.f(res)

            case _ =>
              currOp = op.fa
              conts.addFirst(op.f)
          }
      }
    } while (!done)

    (state.asInstanceOf[SB], log, res.asInstanceOf[A])
  }
}
