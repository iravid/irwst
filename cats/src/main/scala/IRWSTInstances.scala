package com.iravid.irwst
package cats

import _root_.cats.{ Monad, Monoid, StackSafeMonad }

abstract class IRWSSyntax {
  implicit class IRWSOps[E, SA, SB, L, A](fa: IRWS[E, SA, SB, L, A]) {
    def run(e: E, sa: SA)(implicit L: Monoid[L]): (SB, L, A) =
      Interpreter.runOptimized(e, sa, L.empty)(fa)(L.combine)

    def runS(e: E, sa: SA)(implicit L: Monoid[L]): SB = run(e, sa)._1

    def runL(e: E, sa: SA)(implicit L: Monoid[L]): L = run(e, sa)._2

    def runA(e: E, sa: SA)(implicit L: Monoid[L]): A = run(e, sa)._3
  }
}

abstract class IRWSInstances0 {
  implicit def comIravidIRWSMonad[E, S, L]: Monad[IRWS[E, S, S, L, ?]] =
    new Monad[IRWS[E, S, S, L, ?]] with StackSafeMonad[IRWS[E, S, S, L, ?]] {
      def pure[A](x: A): IRWS[E, S, S, L, A] = IRWS.pure(x)

      def flatMap[A, B](fa: IRWS[E, S, S, L, A])(f: A => IRWS[E, S, S, L, B]): IRWS[E, S, S, L, B] =
        fa.flatMap(f)
    }
}
