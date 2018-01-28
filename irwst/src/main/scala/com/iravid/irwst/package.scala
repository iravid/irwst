package com.iravid

package object irwst {
  type Reader[E, A] = IRWS[E, Unit, Unit, Unit, A]

  type Writer[L, A] = IRWS[Unit, Unit, Unit, L, A]

  type State[S, A] = IRWS[Unit, S, S, Unit, A]

  type IndexedState[SA, SB, A] = IRWS[Unit, SA, SB, Unit, A]

  type ReaderWriter[E, L, A] = IRWS[E, Unit, Unit, L, A]

  type WriterState[S, L, A] = IRWS[Unit, S, S, L, A]

  type IWS[SA, SB, L, A] = IRWS[Unit, SA, SB, L, A]

  type ReaderState[E, S, A] = IRWS[E, S, S, Unit, A]

  type IRS[E, SA, SB, A] = IRWS[E, SA, SB, Unit, A]
}
