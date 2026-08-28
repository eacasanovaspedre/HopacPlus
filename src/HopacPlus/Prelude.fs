namespace HopacPlus

open Hopac

type HopacJob<'T> = Job<'T>
type HopacAlt<'T> = Alt<'T>
type HopacPromise<'T> = Promise<'T>
type HopacCh<'T> = Ch<'T>
type HopacIVar<'T> = IVar<'T>
type HopacMVar<'T> = MVar<'T>
type HopacMailbox<'T> = Mailbox<'T>
type HopacLatch = Latch
type HopacBoundedMb<'T> = BoundedMb<'T>
type HopacLock = Lock
