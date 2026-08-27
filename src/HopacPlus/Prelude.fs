namespace HopacPlus

open Hopac

[<AutoOpen>]
module internal InternalTypes =

    type HopacJob<'T> = Job<'T>
    type HopacAlt<'T> = Alt<'T>
    type HopacPromise<'T> = Promise<'T>
