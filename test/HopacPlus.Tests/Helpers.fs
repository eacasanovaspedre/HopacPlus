module HopacPlus.Tests.Helpers

open System
open Expecto
open HopacPlus

let inline eq expected actual = Expect.equal actual expected ""

let throws (j: Job<'a>) =
    Expect.throws (fun () -> run j |> ignore) ""

let runNative (x: Hopac.Job<'a>) = Hopac.Hopac.run x

type TestExn(msg) =
    inherit Exception(msg)
