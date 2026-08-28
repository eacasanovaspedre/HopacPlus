module HopacPlus.Benchmark.Program

open System.Reflection
open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    BenchmarkSwitcher.FromAssembly(Assembly.GetExecutingAssembly()).Run args
    |> ignore

    0
