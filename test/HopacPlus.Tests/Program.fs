module HopacPlus.Tests.Program

open Expecto

[<EntryPoint>]
let main args =
    // Hopac's global scheduler can deadlock if tests share it in parallel.
    let args =
        if args |> Array.exists (fun a -> a = "--parallel" || a = "--sequenced") then
            args
        else
            Array.append [| "--sequenced" |] args

    runTestsInAssemblyWithCLIArgs [] args
