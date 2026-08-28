namespace HopacPlus.Benchmark

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs

module Native =
    open Hopac

    // forNIgnore of immediate jobs does not trampoline and overflows the stack
    // around a few thousand iterations. forUpToIgnore / whileDoDelay use Job.Do.

    let result n =
        Hopac.run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1))

    let map n =
        Hopac.run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1 |> Job.map ((+) 1)))

    let bind n =
        Hopac.run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1 |> Job.bind (fun x -> Job.result (x + 1))))

    let seq n =
        let jobs = Array.init n (fun i -> Job.result i)

        Hopac.run (Job.forUpToIgnore 0 (n - 1) (fun i -> jobs[i]))

    let jobCe n =
        Hopac.run (
            job {
                let mutable i = 0
                let mutable s = 0

                while i < n do
                    let! x = Job.result i
                    s <- s + x
                    i <- i + 1

                return s
            }
        )

module Wrapped =
    open HopacPlus

    let result n =
        run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1))

    let map n =
        run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1 |> Job.map ((+) 1)))

    let bind n =
        run (Job.forUpToIgnore 1 n (fun _ -> Job.result 1 |> Job.bind (fun x -> Job.result (x + 1))))

    let seq n =
        let jobs = Array.init n (fun i -> Job.result i)

        run (Job.forUpToIgnore 0 (n - 1) (fun i -> jobs[i]))

    let jobCe n =
        run (
            job {
                let mutable i = 0
                let mutable s = 0

                while i < n do
                    let! x = Job.result i
                    s <- s + x
                    i <- i + 1

                return s
            }
        )

[<MemoryDiagnoser>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
[<CategoriesColumn>]
type JobBench() =
    [<Params(1_000_000)>]
    member val N = 0 with get, set

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Result")>]
    member this.Hopac_Result() = Native.result this.N

    [<Benchmark>]
    [<BenchmarkCategory("Result")>]
    member this.Plus_Result() = Wrapped.result this.N

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Map")>]
    member this.Hopac_Map() = Native.map this.N

    [<Benchmark>]
    [<BenchmarkCategory("Map")>]
    member this.Plus_Map() = Wrapped.map this.N

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Bind")>]
    member this.Hopac_Bind() = Native.bind this.N

    [<Benchmark>]
    [<BenchmarkCategory("Bind")>]
    member this.Plus_Bind() = Wrapped.bind this.N

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("Seq")>]
    member this.Hopac_Seq() = Native.seq this.N

    [<Benchmark>]
    [<BenchmarkCategory("Seq")>]
    member this.Plus_Seq() = Wrapped.seq this.N

    [<Benchmark(Baseline = true)>]
    [<BenchmarkCategory("JobCE")>]
    member this.Hopac_JobCE() = Native.jobCe this.N

    [<Benchmark>]
    [<BenchmarkCategory("JobCE")>]
    member this.Plus_JobCE() = Wrapped.jobCe this.N
