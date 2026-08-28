# HopacPlus (Hopac+)

A drop-in F# API over [Hopac](https://github.com/Hopac/Hopac) that also works with [F#+](https://github.com/fsprojects/FSharpPlus).

Hopac is a concurrent programming library for F#: lightweight jobs, first-class alternatives (`Alt`), and message-passing primitives (`Ch`, `IVar`, `MVar`, mailboxes, streams). F#+ is a generic programming library that discovers `map`, `bind`, `monad { }`, and similar operations through statically resolved type parameters.

Native Hopac types do not expose the static members F#+ looks for, so `map`, `>>=`, and `monad { }` will not resolve against them. HopacPlus wraps those types in structs that implement the F#+ members, and re-exports Hopac’s surface so you can write the same programs against the wrappers.

HopacPlus itself depends only on Hopac. F#+ is optional: add it if you want generic `map` / `bind` / `monad`.

## Wrappers

Each Hopac primitive becomes a struct with a `ToHopac` conversion back to the original type:

| Wrapper | Hopac type |
| --- | --- |
| `Job<'T>` | lightweight concurrent job |
| `Alt<'T>` | first-class synchronous alternative |
| `Promise<'T>` | memoized job / IVar-like promise |
| `Ch<'T>` | synchronous channel |
| `IVar<'T>` | write-once variable |
| `MVar<'T>` | serialized variable |
| `Mailbox<'T>` | buffered mailbox |
| `BoundedMb<'T>` | bounded synchronous mailbox |
| `Latch` | countdown latch |
| `Lock` | mutual exclusion lock |
| `Stream<'T>`, `Src<'T>`, `Var<'T>` | choice streams |

Modules (`Job`, `Alt`, `Ch`, …) mirror Hopac’s operations and return wrapped values. Most functions accept anything with `ToHopac`, so mixed code can still convert in.

`Job` implements F#+ functor / applicative / monad members (`Map`, `Return`, `>>=`, `<*>`, `Delay`, …). `Alt` also implements alternative (`Empty`, `<|>`).

`open HopacPlus` brings in a `job { }` computation expression and top-level `run` / `start` / `queue` / `server`. `open HopacPlus.Infixes` for Hopac’s operators (`>>=`, `<|>`, `*<+`, `*<-`, …). `HopacPlus.Extensions` covers Seq / Array / Async / Task / Observable interop.

Hopac’s pairing operator `<*>` in `HopacPlus.Infixes` is not F#+ applicative apply. F#+ `apply` is the static member on `Job`.

## Example

```fsharp
open FSharpPlus
open HopacPlus
open HopacPlus.Infixes

let summed =
    monad {
        let! x = Job.result 1
        let! y = map ((+) 1) (Job.result 2)
        return x + y
    }

run summed // 4

let ch = Ch.create ()

let producer =
    job {
        do! ch *<+ 1
        do! ch *<+ 2
    }

let consumer =
    job {
        let! a = Ch.take ch
        let! b = Ch.take ch
        return a + b
    }

run (Job.startIgnore producer >>=. consumer) // 3
```

`job { }` binds `Job`, `Alt`, `Async`, `Task`, and `IObservable`. With F#+ you can also write `map`, `>>=`, and `monad { }` over `Job` and `Alt`.

## Build and test

Targets `netstandard2.1` and `net10.0`. Tests run with [Expecto](https://github.com/haf/expecto) (sequenced, because Hopac’s global scheduler is shared).

```bash
dotnet build
dotnet test
```

Pack (override the version with `-p:PackageVersion=`):

```bash
dotnet pack src/HopacPlus/HopacPlus.fsproj -c Release -p:PackageVersion=0.1.0
```

Publishing to nuget.org is handled by `.github/workflows/nuget.yml` when you publish a GitHub Release. The release tag is the package version (`v0.1.0` or `0.1.0`).

Preferred: add a [Trusted Publishing](https://learn.microsoft.com/nuget/nuget-org/trusted-publishing) policy on nuget.org for this repository and workflow file `nuget.yml` (environment `nuget`), then set GitHub Actions variable or secret `NUGET_USER` to your nuget.org profile name (not email).

Alternatively, set secret `NUGET_API_KEY` to a nuget.org API key.

## Benchmarks

Compare `HopacPlus.Job` with native `Hopac.Job`:

```bash
dotnet run -c Release --project benchmark/HopacPlus.Benchmark
```

## License

MIT. See [LICENSE](LICENSE).
