## alternative-benchmark-runner

Extra features:
- compare multiple benchmarks
- toggle dark mode
- cleaner interface

![Benchmark example](https://raw.githubusercontent.com/lue-bird/alternative-benchmark-runner/master/benchmark-example.png)

```elm
module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (sort)
import Benchmark.Runner.Alternative exposing (BenchmarkProgram, program)


main : BenchmarkProgram
main =
    program suite

suite : Benchmark
suite =
    describe "array"
        [ sort "range from 0"
            (\f -> f 100)
            [ ( "with initialize"
              , \length -> Array.initialize length identity
              )
            , ( "with List.range", increasingWithListRange )
            , ( "with indexedMap", increasingWithIndexedMap )
            ]
        ]

increasingWithListRange length =
    Array.fromList (List.range 0 (length - 1))

increasingWithIndexedMap length =
    Array.repeat length ()
        |> Array.indexedMap (\i _ -> i)
```

You could also add options:

```elm
import Benchmark.Runner.Alternative exposing (BenchmarkProgram, programWith, defaultOptions, Theme(..))


main : BenchmarkProgram
main =
    programWith { defaultOptions | theme = Light }
```

