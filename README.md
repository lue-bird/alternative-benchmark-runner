## alternative-benchmark-runner

Extending [elm-explorations/benchmark](https://package.elm-lang.org/packages/elm-explorations/benchmark/latest/).

- compare _multiple_ benchmarks
- run in a cleaner interface 

![Benchmark example](https://raw.githubusercontent.com/lue-bird/alternative-benchmark-runner/master/benchmark-example.png)

- compatible with all your existing benchmarks, simply replace `Benchmark.Runner.program` with `Benchmark.Runner.Alternative.program`

```elm
import Benchmark exposing (describe)
import Benchmark.Alternative exposing (sort)
import Benchmark.Runner.Alternative as BenchmarkRunner


main =
    BenchmarkRunner.program suite

suite =
    describe "array"
        [ sort "range from 0"
            (\f -> f 100)
            [ ( "with initialize", from0WithInitialize )
            , ( "with List.range", from0WithListRange )
            , ( "with indexedMap", from0WithIndexedMap )
            ]
        ]

from0WithInitialize length =
    Array.initialize length identity

from0WithListRange length =
    Array.fromList (List.range 0 (length - 1))

from0WithIndexedMap length =
    Array.repeat length ()
        |> Array.indexedMap (\i _ -> i)
```

To customize the options:

```elm
import Benchmark.Runner.Alternative as BenchmarkRunner exposing (defaultOptions, lightTheme)

main =
    BenchmarkRunner.programWith
        { defaultOptions | theme = lightTheme }
        suite
```
- dark and light mode
- allows you to write your own render function
