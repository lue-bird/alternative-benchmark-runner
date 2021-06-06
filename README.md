## alternative-benchmark-runner

Extending [elm-explorations/benchmark](https://package.elm-lang.org/packages/elm-explorations/benchmark/latest/).

Extra features:
- compare multiple benchmarks
- cleaner interface
- dark mode option

![Benchmark example](https://raw.githubusercontent.com/lue-bird/alternative-benchmark-runner/master/benchmark-example.png)

```elm
module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Alternative exposing (sort)
import Benchmark.Runner.Alternative as BenchmarkRunner


main : BenchmarkRunner.Program
main =
    BenchmarkRunner.program suite

suite : Benchmark
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

You can also add options:

```elm
import Benchmark.Runner.Alternative as BenchmarkRunner exposing (defaultOptions, Theme(..))


main : BenchmarkRunner.Program
main =
    BenchmarkRunner.programWith
        { defaultOptions | theme = Light }
        suite
```

to the point that you can write your own render function.

```elm
import Element
import Benchmark.State exposing (State(..))
import Benchmark.Reporting.Alternative exposing (Structure(..))

main =
    programWith
        { defaultOptions
            | theme = Light
            , view = view
        }
        suite

view state =
    case state of
        Running _ _ ->
            Element.text "running benchmarks..."

        Finished finished ->
            viewFinished finished

viewFinished finished =
    case finished.structure of
        Group group ->
            Element.column [ Ui.spacing 6 ]
                [ Element.text finisnished.name
                , Element.column [ Ui.spacing 4 ]
                    (group |> List.map viewFinished)
                ]

        Single { result } ->
            case result of
                Ok trend ->
                    Ui.row [ Ui.spacing 6 ]
                        [ Ui.text finished.name
                        , Ui.text
                            (runsPerSecond trend
                                |> String.fromFloat
                            )
                        ]

                Err _ ->
                    Element.text "Failed!"

        --...
```
