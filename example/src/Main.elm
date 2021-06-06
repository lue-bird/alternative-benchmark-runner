module Main exposing (main)

import Array
import Benchmark exposing (Benchmark, benchmark, describe, scale)
import Benchmark.Alternative exposing (sort)
import Benchmark.Runner.Alternative exposing (program, programWith, defaultOptions, BenchmarkProgram, Theme(..))


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "example"
        [ describe "array operations"
            [ sort "range from 0"
                (\f -> f 100)
                [ ( "with initialize", from0WithInitialize )
                , ( "with List.range", from0WithListRange )
                , ( "with indexedMap", from0WithIndexedMap )
                ]
            , let
                list =
                    List.repeat 100 ()
              in
              benchmark "fromList"
                (\()-> Array.fromList list)
            ]
        , describe "list operations"
            [ scale "repeat"
                (List.range 1 6
                    |> List.map ((*) 10)
                    |> List.map
                        (\n ->
                            ( n |> String.fromInt
                            , \()-> List.repeat n ()
                            )
                        )
                )
            , scale "reverse"
                (List.range 1 6
                    |> List.map ((*) 10)
                    |> List.map
                        (\n-> ( n, List.range 0 n ))
                    |> List.map
                        (\( n, listOfN )->
                            ( n |> String.fromInt
                            , \()-> List.reverse listOfN
                            )
                        )
                )
            ]
        ]

from0WithInitialize length =
    Array.initialize length identity

from0WithListRange length =
    Array.fromList (List.range 0 (length - 1))

from0WithIndexedMap length =
    Array.repeat length ()
        |> Array.indexedMap (\i _ -> i)
