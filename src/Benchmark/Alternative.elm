module Benchmark.Alternative exposing (sort)

{-| More ways to produce `Benchmark`s.

@docs sort

-}

import Benchmark exposing (Benchmark)


{-| Run multiple benchmarks and compare them to one benchmark.
This is useful when optimizing data structures or
other situations where you can make
apples-to-apples comparisons between different approaches.

    sort "initialize"
        (\f -> f 100 identity)
        [ ( "optimized", Array.Optimized.initialize )
        , ( "Core", Array.initialize )
        ]

-}
sort : String -> (f -> result) -> List ( String, f ) -> Benchmark
sort name run benchmarks =
    Benchmark.scale name
        (benchmarks
            |> List.map
                (\( benchmarkName, f ) ->
                    ( benchmarkName
                    , \() -> run f
                    )
                )
        )
