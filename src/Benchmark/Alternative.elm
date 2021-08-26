module Benchmark.Alternative exposing (rank)

{-| More ways to produce `Benchmark`s.

@docs rank

-}

import Benchmark exposing (Benchmark)


{-| Run multiple benchmarks and compare them.
This is useful when optimizing data structures or
other situations where you can make
apples-to-apples comparisons between different approaches.

    rank "initialize"
        (\f -> f 100 identity)
        [ ( "optimized", Array.Optimized.initialize )
        , ( "core", Array.initialize )
        ]

With the first argument you specify how you run the functions in the list.

-}
rank : String -> (f -> result_) -> List ( String, f ) -> Benchmark
rank name run benchmarks =
    Benchmark.scale name
        (benchmarks
            |> List.map
                (\( benchmarkName, f ) ->
                    ( benchmarkName
                    , \() -> run f
                    )
                )
        )
