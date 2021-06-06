module Benchmark.Status.Alternative exposing
    ( State, state
    , runsPerSecond, secondsPerRun
    )

{-| Alternative way of representing the status of a benchmark.

while `Running` or when `Finished`:

@docs State, state


## finished state

@docs runsPerSecond, secondsPerRun

-}

import Benchmark.State as State exposing (Finished, Running(..), State(..))
import Benchmark.Status as Status
import Trend.Linear as Trend exposing (Trend)


{-| -}
type alias State =
    State.State () Finished


{-| Predict the amount of runs / 1 second.
-}
runsPerSecond : Trend a -> Float
runsPerSecond trend =
    Trend.predictX (Trend.line trend) 1000


{-| Predict the amount of seconds / 1 run.
-}
secondsPerRun : Trend a -> Float
secondsPerRun trend =
    1 / runsPerSecond trend


{-| Convert a `Benchmark.Status.Status` into a `Benchmark.Status.Alternative.Status`.
-}
state : Status.Status -> State
state status =
    case status of
        Status.Success _ trend ->
            Ok trend |> Finished

        Status.Failure error ->
            Err error |> Finished

        Status.Cold ->
            Running WarmingJit ()

        Status.Unsized ->
            Running FindingSampleSize ()

        Status.Pending baseSampleSize samples ->
            let
                progress =
                    Status.progress
                        (Status.Pending baseSampleSize samples)
            in
            Running (CollectingSamples progress) ()
