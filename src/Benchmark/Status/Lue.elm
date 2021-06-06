module Benchmark.Status.Lue exposing
    ( Running(..)
    , Result
    , runsPerSecond, secondsPerRun
    , StructureStatus, fromReport
    , Status, StructureKind(..)
    )

{-| Alternative way of representing the status of a benchmark.

Instead of

    type Status
        = Cold
        | Unsized
        | Pending Int Samples
        | Failure Error
        | Success Samples (Trend Quick)

the status is split into

@docs State

The `Running` variant contains more specific info:

@docs Running

And `Finished` can contain

@docs Result

while `Running` or when we have a `Result`:

@docs State, state


## finished state

@docs runsPerSecond, secondsPerRun


## structure

@docs Structure, StructureStatus, fromReport

-}

import Benchmark.Reporting as Report
import Benchmark.Status as Status
import Trend.Linear as Trend exposing (Quick, Trend)


{-| State of a benchmark: `Finished` or still `Running`.
-}
type Status running finished
    = Running Running running
    | Finished finished


{-| Status of a running benchmark.

  - `WarmingJit`: We have not warmed up the JIT yet.
  - `FindingSampleSize`: We have not yet determined the best sample size for this benchmark.
  - `CollectingSamples`: We are in the process of collecting sample data. We will keep collecting sample data until we have enough samples. The argument (between 0 and 1) shows the progress.

-}
type Running
    = WarmingJit
    | FindingSampleSize
    | CollectingSamples Float


{-| Result of a finished benchmark.

  - `Ok`: We finished collecting all our sample data and calculated a trend using this data.
  - `Err`: We ran into an exception while collecting sample data. The attached `Error` tells us what went wrong.

-}
type alias Result =
    Result.Result Status.Error (Trend Quick)


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


{-| The structure of a specific `Benchmark` status.
-}
type StructureKind status
    = Single status
    | Group (List (Structure (StructureKind status)))
    | Series (List { status | name : String })


type alias Structure kind =
    { name : String
    , structure : kind
    }


{-| The structure & status of a `Benchmark`.
-}
type alias StructureStatus =
    Structure
        (Status (StructureKind {}) (StructureKind { result : Result }))


{-| Convert a `Benchmark.Reporting.Report` into a `Benchmark.Status.Lue.StructureStatus`.
-}
fromReport : Report.Report -> StructureStatus
fromReport report =
    let
        fromStatus : Status.Status -> Status () Result
        fromStatus status =
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
    in
    case report of
        Report.Single name reportStatus ->
            { name = name
            , structure =
                case reportStatus |> fromStatus of
                    Finished finished ->
                        Single { result = finished }
                            |> Finished

                    Running running _ ->
                        Single {} |> Running running
            }

        Report.Group name reports ->
            { name = name
            , structure =
                case reports of
                    [] ->
                        Group [] |> Finished

                    head :: _ ->
                        case fromReport head |> .structure of
                            Finished _ ->
                                reports
                                    |> List.filterMap
                                        (\structure ->
                                            let
                                                status =
                                                    fromReport structure
                                            in
                                            case status.structure of
                                                Finished finished ->
                                                    Just { name = status.name, structure = finished }

                                                Running _ _ ->
                                                    Nothing
                                        )
                                    |> Group
                                    |> Finished

                            Running running _ ->
                                reports
                                    |> List.filterMap
                                        (\structure ->
                                            let
                                                status =
                                                    fromReport structure
                                            in
                                            case status.structure of
                                                Running _ runningStructure ->
                                                    { name = status.name, structure = runningStructure }
                                                        |> Just

                                                Finished _ ->
                                                    Nothing
                                        )
                                    |> Group
                                    |> Running running
            }

        Report.Series name series ->
            { name = name
            , structure =
                case series of
                    [] ->
                        Series [] |> Finished

                    ( _, headStatus ) :: _ ->
                        case fromStatus headStatus of
                            Finished _ ->
                                series
                                    |> List.filterMap
                                        (\( subName, reportStatus ) ->
                                            case fromStatus reportStatus of
                                                Finished finished ->
                                                    { name = subName, result = finished }
                                                        |> Just

                                                Running _ _ ->
                                                    Nothing
                                        )
                                    |> Series
                                    |> Finished

                            Running running _ ->
                                series
                                    |> List.map
                                        (\( subName, _ ) -> { name = subName })
                                    |> Series
                                    |> Running running
            }
