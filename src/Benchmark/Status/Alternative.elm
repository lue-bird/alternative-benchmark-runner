module Benchmark.Status.Alternative exposing
    ( fromReport, fromBenchmark
    , Status(..)
    , Running(..)
    , Result
    , runsPerSecond, results
    , Structure, StructureKind(..)
    )

{-| Alternative way of representing the status of a benchmark.

@docs fromReport, fromBenchmark

Instead of

    type Status
        = Cold
        | Unsized
        | Pending Int Samples
        | Failure Error
        | Success Samples (Trend Quick)

the status is

@docs Status


## when `Running`

@docs Running


## when `Finished`

@docs Result


### utils

@docs runsPerSecond, results


## structure

@docs Structure, StructureKind

-}

import Benchmark exposing (Benchmark)
import Benchmark.Reporting as Report
import Benchmark.Status as Status
import Trend.Linear as Trend exposing (Quick, Trend)


{-| State of a benchmark: `Finished` or still `Running`.
-}
type Status
    = Running Running (Structure {})
    | Finished (Structure { result : Result })


{-| Status of a running benchmark.

  - `WarmingJit`: We have not warmed up the JIT yet.
  - `FindingSampleSize`: We have not yet determined the best sample size for this benchmark.
  - `CollectingSamples`: We are in the process of collecting sample data. We will keep collecting sample data until we have enough samples. The argument (between 0 and 1) shows the progress.

-}
type Running
    = WarmingJit
    | FindingSampleSize
    | CollectingSamples Float


{-| Information about a finished benchmark.

  - `Ok`: We finished collecting all our sample data and calculated a trend using this data.
  - `Err`: We ran into an exception while collecting sample data. The attached [`Error`](https://package.elm-lang.org/packages/elm-explorations/benchmark/latest/Benchmark-Status#Error) tells us what went wrong.

-}
type alias Result =
    Result.Result Status.Error (Trend Quick)


{-| Predict the amount of runs / 1 second.
-}
runsPerSecond : Trend a_ -> Float
runsPerSecond trend =
    Trend.predictX (Trend.line trend) 1000


{-| Collect all results.

    lowestGoodnessOfFit =
        results
            >> List.filterMap Result.toMaybe
            >> List.map Trend.goodnessOfFit
            >> List.minimum
            >> Maybe.withDefault 1

    errors =
        results
            >> List.filterMap
                (\result ->
                    case result of
                        Err err ->
                            Just err

                        Ok _ ->
                            Nothing
                )

-}
results : Structure { result : Result } -> List Result
results finished =
    case finished.structureKind of
        Single { result } ->
            [ result ]

        Group group ->
            group |> List.concatMap results

        Series series ->
            series |> List.map .result


{-| The structure type of a specific `Benchmark`.

  - `Single`: `Benchmark.benchmark`
  - `Series`: `Benchmark.compare`, `Benchmark.scale`, [`Benchmark.Alternative.rank`](Benchmark-Alternative#rank)
  - `Group`: `Benchmark.describe`

–

  - when running: `StructureKind {}`
  - when finished: `StructureKind { result : Result }`, see [`Result`](#Result)

-}
type StructureKind status
    = Single status
    | Group (List (Structure status))
    | Series (List { status | name : String })


{-| The name and structure of a specific `Benchmark`.

  - when running: `Structure {}`
  - when finished: `Structure { result : Result }`, see [`Result`](#Result)

-}
type alias Structure status =
    { name : String
    , structureKind : StructureKind status
    }


{-| Report the [`Status`](#Status) of a benchmark. Short for

    benchmark
        |> Report.fromBenchmark
        |> Benchmark.Status.Alternative.fromReport

-}
fromBenchmark : Benchmark -> Status
fromBenchmark benchmark =
    Report.fromBenchmark benchmark
        |> fromReport


{-| Convert a `Benchmark.Reporting.Report` to a [`Benchmark.Status.Alternative.Status`](#Status).
-}
fromReport : Report.Report -> Status
fromReport report =
    let
        -- Ok if a result exists, Err if still running
        toOkFinishedOrErrRunning :
            Status.Status
            -> Result.Result Running Result
        toOkFinishedOrErrRunning status =
            case status of
                Status.Success _ trend ->
                    Ok trend |> Ok

                Status.Failure error ->
                    Err error |> Ok

                Status.Cold ->
                    WarmingJit |> Err

                Status.Unsized ->
                    FindingSampleSize |> Err

                Status.Pending baseSampleSize samples ->
                    let
                        progress =
                            Status.progress
                                (Status.Pending baseSampleSize samples)
                    in
                    CollectingSamples progress |> Err

        withName : String -> StructureKind status -> Structure status
        withName name kind =
            { name = name, structureKind = kind }
    in
    case report of
        Report.Single name reportStatus ->
            case reportStatus |> toOkFinishedOrErrRunning of
                Ok finished ->
                    Single { result = finished }
                        |> withName name
                        |> Finished

                Err running ->
                    Single {}
                        |> withName name
                        |> Running running

        Report.Group name reports ->
            case reports of
                [] ->
                    Group []
                        |> withName name
                        |> Finished

                head :: _ ->
                    case fromReport head of
                        Finished _ ->
                            reports
                                |> List.filterMap
                                    (\structure ->
                                        case fromReport structure of
                                            Finished finished ->
                                                Just finished

                                            Running _ _ ->
                                                Nothing
                                    )
                                |> Group
                                |> withName name
                                |> Finished

                        Running running _ ->
                            reports
                                |> List.filterMap
                                    (\structure ->
                                        case fromReport structure of
                                            Running _ runningStructure ->
                                                Just runningStructure

                                            Finished _ ->
                                                Nothing
                                    )
                                |> Group
                                |> withName name
                                |> Running running

        Report.Series name series ->
            case series of
                [] ->
                    Series []
                        |> withName name
                        |> Finished

                ( _, headStatus ) :: _ ->
                    case toOkFinishedOrErrRunning headStatus of
                        Ok _ ->
                            series
                                |> List.filterMap
                                    (\( subName, status ) ->
                                        case toOkFinishedOrErrRunning status of
                                            Ok finished ->
                                                { name = subName, result = finished }
                                                    |> Just

                                            Err _ ->
                                                Nothing
                                    )
                                |> Series
                                |> withName name
                                |> Finished

                        Err running ->
                            series
                                |> List.map
                                    (\( subName, _ ) -> { name = subName })
                                |> Series
                                |> withName name
                                |> Running running
