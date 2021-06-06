module Benchmark.Reporting.Alternative exposing
    ( Report, Structure(..)
    , State, state
    )

{-| Alternative way of reporting the structure of a benchmark.

@docs Report, Structure

while `Running` or when `Finished`:

@docs State, state

-}

import Benchmark.Reporting as Report
import Benchmark.State as State exposing (Finished, State(..))
import Benchmark.Status.Alternative as Status


{-| Each tag of `Structure` has a name and some other information about the structure of a `Benchmark`.
-}
type Structure status
    = Single status
    | Group (List (Report status))
    | Series (List { status | name : String })


{-| Public version of Benchmarks.
-}
type alias Report status =
    { name : String
    , structure : Structure status
    }


{-| -}
type alias State =
    State.State (Report {}) (Report { result : Finished })


{-| Convert a `Benchmark.Reporting.Report` into a `Benchmark.Reporting.Alternative.State`.
-}
state : Report.Report -> State
state report =
    case report of
        Report.Single name reportStatus ->
            case reportStatus |> Status.state of
                Finished finished ->
                    { name = name
                    , structure = Single { result = finished }
                    }
                        |> Finished

                Running running _ ->
                    { name = name, structure = Single {} }
                        |> Running running

        Report.Group name reports ->
            case reports of
                [] ->
                    { name = name, structure = Group [] }
                        |> Finished

                head :: _ ->
                    case state head of
                        Finished _ ->
                            { name = name
                            , structure =
                                reports
                                    |> List.filterMap
                                        (\structure ->
                                            case state structure of
                                                Finished fin ->
                                                    Just fin

                                                Running _ _ ->
                                                    Nothing
                                        )
                                    |> Group
                            }
                                |> Finished

                        Running running _ ->
                            { name = name
                            , structure =
                                reports
                                    |> List.filterMap
                                        (\structure ->
                                            case state structure of
                                                Running _ result ->
                                                    Just result

                                                Finished _ ->
                                                    Nothing
                                        )
                                    |> Group
                            }
                                |> Running running

        Report.Series name series ->
            case series of
                [] ->
                    { name = name, structure = Series [] }
                        |> Finished

                ( _, headStatus ) :: _ ->
                    case Status.state headStatus of
                        Finished _ ->
                            { name = name
                            , structure =
                                series
                                    |> List.filterMap
                                        (\( subName, reportStatus ) ->
                                            case Status.state reportStatus of
                                                Finished finished ->
                                                    { name = subName, result = finished }
                                                        |> Just

                                                Running _ _ ->
                                                    Nothing
                                        )
                                    |> Series
                            }
                                |> Finished

                        Running running _ ->
                            { name = name
                            , structure =
                                series
                                    |> List.map
                                        (\( subName, _ ) -> { name = subName })
                                    |> Series
                            }
                                |> Running running
