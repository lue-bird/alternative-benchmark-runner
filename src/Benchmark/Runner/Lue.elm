module Benchmark.Runner.Lue exposing
    ( Program, program
    , Options, programWith, defaultOptions, Theme, darkTheme, lightTheme
    , progressBenchmark
    )

{-| Run benchmarks in the browser.

@docs Program, program


## options

@docs Options, programWith, defaultOptions, Theme, darkTheme, lightTheme


## writing your own

@docs progressBenchmark

-}

import Benchmark exposing (Benchmark)
import Benchmark.Runner.Humanize as Humanize
import Benchmark.Status.Lue as Status exposing (Running(..), Status(..), StructureKind(..), StructureStatus, runsPerSecond, secondsPerRun)
import Browser
import Element.WithContext as Ui
import Element.WithContext.Background as Background
import Element.WithContext.Font as Font
import Element.WithContext.Input as UiInput
import Process
import Task exposing (Task)
import Trend.Linear as Trend exposing (Quick, Trend)


type alias Model =
    { suite : Benchmark
    }


{-| -}
type alias Program =
    Platform.Program () Model Msg


{-| Run benchmarks with [`defaultOptions`](Benchmark.Runner.Alternative#defaultOptions).
-}
program : Benchmark -> Program
program suite =
    programWith defaultOptions suite


{-| Run benchmarks with custom [`Options`](Benchmark.Runner.Alternative#Options).

    main : BenchmarkProgram
    main =
        programWith { defaultOptions | theme = Light }

-}
programWith : Options -> Benchmark -> Program
programWith options suite =
    Browser.document
        { init =
            \_ ->
                ( { suite = suite }
                , Task.succeed suite
                    |> Task.perform BenchmarkProgress
                )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = viewDocument options
        }


{-| Opions to start the [`BenchmarkProgram`](Benchmark.Runner.Alternative#BenchmarkProgram) with:

  - `theme :`[`Theme`](Benchmark.Runner.Alternative#Theme).

-}
type alias Options =
    { theme : Theme
    , view : Report.Status -> Ui.Element Context Msg
    }


type alias Context =
    { theme : Theme
    }


{-| `{ theme = Dark }` and default rendering.
-}
defaultOptions : Options
defaultOptions =
    { theme = darkTheme
    , view = view
    }


{-| Color theme.
-}
type alias Theme =
    { background : Ui.Color
    , foreground : Ui.Color
    }


darkTheme : Theme
darkTheme =
    { background = Ui.rgb 0 0 0
    , foreground = Ui.rgb 1 1 1
    }


lightTheme : Theme
lightTheme =
    { background = Ui.rgb 1 1 1
    , foreground = Ui.rgb 0 0 0
    }


type Msg
    = BenchmarkProgress Benchmark


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BenchmarkProgress updatedSuite ->
            ( { model | suite = updatedSuite }
            , progressBenchmark updatedSuite
            )


breakForRender : Task x a -> Task x a
breakForRender task =
    Task.andThen (\_ -> task) (Process.sleep 0)


progressBenchmark : Benchmark -> Cmd Msg
progressBenchmark benchmark =
    if Benchmark.done benchmark then
        Cmd.none

    else
        Benchmark.step benchmark
            |> breakForRender
            |> Task.perform BenchmarkProgress


viewDocument : Options -> Model -> Browser.Document Msg
viewDocument { theme } { suite } =
    { title = "benchmarks"
    , body =
        [ let
            suiteReport =
                suite |> Report.fromBenchmark |> Report.fromReport
          in
          view suiteReport
            |> Ui.layout
                { theme = theme }
                [ Ui.withAttribute (.theme >> .background) Background.color
                , Ui.withAttribute (.theme >> .foreground) Font.color
                ]
        ]
    }


view : Report.Status -> Ui.Element Context msg
view status =
    [ Ui.column []
        [ Ui.text "benchmark report"
            |> Ui.el [ Font.size 32 ]
        , case status of
            Running running _ ->
                viewRunningStatus running

            Finished _ ->
                Ui.none
        ]
    , viewStructure status
    ]
        |> Ui.column
            [ Ui.paddingXY 40 45
            , Ui.spacing 20
            ]


viewStructure : Report.Status -> Ui.Element Context msg
viewStructure report =
    case report of
        Running _ running ->
            viewRunningStructure running

        Finished finished ->
            viewFinishedStructure finished


viewFinishedStructure : Report Finished -> Ui.Element Context msg
viewFinishedStructure finished =
    case finished.structure of
        Group group ->
            viewGroup finished.name
                (group |> List.map viewFinishedStructure)

        Single status ->
            viewFinishedSingle finished.name status

        Series series ->
            viewFinishedSeries finished.name series


viewRunningStructure : Report {} -> Ui.Element Context msg
viewRunningStructure running =
    case running.structure of
        Group group ->
            viewGroup running.name
                (group |> List.map viewRunningStructure)

        Single _ ->
            viewRunningSingle running.name

        Series series ->
            viewRunningSeries running.name series


viewRunningSingle : String -> Ui.Element context msg
viewRunningSingle name =
    viewHeadline name
        |> Ui.el [ Ui.paddingXY 0 4 ]


viewFinishedSingle : String -> Finished -> Ui.Element context msg
viewFinishedSingle name finished =
    let
        viewSuccess trend =
            { data = [ () ] -- 1 row below the headers
            , columns =
                [ { header = viewHeadline name
                  , width = Ui.shrink
                  , view = \_ -> Ui.none
                  }
                , { header = viewInfoHeader "runs / second"
                  , width = Ui.shrink
                  , view =
                        \_ ->
                            (runsPerSecond trend |> floor |> Humanize.int)
                                |> Ui.text
                  }
                , { header = viewInfoHeader "goodness of fit"
                  , width = Ui.shrink
                  , view =
                        \_ ->
                            Trend.goodnessOfFit trend
                                |> Humanize.percent
                                |> Ui.text
                  }
                ]
            }
                |> Ui.table [ Ui.spacingXY 10 4 ]
    in
    (case finished of
        Ok trend ->
            viewSuccess trend

        Err _ ->
            Ui.text "Failed!"
    )
        |> Ui.el
            [ Ui.paddingXY 0 4 ]


viewRunningStatus : Status.Running -> Ui.Element Context msg
viewRunningStatus runningStatus =
    let
        viewInfo info =
            Ui.text info
                |> Ui.el [ Font.size 18 ]
    in
    case runningStatus of
        WarmingJit ->
            viewInfo "Warming JIT"

        FindingSampleSize ->
            viewInfo "Finding sample size"

        CollectingSamples progress ->
            Ui.row [ Ui.spacing 10 ]
                [ viewInfo "collecting samples"
                , viewRelation progress
                    [ Ui.width (Ui.px 110), Ui.height Ui.fill ]
                ]


viewGroup :
    String
    -> List (Ui.Element Context msg)
    -> Ui.Element Context msg
viewGroup name structures =
    Ui.column [ Ui.spacing 12 ]
        [ viewHeadline name
        , structures
            |> Ui.column
                [ Ui.paddingXY 26 0, Ui.spacing 13 ]
        ]


viewFinishedSeries :
    String
    -> List { name : String, result : Status.Result }
    -> Ui.Element Context msg
viewFinishedSeries name series =
    let
        successes =
            series
                |> List.map
                    (\sub ->
                        case sub.result of
                            Ok trend ->
                                Just { name = sub.name, trend = trend }

                            Err _ ->
                                Nothing
                    )
                |> List.sortBy (.trend >> secondsPerRun)

        failures =
            series
                |> List.filterMap
                    (\sub ->
                        case sub.result of
                            Err _ ->
                                Just sub.name

                            Ok _ ->
                                Nothing
                    )
    in
    [ { data = successes
      , columns =
            [ { header = viewHeadline name
              , width = Ui.shrink
              , view =
                    \sub ->
                        Ui.text sub.name
                            |> Ui.el [ Ui.alignBottom ]
              }
            , let
                maxSecondPerRun =
                    List.maximum
                        (successes
                            |> List.map secondsPerRun
                        )
                        |> Maybe.withDefault 0
              in
              { header = viewInfoHeader "time / run"
              , width = Ui.minimum 130 Ui.shrink
              , view =
                    \{ trend } ->
                        viewRelation
                            (secondsPerRun trend / maxSecondPerRun)
                            [ Ui.width Ui.fill, Ui.height Ui.fill ]
              }
            , { header = viewInfoHeader "runs / second"
              , width = Ui.shrink
              , view =
                    \{ trend } ->
                        (runsPerSecond trend |> floor |> Humanize.int)
                            |> Ui.text
              }
            , { header = viewInfoHeader "goodness of fit"
              , width = Ui.shrink
              , view =
                    \{ trend } ->
                        Trend.goodnessOfFit trend
                            |> Humanize.percent
                            |> Ui.text
              }
            ]
      }
        |> Ui.table [ Ui.spacingXY 16 6 ]
    , { data = failures
      , columns =
            [ \subName -> Ui.text subName
            , \_ -> Ui.text "Failed!"
            ]
                |> List.map
                    (\view_ ->
                        { header = Ui.none
                        , width = Ui.shrink
                        , view = view_
                        }
                    )
      }
        |> Ui.table [ Ui.spacingXY 16 6 ]
    ]
        |> Ui.row []


viewRunningSeries :
    String
    -> List { name : String }
    -> Ui.Element context msg
viewRunningSeries name series =
    Ui.column [ Ui.spacing 8 ]
        [ viewHeadline name
        , series
            |> List.map (.name >> Ui.text)
            |> Ui.column [ Ui.paddingXY 20 0 ]
        ]


{-| View a percentage as a horizontal bar. The argument must be between 0 and 1.

    halfHalf =
        viewRelation 0.5
            [ width fill, height fill ]

-}
viewRelation :
    Float
    -> List (Ui.Attribute Context msg)
    -> Ui.Element Context msg
viewRelation percent attrs =
    let
        per100 =
            percent * 100 |> floor

        bar width =
            Ui.el
                [ Ui.withAttribute (.theme >> .foreground) Background.color
                , Ui.height Ui.fill
                , Ui.width width
                ]
                Ui.none
    in
    [ bar (Ui.fillPortion per100)
        |> Ui.el [ Ui.alpha 0.6 ]
    , bar (Ui.fillPortion (100 - per100))
        |> Ui.el [ Ui.alpha 0.13 ]
    ]
        |> Ui.row attrs


viewInfoHeader : String -> Ui.Element context msg
viewInfoHeader name =
    Ui.text name
        |> Ui.el
            [ Font.size 17
            , Ui.paddingXY 0 3
            ]


viewHeadline : String -> Ui.Element context msg
viewHeadline name =
    name
        |> Ui.text
        |> Ui.el [ Font.size 23 ]
