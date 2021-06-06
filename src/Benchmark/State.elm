module Benchmark.State exposing
    ( State(..)
    , Running(..)
    , Finished
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

@docs Finished

-}

import Benchmark.Status as Status
import Trend.Linear exposing (Quick, Trend)


{-| State of a benchmark: `Finished` or still `Running`.
-}
type State running finished
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
type alias Finished =
    Result Status.Error (Trend Quick)
