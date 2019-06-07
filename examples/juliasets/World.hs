module World where

data World =
  World { dimens   :: Int
        , time     :: Float
        , zoom     :: Float
        , speed    :: Float
        , simState :: SimState
        , offset   :: (Float, Float)
        }

data SimState = Normal | Paused
