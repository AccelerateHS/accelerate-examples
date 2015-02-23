--
-- An N-Body simulation
--

-- friends
import Test
import Config
import Common.Body
import Common.World
import Common.Type
import Gloss.Draw
import Gloss.Event
import Gloss.Simulate
import Random.Position
import qualified Solver.Naive1                          as Naive1
import qualified Solver.Naive2                          as Naive2
import qualified Solver.BarnsHut                        as BarnsHut

import Data.Array.Accelerate                            as A hiding ( size )
import Data.Array.Accelerate.Examples.Internal          as A

import Data.Array.Accelerate.Debug 

-- system
import Prelude                                          as P
import Data.Label
import System.Environment
import Graphics.Gloss.Interface.Pure.Game


main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)      <- parseArgs options defaults header footer argv

--        setFlags [dump_gc, dump_cc, verbose] 

        let solver      = case get configSolver conf of
                            Naive1      -> Naive1.calcAccels
                            Naive2      -> Naive2.calcAccels
                            BarnsHut    -> BarnsHut.calcAccels

            n           = get configBodyCount conf
            size        = get configWindowSize conf
            fps         = get configRate conf
            epsilon     = get configEpsilon conf
            mass        = get configBodyMass conf
            radius      = get configStartDiscSize conf
            backend     = get optBackend opts

            duped       = get configDuped conf 
        
            -- Generate random particle positions in a disc layout centred at
            -- the origin. Start the system rotating with particle speed
            -- proportional to distance from the origin
            --
            positions   = randomArray (cloud (size,size) radius) (Z :. n)
            masses      = randomArray (uniformR (1, mass)) (Z :. n)
            positions2  = randomArray (cloud (size,size) radius) (Z :. n)
            masses2     = randomArray (uniformR (1, mass)) (Z :. n)
            positions3  = randomArray (cloud (size,size) radius) (Z :. n)
            masses3     = randomArray (uniformR (1, mass)) (Z :. n)

        
        
            bodies      = run backend
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses)
                        $ A.map unitBody (A.use positions)
                        
            bodies2     = run backend
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses2)
                        $ A.map unitBody (A.use positions2)

            bodies3     = run backend
                        $ A.map (setStartVelOfBody . constant $ get configStartSpeed conf)
                        $ A.zipWith setMassOfBody (A.use masses3)
                        $ A.map unitBody (A.use positions3)


            -- The initial simulation state
            --
            universe    = initialise world
            world       = World { worldBodies   = bodies
                                , worldSteps    = 0
                                , worldTime     = 0 }

            -- Advancing the simulation
            --
            advance     = advanceWorld step
            step        = P.curry
                        $ run1 backend
                        $ A.uncurry
                        $ advanceBodies (solver $ constant epsilon)

            dupedStep   = run1 backend
                        $ \x -> let (a1,a2) = unlift x
                                in lift (f_ a1, f_ a2)

            undupedStep = run1 backend f_ 
        

            f_ = advanceBodies (solver $ constant epsilon) (use (fromList Z [0.1::Time]))
                        
        -- Forward unto dawn
        --
        runTests opts rest
          $ makeTests step

        runBenchmarks opts rest
          [ case duped of
               False -> bench "n-body-unduped"       $ whnf undupedStep bodies3
               True -> bench "n-body-duped" $ whnf (dupedStep) (bodies, bodies2) ]
          

        runInteractive opts rest
          $ play
              (InWindow "N-Body" (size, size) (10, 10))         -- window size & position
              black                                             -- background colour
              fps                                               -- number of simulation steps per second
              universe                                          -- initial world
              (draw conf)                                       -- fn to convert a world into a picture
              react                                             -- fn to handle input events
              (simulate advance)                                -- fn to advance the world

