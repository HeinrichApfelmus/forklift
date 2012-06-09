{-----------------------------------------------------------------------------
    forklift
    
    Some quick tests.
------------------------------------------------------------------------------}
import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.ForkLift
import Control.Monad.Trans.Maybe

test = do
    m <- newForkLift (void . runMaybeT)
    carry m $ liftIO $ putStrLn "Good"
    threadDelay (10^5)
    -- using  carry  here does *not* work!
    carryAway m $ MaybeT (return Nothing)
    threadDelay (10^5)
    carry m $ liftIO $ putStrLn "Bad"

void = (>> return ())

catchMVar = handle (\BlockedIndefinitelyOnMVar -> putStrLn "waiting")