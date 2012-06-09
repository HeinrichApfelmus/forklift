{-----------------------------------------------------------------------------
    forklift
------------------------------------------------------------------------------}
{-# LANGUAGE DeriveDataTypeable #-}
module Control.Monad.ForkLift (
    -- * Synopsis
    -- $intro
    
    -- * Documentation
    ForkLift, newForkLift,
    carry, carryAway,
    ForkLiftBroken(..),
    
    -- * Examples
    -- $example
    ) where

import Data.Dynamic
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

{-----------------------------------------------------------------------------
    Documentation
------------------------------------------------------------------------------}
{-$intro

Perform arbitrary monadic computations in a worker thread.

See also <http://apfelmus.nfshost.com/blog/2012/06/07-forklift.html> .


-}
{-$example

Example using the state monad transformer.

@
    import Control.Monad.ForkLift
    import Control.Monad.Trans.State -- from the transformers package
@

@
    example :: IO ()
    example = do
        state <- newForkLift (flip evalStateT (0::Int))
        carry state $ modify (+10)
        carry state $ modify (+10)
        print =<< carry state get
@

>>> example
20

-}

{-----------------------------------------------------------------------------
    Code
------------------------------------------------------------------------------}
-- | A worker thread that evaluates computations from the monad @m@ in sequence.
newtype ForkLift m = ForkLift (MyChan (m ()))

-- | Create a new 'ForkLift'.
-- This will a spawn a new worker thread which
-- receives monadic computations @m a@ and evaluates them in sequence.
--
-- You have to specify a method for evaluating
-- the monad @m@ in the first argument.
--
-- > newForkLift unlift = forkIO $ unlift $ forever ...
--
-- Note: The thread will stop when the ForkLift is garbage collected.
newForkLift :: MonadIO m => (m () -> IO ()) -> IO (ForkLift m)
newForkLift unlift = do
    channel <- newMyChanIO
    let loop  = forever . join . liftIO $ readMyChan channel
    mask $ \restore -> forkIO $ do
        (restore $ unlift loop) `finally` (closeMyChan channel)
    return $ ForkLift channel


-- | The 'ForkLift' thread has died and is unable to return a result.
data ForkLiftBroken = ForkLiftBroken deriving (Show, Typeable)
instance Exception ForkLiftBroken

-- | Send an action to the 'ForkLift' thread and wait for the result.
--
-- Note: A 'ForkLiftBroken' exception will be thrown when the result
-- cannot be obtained because the 'ForkLift' thread has died.
carry :: MonadIO m => ForkLift m -> m a -> IO a
carry (ForkLift channel) action = do
    ref <- newEmptyMVar
    
    -- throws an exception when the channel is not available
    writeMyChan channel $ liftIO . putMVar ref =<< action
    
    -- throws an exception when the channel is closed
    -- and hence the MVar is garbage collected
    mapException (\BlockedIndefinitelyOnMVar -> ForkLiftBroken) $
        takeMVar ref

-- | Send an action to the 'ForkLift' thread, but do not wait for the result.
carryAway :: ForkLift m -> m () -> IO ()
carryAway (ForkLift channel) action = writeMyChan channel action

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
-- a concurrent channel that can be closed
type MyChan a = TVar (Maybe (TChan a))

newMyChanIO :: IO (MyChan a)
newMyChanIO = newTVarIO =<< liftM Just newTChanIO

writeMyChan :: MyChan a -> a -> IO ()
writeMyChan myc x = do
    err <- atomically $ do
        mc <- readTVar myc
        case mc of
            Nothing -> return True
            Just c  -> do writeTChan c x; return False
    when err $ throw ForkLiftBroken

readMyChan  :: MyChan a -> IO a
readMyChan myc = atomically $ (\(Just c) -> readTChan c) =<< readTVar myc

closeMyChan :: MyChan a -> IO ()
closeMyChan myc = atomically $ writeTVar myc Nothing

