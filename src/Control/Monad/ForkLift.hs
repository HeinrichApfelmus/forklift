{-----------------------------------------------------------------------------
    forklift
------------------------------------------------------------------------------}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
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
import Data.IORef hiding (readIORef, writeIORef)
import Control.Applicative
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
newtype ForkLift m = ForkLift (DrSommer m)

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
    dr <- newDrSommer
    let loop  = forever $ do
        Question ma var <- liftIO $ nextQuestion dr
        a <- ma
        liftIO $ atomically $ putTMVar var (Just a)
    forkIO $ unlift loop `finally` (closeOffice dr)
    return $ ForkLift dr

-- | The 'ForkLift' thread has died and is unable to return a result.
data ForkLiftBroken = ForkLiftBroken deriving (Show, Typeable)
instance Exception ForkLiftBroken

-- | Send an action to the 'ForkLift' thread and wait for the result.
--
-- Note: A 'ForkLiftBroken' exception will be thrown when the result
-- cannot be obtained because the 'ForkLift' thread has died.
carry :: MonadIO m => ForkLift m -> m a -> IO a
carry (ForkLift dr) action = do
    var <- newEmptyTMVarIO
    askQuestion dr $ Question action var
    ma <- atomically $ takeTMVar var
    case ma of
        Nothing -> throw ForkLiftBroken
        Just a  -> return a

-- | Send an action to the 'ForkLift' thread, but do not wait for the result.
carryAway :: ForkLift m -> m () -> IO ()
carryAway (ForkLift dr) action =
    askQuestion dr . Question action =<< newEmptyTMVarIO


{-----------------------------------------------------------------------------
    Utility functions for communication
------------------------------------------------------------------------------}
-- Dr. Sommer answers questions in a different thread.

data Question m = forall a. Question (m a) (TMVar (Maybe a))
data DrSommer m = DrSommer
    { latest  :: TVar (Maybe (Question m))
    , closed  :: TVar Bool
    , channel :: TChan (Question m)
    }

newDrSommer :: IO (DrSommer m)
newDrSommer = DrSommer <$> newTVarIO Nothing <*> newTVarIO False <*> newTChanIO

askQuestion :: DrSommer m -> Question m -> IO ()
askQuestion dr q = atomically $ do
    b <- readTVar (closed dr)
    if b
        then throw ForkLiftBroken
        else writeTChan (channel dr) q

nextQuestion :: DrSommer m -> IO (Question m)
nextQuestion dr = atomically $ do
    q <- readTChan (channel dr)
    writeTVar (latest dr) (Just q)
    return q

closeOffice :: DrSommer m -> IO ()
closeOffice dr = atomically $ do
        writeTVar (closed dr) True
        readTVar (latest dr) >>= maybe (return ()) (unGetTChan (channel dr))
        whileM (not <$> isEmptyTChan (channel dr)) $ 
            readTChan (channel dr) >>= closeQuestion
    where
    closeQuestion (Question _ answer) = putTMVar answer Nothing

whileM mb f = do b <- mb; when b (f >> whileM mb f)

