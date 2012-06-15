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
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import System.Mem.Weak

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
        liftIO $ putMVar var (Just a)
    forkIO $ unlift loop `finally` closeOffice dr
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
    var <- newEmptyMVar
    askQuestion dr $ Question action var
    ma <- takeMVar var
    case ma of
        Nothing -> throw ForkLiftBroken
        Just a  -> return a

-- | Send an action to the 'ForkLift' thread, but do not wait for the result.
carryAway :: ForkLift m -> m () -> IO ()
carryAway (ForkLift dr) action =
    askQuestion dr . Question action =<< newEmptyMVar


{-----------------------------------------------------------------------------
    Utility functions for communication
------------------------------------------------------------------------------}
-- Dr. Sommer answers questions in a different thread.

data Question m = forall a. Question (m a) (MVar (Maybe a))
data DrSommer m = DrSommer
    { latest  :: IORef (Maybe (Question m))
    , closed  :: IORef Bool
    , channel :: Chan (Question m)
    }

-- concurrent IORef manipulation
readIORef  ref   = atomicModifyIORef ref (\x -> (x,x ))
writeIORef ref x = atomicModifyIORef ref (\_ -> (x,()))

newDrSommer :: IO (DrSommer m)
newDrSommer = DrSommer <$> newIORef Nothing <*> newIORef False <*> newChan

askQuestion :: DrSommer m -> Question m -> IO ()
askQuestion dr q = do
    b <- readIORef (closed dr)
    if b
        then throw ForkLiftBroken
        else writeChan (channel dr) q
        -- race condition: question written to channel though it won't be cleared

nextQuestion :: DrSommer m -> IO (Question m)
nextQuestion dr = do
    q <- readChan (channel dr)
    writeIORef (latest dr) (Just q)
    return q

closeOffice :: DrSommer m -> IO ()
closeOffice dr = do
        print "Closing!"
        writeIORef (closed dr) True
        readIORef (latest dr) >>= maybe (return ()) (unGetChan (channel dr))
        whileM (isEmptyChan $ channel dr) $ 
            readChan (channel dr) >>= closeQuestion
    where
    closeQuestion (Question _ answer) = putMVar answer Nothing

whileM mb f = do b <- mb; when b (f >> whileM mb f)

