{-# OPTIONS -XBangPatterns -XDeriveDataTypeable #-}
module Control.Concurrent.XChan (
    XChan,
    newXChan,
    writeXChan,
    unGetXChan,
    readXChan,
    tryReadXChan,
    isEmptyXChan,
    inspectXChan
    )
    where

import Control.Concurrent.MVar
import Data.Typeable
import qualified Data.Sequence as Seq
import qualified Control.Exception as CE
import System.IO.Unsafe


   
data XChan x = XChan 
                -- signal; notifies blocking readers that data may be 
                -- available
                (MVar ()) 
                
                -- contents; contains the messages of the channel.
                -- This MVar will never be blocked on (i.e. you may not run 
                -- takeMVar/readMVar on it)
                (MVar (Seq.Seq x)) 
                
                -- unique ID for Eq instance
                Int 

  deriving (Typeable)                

                       
instance Eq (XChan a) where
    (XChan _ _ a) == (XChan _ _ b) = a == b

lastXChanID = unsafePerformIO $ newMVar 1                       
                       
newXChan = do
    a <- newEmptyMVar 
    b <- newMVar Seq.empty
    c <- modifyMVar lastXChanID $ \l -> return (l+1,l+1)
    return $ XChan a b c

writeXChan :: XChan a -> a -> IO ()                         
writeXChan (XChan a b _) val = CE.mask_ $ do
    modifyMVar_ b $ \old -> do
        tryPutMVar a ()
        return $! old Seq.|> val
    
unGetXChan :: XChan a -> a -> IO ()
unGetXChan (XChan a b _) val = CE.mask_ $ do
    modifyMVar_ b $ \old -> do
        tryPutMVar a ()
        return $! val Seq.<| old

readXChan :: XChan a -> IO a
readXChan m@(XChan a b _) = do
    CE.onException (takeMVar a)
      (tryPutMVar a ())
      
    ret <- tryReadXChan m
    case ret of
        Just x -> return x
        Nothing -> 
            -- this may happen if a concurrent call to tryReadXChan is made 
            -- after we received the read signal
            readXChan m 

    

tryReadXChan :: XChan a -> IO (Maybe a)    
tryReadXChan m@(XChan a b _) = do
    modifyMVar b $ \old -> do
        case Seq.viewl old of
            x Seq.:< rest | Seq.null rest -> do
                return (rest, Just x)
                
            x Seq.:< rest -> do
                tryPutMVar a ()
                return (rest, Just x)
                
            Seq.EmptyL -> do
                return (old, Nothing)


isEmptyXChan :: XChan a -> IO Bool
isEmptyXChan m@(XChan a b _) = do
    withMVar b $ \old -> return $ Seq.length old == 0
    
-- | returns the sequence of elements yet to be read
inspectXChan :: XChan a -> IO (Seq.Seq a)   
inspectXChan (XChan a b _) = do
    withMVar b $ \old -> return old
    
        
