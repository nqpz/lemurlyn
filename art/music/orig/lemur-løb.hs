#!/usr/bin/env runghc
module Main (main) where

import Guld
import GuldInstruments
import Control.Monad
import Control.Applicative


p p d n = withPitch (+ p) $ withDuration (+ d) n
b p0 d t n = p p0 d n >> setTime (+ t)

lemur :: TrackM ()
lemur = do
  localContext bass
  localContext (withVolume (subtract 10) (replicateM_ 2 (strings >> fiddl)))

bass :: TrackM ()
bass = replicateM_ 420 $ do
  p (-20) 2 synthBass1
  setTime (+ 1)

strings :: TrackM ()
strings = do
  forM_ [3, 1, 9] $ \w -> withPitch (subtract w) $ do
    forM_ [1..4] $ \q -> do
      b (q ^ 2) 1 2 stringEnsemble1

fiddl :: TrackM ()
fiddl = do
  forM_ [0, 2, 0, 2]
  $ \q -> withPitch (+ q) $ withPitch (+ 10) $ withInstrument fiddleR $ do
  b 0 1 1 note
  b 1 1 1 note
  b 2 1 1 note
  b 1 1 1 note
  b 0 1 1 note
  b (-1) 5 10 note

  b 10 1 1 note
  b 11 1 16 note

main :: IO ()
main = outputControl $ convertNotes 240 $ evalTrackM lemur
