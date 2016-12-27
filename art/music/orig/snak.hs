#!/usr/bin/env runghc
module Main (main) where

import Guld
import GuldInstruments
import Control.Monad
import Control.Applicative


p p d n = withPitch (+ p) $ withDuration (+ d) n
b p0 d t n = p p0 d n >> setTime (+ t)

snak :: TrackM ()
snak = do
  localContext ringgg
  localContext back

ringgg :: TrackM ()
ringgg =
  withDuration (+ 5) telephoneRing

back :: TrackM ()
back =
  withInstrument slapBass1R
  $ replicateM_ 5
  $ withDuration (+ 3)
  $ forM_ [3, 1, 3, 5]
  $ \p -> withPitch (+ p)
  $ do
  note
  setTime (+ 3)

main :: IO ()
main = outputControl $ convertNotes 120 $ evalTrackM snak
