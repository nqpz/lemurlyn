#!/usr/bin/env runghc
module Main (main) where

import Guld
import GuldInstruments
import Control.Monad
import Control.Applicative


lemur :: TrackM ()
lemur = do
  localContext water
  localContext $ setTime (+ 10) >> backingTaiko
  localContext $ setTime (+ 15) >> backingSynth
  localContext $ withVolume (subtract 10) $ setTime (+ 20) >> melodicSlow
  localContext $ withVolume (subtract 5) $ setTime (+ 25) >> melodicFast

backingTaiko :: TrackM ()
backingTaiko = do
  let drumNotes =
        [ (0, 8, 5)
        , (2, 7, 5)
        , (4, 6, 5)
        , (6, 7, 5)
        ]
  withInstrument taikoDrumR $ withPitch (+ 5)
    $ replicateM_ 70
    $ forM_ drumNotes
    $ \(p, d, t) -> withPitch (+ p) $ withDuration (+ d)
    $ do
    note
    setTime (+ t)

backingSynth :: TrackM ()
backingSynth = do
  withInstrument synthDrumR $ withPitch (+ 20)
    $ replicateM_ 100 $ do
    note
    setTime (+ 13)

melodicSlow :: TrackM ()
melodicSlow = do
  let melNodes =
        [ (0, 10)
        , (5, 10)
        , (10, 10)
        , (5, 3)
        , (-2, 2)
        , (-4, 2)
        , (-6, 2)
        , (-9, 2)
        , (10, 8)
        , (9, 7)
        , (8, 8)
        , (7, 17)
        ]
  withInstrument lead5CharangR
    $ replicateM_ 2
    $ forM_ [0, 1, 2, 1, 0, -1, -2, -1]
    $ \p0 -> withPitch (+ p0)
    $ forM_ melNodes
    $ \(p, d) -> withPitch (+ p) $ withDuration (+ d)
    $ do
    note
    setTime (+ d)

water :: TrackM ()
water = do
  let tides =
        [ (0, 2)
        , (5, 3)
        , (3, 1)
        ]
  withInstrument seashoreR
    $ forM_ (take 650 $ cycle tides)
    $ \(p, d) -> withPitch (+ p) $ withDuration (+ d)
    $ do
    note
    setTime (+ d)

melodicFast :: TrackM ()
melodicFast = do
  withInstrument violaR
    $ replicateM_ 33
    $ forM_ (zip ([1..10] ++ [9,8..2]) (repeat 2))
    $ \(p, d) -> withPitch (subtract p) $ withDuration (+ d)
    $ do
    note
    setTime (+ d)

main :: IO ()
main = outputControl $ convertNotes 900 $ evalTrackM lemur
