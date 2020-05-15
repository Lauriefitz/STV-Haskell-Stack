module Main where

import Lib
import STV.A1_V2

main :: IO ()
main = do
    print ("Alternative voting winner: ")
    print $ no1
    print ("STV Voting outcome (3 seats): ")
    print ("First seat: ")
    print $ no1
    print ("Second seat: ")
    print $ no2
    print ("Third seat: ")
    print $ no3
    print ("Fourth seat: ")
    print $ no4
    print ("Eliminated: ")
    print $ eliminated