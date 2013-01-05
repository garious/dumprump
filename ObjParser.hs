-- Parsing utilities for Objdump files

{-# LANGUAGE OverloadedStrings #-}

module ObjParser where

import qualified Data.Attoparsec.ByteString         as A
import qualified Data.Attoparsec.ByteString.Char8   as AC
import qualified Data.ByteString                    as B
import Data.ByteString
  ( ByteString
  )
import Control.Monad
  ( liftM
  , liftM2
  )
import Control.Applicative
  ( (<|>)
  )

condition :: A.Parser ByteString
condition = A.string "eq"
        <|> A.string "ne"
        <|> A.string "cs"
        <|> A.string "cc"
        <|> A.string "mi"
        <|> A.string "pl"
        <|> A.string "vs"
        <|> A.string "vc"
        <|> A.string "hi"
        <|> A.string "ls"
        <|> A.string "ge"
        <|> A.string "lt"
        <|> A.string "gt"
        <|> A.string "le"
        <|> A.string ""

lowRegister :: A.Parser Int
lowRegister = do
    _  <- A.string "r"
    ch <- AC.satisfy (AC.inClass "0-7")
    return $ read [ch]

-- Parse a 32-bit thumb address or a 32-bit ARM address or a 16-bit Thumb address
thumbOrArmAddr :: A.Parser ByteString
thumbOrArmAddr = appendM (hexStringN 4) rest
   where
      rest = appendM spaces (hexStringN 4)  -- 2nd half of 32-bit Thumb2 address
         <|> hexStringN 4                   -- 2nd half of 32-but ARM address
         <|> return ""                      -- Assume 1st half is Thumb1 address

-- Run each parser, and concat the results
concatSequence :: Monad m => [m ByteString] -> m ByteString
concatSequence = liftM B.concat . sequence

appendM :: A.Parser ByteString -> A.Parser ByteString -> A.Parser ByteString
appendM = liftM2 B.append
 
spaces :: A.Parser ByteString
spaces = A.takeWhile1 AC.isHorizontalSpace

hexString :: A.Parser ByteString
hexString = A.takeWhile1 (A.inClass "0-9a-fA-F")

hexStringN :: Int -> A.Parser ByteString
hexStringN 0 = return ""
hexStringN n = do
    ch <- A.satisfy (A.inClass "0-9a-fA-F")
    rest <- hexStringN (n - 1)
    return $ B.cons ch rest

