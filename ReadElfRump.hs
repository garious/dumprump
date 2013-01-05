-- readelfrump is to be run as on the tail-end of readelf
-- 
-- Usage:
--      readelf -s MyFile.o | readelfrump
--
-- readelfrump's role is to format the readelf output such that disassembly
-- dumped from GCC and LLVM binaries can be compared with minimal
-- noise.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.ByteString         as A
import qualified Data.Attoparsec.ByteString.Char8   as AC
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as BC
import Data.ByteString
  ( ByteString
  )
import Control.Monad
  ( liftM
  )
import Data.List
  ( sort
  )

main :: IO ()
main = B.getContents >>= B.putStr . tweak

tweak :: ByteString -> ByteString
tweak s = BC.unlines (hdr : sort (map tweakLine tbl))
  where
     hdr = "    Value  Size Type    Bind   Vis      Ndx Name"
     tbl = filter (BC.isInfixOf "$") (drop 3 ls)
     ls = BC.lines s

tweakLine :: ByteString -> ByteString
tweakLine = foldr (.) id $ map transform transforms

transforms :: [A.Parser ByteString]
transforms = [
      dropAddr
    , dropMoney
    ]

transform :: A.Parser ByteString -> ByteString -> ByteString
transform p s = either (const s) id (A.parseOnly p s)

-- Symbol table output looks like this:
--
--    Symbol table '.symtab' contains 252 entries:
--      Num:    Value  Size Type    Bind   Vis      Ndx Name
--        0: 00000160     0 NOTYPE  LOCAL  DEFAULT    1 $d.1

-- Make it look like this:
--
--              Value  Size Type    Bind   Vis      Ndx Name
--           00000160     0 NOTYPE  LOCAL  DEFAULT    1 $d

dropAddr :: A.Parser ByteString
dropAddr = spaces >> hexString >> A.string ":" >> A.takeByteString

dropMoney :: A.Parser ByteString
dropMoney = concatSequence [
       AC.takeWhile1 (/= '$')
     , A.string "$"
     , A.takeWhile1 (A.inClass "tad")
     ]

-- Run each parser, and concat the results
concatSequence :: Monad m => [m ByteString] -> m ByteString
concatSequence = liftM B.concat . sequence

spaces :: A.Parser ByteString
spaces = A.takeWhile1 AC.isHorizontalSpace

hexString :: A.Parser ByteString
hexString = A.takeWhile1 (A.inClass "0-9a-fA-F")

