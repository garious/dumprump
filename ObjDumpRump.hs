-- objdumprump is to be run as on the tail-end of objdump.
-- 
-- Usage:
--      objdump -d MyFile.o | objdumprump
--
-- objdumprump's role is to format the objdump output such that disassembly
-- dumped from GCC and LLVM binaries can be compared with minimal
-- noise.

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Attoparsec.ByteString         as A
import qualified Data.Attoparsec.ByteString.Char8   as AC
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lazy               as BL
import Data.ByteString
  ( ByteString
  )
import Control.Applicative
  ( (*>)  -- Run both, but return the return value of the right
  , (<|>)
  )
import Numeric
  ( showHex
  )
import Data.Bits
  ( (.|.)
  , shift
  )
import System.IO
  ( stdin
  , stdout
  )
import Data.Enumerator
  ( ($$)  -- Pipe enumerator to iteratee
  , ($=)  -- Pipe enumerator to enumeratee
  , Iteratee
  , Enumerator
  , Enumeratee
  )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import ObjParser

main :: IO ()
main = E.run_ $ enumStdin $= dropHeader $= tweak $$ iterStdout

dropHeader :: (Monad m) => Enumeratee ByteString ByteString m b
dropHeader = E.sequence $ do
    _ <- lineI
    _ <- lineI
    s <- EB.consume
    return (BL.toStrict s)

tweak :: (Monad m) => Enumeratee ByteString ByteString m b
tweak = E.sequence $ do
    x <- lineI
    return $ BC.append (tweakLine x) "\n"

lineI :: Monad m => Iteratee ByteString m ByteString
lineI = do
    s <- EB.takeWhile (/= 10) -- Take until \n
    EB.drop 1                 -- Drop the newline
    return (BL.toStrict s)    -- Return a strict bytestring

enumStdin :: Enumerator ByteString IO b
enumStdin = EB.enumHandle 1024 stdin

iterStdout :: Iteratee ByteString IO ()
iterStdout = EB.iterHandle stdout

tweakLine :: ByteString -> ByteString
tweakLine = foldr (.) id $ map transform transforms

transforms :: [A.Parser ByteString]
transforms = [
      addsTransform
    , addRegTransform
    , ldrTransform
    , nopR8Transform
    , nopTransform
    , labelTransform
    , branchTransform
    ]

transform :: A.Parser ByteString -> ByteString -> ByteString
transform p s = either (const s) id (A.parseOnly p s)

-- Replace LLVM's silly "mov r8, r8" nop to a proper nop
--
--    - 46c0            nop   ; mov r8, r8
--    + bf00            nop
nopR8Transform :: A.Parser ByteString
nopR8Transform = concatSequence [
      address
    , spaces
    , A.string "46c0" *> return "bf00"
    , spaces
    , A.string "nop"
    ]

-- Replace GCC's intelligent usage of nop.w with LLVM's silly 2 nop instructions 
-- -     d1c:	f3af 8000 	nop.w
-- +     d1c:	bf00      	nop
-- +     d1e:	bf00      	nop
nopTransform :: A.Parser ByteString
nopTransform = do
    spc0 <- spaces
    addr <- hexString
    _    <- A.string ":"
    spc1 <- spaces
    _    <- A.string "f3af 8000"

    return $ B.concat [
        spc0
      , addr
      , ":"
      , spc1
      , "bf00      \tnop\n"
      , spc0
      , incThumbAddr addr
      , ":"
      , spc1
      , "bf00      \tnop"
      ]

-- Drop comment from 'ldr' because GCC and LLVM each point
-- to a label with different syntax
-- - ldr   r2, [pc, #592]   ; (blah <blah>)
-- + ldr   r2, [pc, #592]
ldrTransform :: A.Parser ByteString
ldrTransform = concatSequence [
    address
  , spaces
  , A.string "ldr"
  , spaces
  , AC.takeTill (== ']')
  , A.string "]"
  , AC.takeTill (== '<') *> return "" -- Make sure the comment has '<label>' syntax
  ]

-- TODO: come up with a mechanism for removing newlines
-- TODO: Remove LLVM's redundant labels:
--000032a6 <.LBB5_867>:

-- Replace LLVM's use of T1 encoding with T2 encoding
--
--    addeq r0, r0, r1    =>    addeq r0, r1
addRegTransform :: A.Parser ByteString
addRegTransform = do
    addr <- concatSequence [address, spaces]
    _old <- hexStringN 4
    inst <- concatSequence [spaces, A.string "add", condition, spaces]
    reg  <- lowRegister
    _    <- A.string "," >> spaces

    let regStr = BC.pack $ "r" ++ show reg
    _    <- A.string regStr >> A.string "," >> spaces
    rm   <- lowRegister

    return $ B.concat [
        addr
      , addRegEncoding reg rm
      , inst
      , regStr
      , ", "
      , BC.pack $ "r" ++ show rm
      ]

addRegEncoding :: Int -> Int -> ByteString
addRegEncoding rdn rm = BC.pack $ showHex (0x4400 .|. shift rm 3 .|. rdn) ""

-- Replace LLVM's use of T1 encoding with T2 encoding
--
--    adds r0, r0, #1    =>    adds r0, #1
--    subs r0, r0, #1    =>    subs r0, #1
addsTransform :: A.Parser ByteString
addsTransform = do
    addr <- concatSequence [address, spaces]
    _old <- hexStringN 4
    spc0 <- spaces
    inst <- A.string "adds" <|> A.string "subs"
    spc1 <- spaces
    reg  <- lowRegister
    _    <- A.string "," >> spaces

    let regStr = BC.pack $ "r" ++ show reg
    _    <- A.string regStr >> A.string "," >> spaces
    imm  <- A.string "#" >> AC.decimal

    return $ B.concat [
        addr
      , addsEncoding inst reg imm
      , spc0
      , inst
      , spc1
      , regStr
      , ", "
      , "#"
      , BC.pack $ show imm
      ]

addsEncoding :: ByteString -> Int -> Int -> ByteString
addsEncoding inst reg imm = BC.pack $ showHex (shift (op .|. reg) 8 .|. imm) ""
  where
    op = if inst == "adds" then 0x30 else 0x38

-- Add 2 to the given hexadecimal address
incThumbAddr :: ByteString -> ByteString
incThumbAddr s = BC.pack (showHex (read ("0x" ++ BC.unpack s) + 2 :: Int) "")

branchTransform :: A.Parser ByteString
branchTransform = concatSequence [
    address
  , spaces
  , thumbAddrPlaceholder <|> armAddrPlaceholder
  , spaces
  , A.string "bl"
  , condition
  , spaces
  , A.takeByteString
  ]

address :: A.Parser ByteString
address = concatSequence [spaces, hexString, A.string ":"]

-- Parse a 32-bit thumb address and return the placeholder constant
thumbAddrPlaceholder :: A.Parser ByteString
thumbAddrPlaceholder = concatSequence [
    hexStringN 4 *> return "f7ff"
  , spaces
  , hexStringN 4 *> return "fffe"
  ]

-- Parse an arm address and return the placeholder constant
armAddrPlaceholder :: A.Parser ByteString
armAddrPlaceholder = hexStringN 8 *> return "ebfffffe"

-- Remove the label comment on branch instructions
labelTransform :: A.Parser ByteString
labelTransform = concatSequence [
    spaces
  , hexString
  , A.string ":"
  , spaces
  , thumbOrArmAddr
  , spaces
  , A.string "b"
  , A.takeWhile (not . AC.isHorizontalSpace)
  , spaces
  , hexString
  , A.takeByteString *> return ""
  ]
