module Handler.Condorcet 
    ( getCreateR
    , postCreateR
    )
where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

import Database.Persist
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import System.Random  -- TODO: consider using System.Random.MWC form mwc-random

getCreateR :: Handler Html
getCreateR = do
    defaultLayout $ do
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/knockout/3.3.0/knockout-min.js"
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "create")

postCreateR :: Handler Html
postCreateR = do
    defaultLayout $ do
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/knockout/3.3.0/knockout-min.js"
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "create")


{- Adapted form http://www.alfredodinapoli.com/posts/2012-10-18-fast-random-strings-generation-in-haskell.html
-}

{- | Converts a Char to a Word8. Took from MissingH -}
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

{- | Gives a Word8 that corresponds to 0..9 or a..z or A..Z -}
intToWord8 :: Integral a => a -> Word8
intToWord8 input
             | n < inputNumEnd      = word8NumStart + n  
             | n < inputLowerEnd    = word8LowerStart + (n - inputNumEnd)
             | otherwise            = word8UpperStart + (n - inputLowerEnd)
    where
        n = fromIntegral $ input `mod` (10+26+26)
        inputNumEnd = 10
        inputLowerEnd = inputNumEnd + 26
        inputUpperEnd = inputLowerEnd + 26
        word8NumStart = c2w8 '0'
        word8LowerStart = c2w8 'a'
        word8UpperStart = c2w8 'A'

{- | Generates a random poll uid -}
randomUid :: IO String
randomUid = do
    randomList <- replicateM 6 $ getStdRandom (randomR (0,randomRange))
    let w8List = map intToWord8 randomList
    return $ C.unpack $ B.pack w8List
    where
        randomRange = 10+26+26 :: Int

-- unusedUid :: IO String
-- unusedUid = do
--     uid <- randomUid
--     maybePoll <- getBy PollUid uid
--     case maybePoll of
--         Just _   -> unusedUid   -- try again
--         Nothing  -> return uid

