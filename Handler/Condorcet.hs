module Handler.Condorcet 
    ( getCreateR
    , postCreateR
    , getVoteR
    )
where

import Import
-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)

-- import Database.Persist
-- import Text.Shakespeare.Text                       -- for the plain text quasiquote
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import System.Random  -- TODO: consider using System.Random.MWC form mwc-random

getCreateR :: Handler Html
getCreateR = do
    defaultLayout $ do
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/knockout/3.3.0/knockout-min.js"
        addScriptRemote "https://code.jquery.com/jquery-2.1.4.min.js"
        $(widgetFile "create")

postCreateR :: Handler RepPlain
postCreateR = do
    maybeTitle <- lookupPostParam "title"
    optionsText <- lookupPostParams "options[]"
    case (maybeTitle, ((length optionsText) >= 2)) of   -- validate that title is not null, and at least 2 options
        (Just title, True) -> runDB $ do 
            uid <- unusedUid
            poll <- insert $ Poll uid title
            let options = map (\name -> PollOption poll name) optionsText
            optionIds <- mapM insert options
            return $ RepPlain $ toContent $ (unPollKey poll)       -- this creates a plaintext response
        _ -> notFound       -- TODO another type of error would be more appropriate
    where
        unusedUid = do
            uid <- liftIO randomUid     -- we need to liftIO to go from IO monad to Handler monad
            pollList <- selectList [PollUid ==. uid] [LimitTo 1]        -- setting up Unique in models could allow us to write this with getBy instead
            case pollList of
                []  -> return uid
                _   -> unusedUid   -- try again
        -- makeUrl poll = [st|@{VoteR poll}|]

getVoteR :: PollId -> Handler Html
getVoteR pollId = do
    (poll, pollOptions) <- runDB $ do
            poll <- get404 pollId
            pollOptions <- selectList [PollOptionPollId ==. pollId] []
            return (poll, pollOptions)
    defaultLayout $ do
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/knockout/3.3.0/knockout-min.js"
        addScriptRemote "https://code.jquery.com/jquery-2.1.4.min.js"
        $(widgetFile "vote")


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
        n = fromIntegral $ input `mod` inputUpperEnd
        inputNumEnd = 10
        inputLowerEnd = inputNumEnd + 26
        inputUpperEnd = inputLowerEnd + 26
        word8NumStart = c2w8 '0'
        word8LowerStart = c2w8 'a'
        word8UpperStart = c2w8 'A'

{- | Generates a random poll uid -}
randomUid :: IO Text
randomUid = do
    randomList <- replicateM 6 $ getStdRandom (randomR (0,randomRange))
    let w8List = map intToWord8 randomList
    return $ pack $ C.unpack $ B.pack w8List
    where
        randomRange = 10+26+26 :: Int
