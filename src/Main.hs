{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

import Text.Parsec (parse)
import Reflex 
import Reflex.Dom
import Reflex.Dom.Xhr
import qualified Data.Map.Lazy as Map
import Data.FileEmbed
import Control.Applicative
import Control.Lens

import Qup.Parse
import Qup.Wire
import Qup.Widgets
import Qup.Data

makeLenses ''XhrRequestConfig
makeLenses ''XhrResponse

main :: IO ()
main = do
    mainWidgetWithCss $(embedFile "style.css") $ do
        box <- divClass "input"  $ textArea $ TextAreaConfig (unlines example) never $ constDyn Map.empty
        qu <- mapDyn (render.attachMeta.parse quParser "An error occured") $ value box
        divClass "output" $ do 
            renderChanged <- dyn qu
            changes <- count renderChanged
            divClass "stats" $ do
                text "Changes: "
                display changes

render (Left e) = el "p" $ do 
    el "b" $ text $ "Markup is erroneous:"
    el "p" $ text e
render (Right doc@(Element Document _ _)) = do
    results <- wire doc
    --divClass "values" $ mapM (\(k,v) -> el "div" $ text (k++": ") >> rdisplay v) $ Map.toList results
    responses <- mapM (\(k,v) -> xhrAnswer k v) $ Map.toList results
    --divClass "response" $ mapM xhrDisplay responses
    return () -- TODO handle results; send to web service
render (Right _) = el "p" $ text $ "Data is malformed"

xhrDisplay resp = do
    t <- mapDyn (\r -> r^.xhrResponse_body) resp
    el "div" $ display t
    return ()

xhrAnswer k v = do
    t <- rshow v
    req <- mapDyn (answerReq k) t
    resp <- performRequestAsync $ updated req
    holdDyn (XhrResponse Nothing) resp
    where answerReq id answer = xhrRequest "POST" ("http://localhost/~j/post/answer/" ++ id)  -- TODO move url from user
                              $ def & xhrRequestConfig_sendData .~ (Just answer)


