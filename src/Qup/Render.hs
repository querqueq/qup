{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}

module Qup.Render (render) where

import Reflex
import Reflex.Dom
import Control.Applicative
import Control.Monad hiding (guard)
import Control.Lens
import Safe
import Data.Tree
import Data.Maybe
import qualified Data.Map as Map

import Qup.Parse
import Qup.Widgets
import Qup.Data

render :: (Show a, MonadWidget t m) => Either a Element -> m ()  
render (Left e) = el "p" $ text $ "Markup is erroneous: " ++ (show e)
render (Right (Element Document m ((Element Heading _ ((CPlain t):_)):cs))) = do
    el "h1" $ text t
    pages $ map (\(Element Page m ((Element Heading _ ((CPlain t):_)):pcs)) -> (t, do debug m; mapM_ renderContent pcs)) cs
    return ()
render (Right _) = el "p" $ text $ "Data is malformed"

renderContent (Element Question m ((Element Paragraph _ ((CPlain t):_)):cs)) = do
    el "p" $ do
        text t
        debug m
    divClass "answers" $ renderContent $ head cs 
renderContent (Element Answers _ as) = divClass "answers" $ mapM_ renderContent as
renderContent (Element SingleAnswer m ((CPlain t):_)) = divClass "single" $ do 
    text t
    debug m
renderContent (Element MultiAnswer m ((CPlain t):_)) = divClass "multi" $ void $ do
    multiAnswers t 
    debug m
renderContent (Element FreeAnswer m _) = divClass "free" $ do
    textAnswer "" 
    debug m
    return ()
renderContent (Element Paragraph _ ((CPlain t):_)) = el "p" $ do
    mapM_ textBr $ lines t
renderContent (Element (Guard onId) _ (c:_)) = divClass "guard" $ do
    text $ "Guarded by " ++ onId
    renderContent c
renderContent (Element t _ _) = divClass "error" $ text $ "Render for " ++ show t ++ " not implemented"

textBr t = el "some" $ do
    text t
    el "br" $ return ()

debug Nothing = divClass "debug" $ text "[]"
debug (Just m) = let 
            getter = [meta_index.to (liftA show),meta_id] -- Add getters here
            metas  = filter (/="") $ map (maybe "" id . (flip view) m) getter
            p = initSafe $ unwords $ zipWith (++) metas (repeat ",")
          in divClass "debug" $ text $ " [" ++ p ++ "]"
