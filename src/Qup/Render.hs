{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Qup.Render (wire, Result(..), rdisplay, rshow) where

import Reflex
import Reflex.Dom
import Control.Applicative
import Control.Monad hiding (guard)
import Control.Lens
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as Map
import Safe

import Qup.Parse
import Qup.Widgets hiding (page) -- TODO fix this hiding
import Qup.Data

import Debug.Trace

--f :: MonadWidget t m => m ()
--f = Map.lookup "a" ("a" =: constDyn True) >>= liftM (\x -> guardWidget x $ text "a")

data Result t = RBool (Dynamic t Bool)
              | RString (Dynamic t String)
              | RPair (String, Dynamic t Bool)
              | RUnit ()

wire :: MonadWidget t m => Element -> m (Map.Map Id (Result t))
wire root = do
        rec results <- render results root
        return results

onlyContent [] = True
onlyContent ((Element _ _ _):cs) = False
onlyContent (_:cs) = onlyContent cs

insertIds :: Maybe Meta -> Result t -> Map.Map Id (Result t) -> Map.Map Id (Result t)
insertIds _ (RUnit _) m = m
insertIds meta (RPair (_,b)) m = insertIds meta (RBool b) m
insertIds meta d m = foldl (\m k -> Map.insert k d m) m $ ids meta

render :: MonadWidget t m => Map.Map Id (Result t) -> Element -> m (Map.Map Id (Result t))
render rs (Element (Guard gid) _ [c]) = do
    let (Just (RBool v)) = Map.lookup gid rs -- TODO error handling on Nothing TODO implement guard for other results
    ds <- guardWidget v $ render rs c
    return ds
render rs (Element Document _ ((CPlain t):ps)) = el "content" $ do
    el "h1" $ text t
    dss <- divClass "pages" $ mapM (render rs) ps
    return $ Map.unions dss
render rs (Element Page (Just m) ((CPlain t):cs)) = divClass "page" $ do -- TODO set id for div to page's id
    el "h2" $ text t
    debug m
    dss <- divClass "pageContent" $ mapM (render rs) cs
    return $ Map.unions dss
render rs (Element MultiAnswer (Just m) [CPlain t]) = divClass "multiAnswer" $ do
    r <- multiAnswers $ t
    debug m
    return $ insertIds (Just m) (RBool r) Map.empty
render rs (Element FreeAnswer (Just m) []) = divClass "freeAnswer" $ do
    r <- textAnswer ""
    debug m
    return $ insertIds (Just m) (RString r) Map.empty
render rs (Element Question (Just m) [CPlain t, Element Answers _ as]) = divClass "question" $ do
    divClass "question-text" $ do
        text $ t
        debug m
    let groupedAnswers = groupBy sameType as
    ds <- Map.unions <$> mapM renderAnswers groupedAnswers
    return ds
    where renderAnswers as@((Element SingleAnswer _ _):_)
            | isRange as = divClass "range" $ do
                d <- range (getTitle $ head as) (getTitle $ last as) $ map (\x -> unwrap $ getMeta x^.meta_index) as
                return $ insertIds (Just m) (RString d) Map.empty
            | otherwise  = divClass "radios" $ do
                d <- radioButtons $ transSas as
                return $ insertIds (Just m) (RString d) Map.empty
          renderAnswers as = Map.unions <$> mapM (render rs) as
          transSas [] = []
          transSas (e@(Element SingleAnswer (Just m) [CPlain t]):sas) = (unwrap $ m^.meta_index,t):transSas sas
          unwrap Nothing = ""
          unwrap (Just x) = x
render rs (Element Paragraph _ [CPlain t]) = el "p" $ do
    mapM_ textBr $ lines t
    return Map.empty
render rs (Element t _ _) = divClass "error" $ do
    text $ "Cannot display " ++ show t
    return Map.empty

textBr t = do 
    text t
    el "br" $ return ()

debug m = let 
            getter = [meta_index,meta_id] -- Add getters here
            metas  = filter (/="") $ map (maybe "" id . (flip view) m) getter
            p = initSafe $ unwords $ zipWith (++) metas (repeat ",")
          in divClass "debug" $ text $ " [" ++ p ++ "]"

rshow :: (Reflex t, MonadHold t m) => Result t -> m (Dynamic t String)
rshow (RBool v) = mapDyn show v
rshow (RString v) = mapDyn id v
rshow (RPair (s,b)) = rshow (RBool b) >>= mapDyn (\b -> s ++ ": " ++ b)
rshow (RUnit ()) = return $ constDyn "-"

rdisplay :: MonadWidget t m => Result t -> m ()
rdisplay r = do
    rString <- rshow r
    display rString
