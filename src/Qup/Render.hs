{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Qup.Render (wire, Result(..), rdisplay, rshow) where

import Reflex
import Reflex.Dom
import Control.Applicative
import Control.Monad hiding (guard)
import Control.Lens
import Safe
import Data.Tree
import Data.Maybe
import qualified Data.Map.Lazy as Map

import Qup.Parse
import Qup.Widgets hiding (page) -- TODO fix this hiding
import Qup.Data

import Debug.Trace

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
    r <- el "div" $ do 
        r <- multiAnswers $ t
        debug m
        return r
    return $ insertIds (Just m) (RBool r) Map.empty
render rs (Element FreeAnswer (Just m) []) = divClass "freeAnswer" $ do
    r <- el "div" $ textAnswer ""
    debug m
    return $ insertIds (Just m) (RString r) Map.empty
render rs (Element Question (Just m) [CPlain t, Element Answers _ as]) = do
    el "p" $ do
        text $ t
        debug m
    let (singleAnswers,otherAnswers) = splitAnswers as [] []
    ds1 <- if length singleAnswers > 0
        then do
            d <- radioButtons $ transSas singleAnswers
            return $ insertIds (Just m) (RString d) Map.empty
        else return Map.empty
    ds2 <- Map.unions <$> mapM (render rs) otherAnswers
    return $ Map.unions [ds1,ds2]
    where splitAnswers [] sas oas = (reverse sas,reverse oas) 
          splitAnswers (e@(Element SingleAnswer _ _):as) sas oas = splitAnswers as (e:sas) oas
          splitAnswers (a:as) sas oas = splitAnswers as sas $ a:oas
          transSas [] = []
          transSas (e@(Element SingleAnswer (Just m) [CPlain t]):sas) = (unwrap $ m^.meta_index,t):transSas sas
          unwrap Nothing = ""
          unwrap (Just x) = x
    {-
    let isPair (RPair _) = True
        isPair _ = False
        pshow (RPair _) = "pair"
        pshow (RBool _) = "bool"
        pshow (RUnit _) = "unit"
        pshow (RString _) = "string"
    rec sae <- return $ map (\(RPair (s,b)) -> attach (constant s) (updated b)) $ filter isPair ps
        setV  <- return $ tag (constant False) $ leftmost $ sae  -- FIXME fix cycle
        (ps,ds) <- (\(x,y) -> (x, Map.unions y)).unzip <$> mapM (renderAnswer never rs) as
    sv <- holdDyn True setV
    el "p" $ do
        text "Reset: "
        display sv
    r <- holdDyn "" $ fst $ splitE $ leftmost $ sae 
    --mapM (el "div" . rdisplay) singleAnswers
    if length sae > 0
        then return $ insertIds (Just m) (RString r) ds
        else return ds
    -}
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
