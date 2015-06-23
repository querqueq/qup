{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Qup.Wire (wire, Result(..), rshow, rdisplay) where

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

import Debug.Trace -- FIXME remove

data Result t = RBool (Dynamic t Bool)
              | RString (Dynamic t String)
              | RPair (String, Dynamic t Bool)
              | RUnit ()

wire :: MonadWidget t m => Element -> m (Map.Map Id (Result t))
wire root = do
        rec (_,results) <- trav results root
        return results
    where
        trav :: MonadWidget t m => Map.Map Id (Result t) -> Element -> m (Result t, Map.Map Id (Result t))
        trav rs e@(Element t m cs) = do
            (d,ds,subws) <- render rs e $ (\(x,y) -> (x, Map.unions y))
                            .unzip 
                            <$> mapM (trav rs) cs
            return (d, insertIds m d ds)
        trav rs _ = return (RUnit (), Map.empty) -- FIXME Map.empty also needed for elements without any children

onlyContent [] = True
onlyContent ((Element _ _ _):cs) = False
onlyContent (_:cs) = onlyContent cs

insertIds :: Maybe Meta -> Result t -> Map.Map Id (Result t) -> Map.Map Id (Result t)
insertIds _ (RUnit _) m = m
insertIds meta (RPair (_,b)) m = insertIds meta (RBool b) m
insertIds meta d m = foldl (\m k -> Map.insert k d m) m $ ids meta

render rs (Element Document _ ((CPlain t):ps)) sub = el "content" $ do
    el "h1" $ text t
    (subws,ds) <- divClass "pages" $ sub
    return (RUnit (),ds,subws)
render rs (Element Page (Just m) ((CPlain t):_)) sub = divClass "page" $ do -- TODO set id for div to page's id
    el "h2" $ text t
    debug m
    (subws,ds) <- divClass "pageContent" $ sub
    return (RUnit (),ds,subws)
render rs (Element SingleAnswer (Just m) [CPlain t]) sub = divClass "singleAnswer" $ do
    -- FIXME implement this correct 
    {-
    let (Just idx) = m^.meta_index 
        pidx = init $ init idx
        (Just (RString pv)) = Map.lookup pidx rs
        setV = tag (constant False) $ updated pv 
    --}
    r <- el "div" $ do
        (RadioButton r) <- radioButton False $ def {-& radioButtonConfig_setValue .~ setV--}
        text $ " " ++ t
        debug m
        return r
    (subws,ds) <- sub
    return $ (RPair (t,r), ds, subws)
render rs (Element MultiAnswer (Just m) [CPlain t]) sub = divClass "multiAnswer" $ do
    r <- el "div" $ do 
        r <- multiAnswers $ t
        debug m
        return r
    (subws,ds) <- sub
    return $ (RBool r,ds,subws)
render rs (Element FreeAnswer (Just m) []) sub = divClass "freeAnswer" $ do
    r <- el "div" $ textAnswer ""
    (subws,ds) <- sub
    debug m
    return $ (RString r, ds, subws)
render rs (Element (Guard gid) _ (c:_)) sub = do
    let (Just (RBool v)) = Map.lookup gid rs -- TODO error handling on Nothing TODO implement guard for other results
    (subws,ds) <- guardWidget v $ sub
    return (head subws,ds,subws)
    where       
        g Nothing = text "Guard: ID not found" >> sub
        g (Just (RBool v)) = guardWidget v $ sub
        g (Just _) = text "Guard: value not supported" >> sub
render rs (Element Question (Just m) ((CPlain t):_)) sub = do
    el "p" $ do
        text $ t
        debug m
    (subws, ds) <- sub
    return (last subws,ds,subws)
render rs (Element Paragraph _ [CPlain t]) sub = el "p" $ do
    mapM_ textBr $ lines t
    (subws, ds) <- sub
    return (RUnit (),ds,subws)
render rs (Element Answers _ _) sub = do
    (subws,ds) <- sub
    let isPair (RPair _) = True
        isPair _ = False
        singleAnswers = filter isPair subws
        --trueAnswers = map (\(RPair (s,b)) -> gate (current b) $ attach (constant s) (updated b)) singleAnswers 
        sae = map (\(RPair (s,b)) -> attach (constant s) (updated b)) singleAnswers
    r <- holdDyn "" $ fst $ splitE $ leftmost $ sae -- TODO improve: use event with True insteadof leftmost
    --mapM (el "div" . rdisplay) singleAnswers
    if length sae > 0 
        then return (RString r,ds,subws)
        else return (RUnit (),ds,subws)
render rs (Element t _ _) sub = do
    text $ "Cannot display " ++ show t
    (subws,ds) <- sub
    return (RUnit (),ds,subws)

textBr t = do 
    text t
    el "br" $ return ()

debug m = let 
            getter = [meta_index{-.to (liftA show)-},meta_id] -- Add getters here
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
