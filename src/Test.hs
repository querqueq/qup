{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import Reflex
import Reflex.Dom
import qualified Data.Map.Lazy as Map
import Control.Monad hiding (guard)
import Data.Tree
import Debug.Trace
import Control.Lens

import Qup.Widgets

data Element = Element 
               {_element_id     :: String
               ,_element_gid    :: Maybe String
               }

makeLenses ''Element

recTest = do
    x <- el "div" $ do
        multiAnswers "Hi"
    return ()

recMain = mainWidget $ do recTest; return()

xtree = Node "Root" [Node "Child One" [],Node "Child 2" [Node "S" []]]
xforest = [xtree, xtree]

etree = Node (Element "Root" Nothing) 
    [Node (Element "C1" (Just "C2")) []
    ,Node (Element "C2" (Just "Root")) 
        [Node (Element "C3" Nothing)
            [Node (Element "C4" (Just "C1")) []
            ]
        ]
    ]

-- challenge 1 
-- give every widget node the dynamic bool of the node with the shortest string
impure :: MonadWidget t m => Tree Element -> m (Map.Map String (Dynamic t Bool))
impure tree = do
        rec results <- go results tree
        return results
    where
        go rs (Node e@(Element id _) []) = do
            b <- rr rs e
            return $ Map.insert id b Map.empty
        go rs (Node e@(Element id _) cs) = do
            b <- rr rs e
            bs <- mapM (go rs) cs
            let bmap = Map.unions bs
            return $ Map.insert id b bmap

rr :: MonadWidget t m => Map.Map String (Dynamic t Bool) -> Element -> m (Dynamic t Bool)
rr rs (Element id Nothing) = do
    el "div" $ multiAnswers id
rr rs (Element id (Just gid)) = do
    let (Just v) = Map.lookup gid rs
    guard v $ rr rs (Element id Nothing)

guard v content = do
    let getAttrs True = "style" =: "display:inherit"
        getAttrs False = "style" =: "display:none"
    attrs <- mapDyn getAttrs v
    elDynAttr "span" attrs content

miniDyn bool t = do
    mapDyn (\v -> when v (t >> return())) bool >>= dyn
    
main = mainWidget $ do
    results <- trace "results " $ impure etree
    --Map.foldlWithKey' (\ws key bool -> trace key $ ws >> mini bool key) (mini (constDyn False) "" {-el "h2" $ text "Heading"-}) results
    return ()

mini :: MonadWidget t m => Dynamic t Bool -> String -> m (Event t ())
mini bool t = do
    mapDyn (\v -> when v (text t)) bool >>= dyn
    

wire root = do
    (_,cs) <- render root
    rec (rs,cs) <- (liftM split) $ (sequence $ map render cs)
    return rs

split [] = ([], [])
split ((a,bs):ps) = (a:as, bs ++ bss) where (as, bss) = split ps

render (Node s cs) = do
    r <- el "div" $ multiAnswers s
    return (r,cs)


