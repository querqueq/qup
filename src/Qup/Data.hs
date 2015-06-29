{-# LANGUAGE TemplateHaskell #-} 
   
module Qup.Data where

import Control.Lens
import Control.Applicative
import Safe
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Reflex
import Reflex.Dom
import Data.List

type Id = String

data Meta = Meta
            { _meta_index     :: Maybe Id
            , _meta_id        :: Maybe Id
            } deriving Show

makeLenses ''Meta

data ElementType = Document
                 | Page
                 | Question 
                 | Answers 
                 | Paragraph 
                 | Heading 
                 | SingleAnswer
                 | MultiAnswer
                 | FreeAnswer
                 | Guard Id
                 | Range
                 deriving (Show,Eq)

data Element = Element 
                 { _element_type        :: ElementType
                 , _element_meta        :: Maybe Meta
                 , _element_content     :: [Element]
                 } 
               | CPlain String
               | CNum Integer
               deriving Show

makeLenses ''Element

-- Meta attachments

-- TODO get rid of either
attachMeta (Left a) = Left (show a)
attachMeta (Right doc) = checkIds $ setIndicies "" 1 doc

sameType (Element t1 _ _) (Element t2 _ _) = t1 == t2
isRange as = length as > 2 
             && (\x -> isSa x && hasT x) (head as) 
             && and (map (\x -> isSa x && not (hasT x)) (init $ tail as))
             && (\x -> isSa x && hasT x) (last as)
            where isSa (Element SingleAnswer _ _) = True
                  isSa _ = False
                  hasT (Element _ _ [CPlain t]) = t /= ""
                  hasT _ = False

-- FIXME implement correct, skip nothings, take prev from parent node with meta
setIndicies :: String -> Integer -> Element -> Element 
setIndicies pre n (Element t m contents) = case m of
                                            Nothing  -> Element t Nothing $ forCs pre 1 contents
                                            (Just m) -> Element t (Just (set meta_index (Just (pre ++ show n)) m)) $ forCs exPre 1 contents
                                       where forCs pre x [] = []
                                             forCs pre x (c@(Element _ (Just _) _):cs) = setIndicies pre x c : forCs pre (1+x) cs
                                             forCs pre x (c@(Element _ Nothing _):cs) = setIndicies pre x c : forCs pre x cs
                                             forCs pre x (c:cs) = c : forCs pre x cs
                                             exPre
                                                | length pre > 0 = pre ++ show n ++ "."
                                                | otherwise      = show n ++ "."
setIndicies _ _ x = x -- Catch primitive Elements

checkIds doc = if length dups == 0 then Right doc else Left $ "Duplicate IDs:" ++ unwordsBy "," dups 
               where ids = catMaybes $ map (view meta_id) $ allMetas doc
                     dups = doubles ids

-- Constructor functions

meta = Just $ Meta Nothing Nothing

document heading ps = Element Document Nothing $ CPlain heading : ps

page heading content = Element Page meta $ CPlain heading : content

paragraph text = Element Paragraph Nothing [CPlain text]

question text as = Element Question meta
                     [ CPlain text
                     , Element Answers Nothing as
                     ]

guard onId c = Element (Guard onId) Nothing [c]

answer :: ElementType -> String -> Element
answer SingleAnswer t = singleAnswer t
answer MultiAnswer t = multiAnswer t
answer FreeAnswer _ = freeAnswer
singleAnswer text = Element SingleAnswer meta [CPlain text]
multiAnswer text = Element MultiAnswer meta [CPlain text]
freeAnswer = Element FreeAnswer meta []

-- Helper

getTitle (Element _ _ [CPlain t]) = t
getMeta (Element _ (Just m) _) = m

ids :: Maybe Meta -> [Id]
ids Nothing  = []
ids (Just m) = catMaybes [m^.meta_index, m^.meta_id]

allMetas :: Element -> [Meta]
allMetas (Element _ m cs) = case m of
                                 Nothing -> other
                                 Just m -> m : other
                            where other = foldl (\ms e -> ms ++ allMetas e) [] cs
allMetas _ = []

doubles :: [String] -> [String]
doubles ls = let 
    f _ [] = []
    f prev (x:xs) = if prev == x then (x:(f x xs)) else f x xs
    in nub $ f "" (sort ls)

unwordsBy :: String -> [String] -> String
unwordsBy sep words = initSafe $ unwords $ zipWith (++) words (repeat sep)

flattenDoc n (Element t m cs) = (n, (Element t m [])) : (concat $ map (flattenDoc $ n + 1) cs)
flattenDoc n x = [(n,x)]

--printDoc :: Element -> IO ()
printDoc = putStr . unlines . map indent . flattenDoc 1
           where indent (n,x) = replicate n ' ' ++ show x

-- Example 

bDocument = document "DOC" [ page "PAGE" [ question "QUEST" [singleAnswer "JA", singleAnswer "NEIN"]]]

aDocument = document "Ein Fragebogen" 
            [ page "Seite 1" 
                [ question "Eine Frage mit Einfachauswahl-Antworten?"
                    [ singleAnswer "Ja"
                    , singleAnswer "Nein"
                    ]
                , paragraph "Etwas Information zu diesem Fragebogen."
                , question "Eine Frage mit Mehrfachauswahl-Antworten."
                    [ multiAnswer "A"
                    , multiAnswer "B"
                    , multiAnswer "C"
                    ]
                ]
            , page "Seite 2" 
                [ question "Eine Frage mit einer Text-Antwort:"
                    [ freeAnswer 
                    ]
                ]
            , page "Seite 3"
                [ paragraph "Danke für das Feedback"
                ]
            ]
