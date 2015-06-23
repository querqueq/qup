module Qup.Conditions where

import Reflex
import Reflex.Dom

import Qup.Widgets

type Id = String

test = mainWidget $ do
    text "Guard Demo"
    answer1 <- el "div" $ multiAnswers "Check me!" 
    answer2 <- el "div" $ multiAnswers "Don't check me!" 
    let lookout1 = lookout ["ID01"] $ updated answer1
        lookout2 = lookout ["ID02"] $ updated answer2
        events = leftmost [lookout1, lookout2]
    guard ["ID01","ID02"] events $ el "div" $ text "This but one guard"
    return ()

-- TODO Change event to Event t a and a with a constraint on something
-- for conditions
lookout :: Reflex t => [Id] -> Event t Bool -> Event t ([Id], Bool)
lookout ids e = attach (constant ids) e

-- TODO Generalize so it can do other things than toogling visibility
guard :: MonadWidget t m => [Id] -> Event t ([Id], Bool) -> m a -> m a
guard ids e widget = do
    let getAttrs True = "style" =: "display:inherit"
        getAttrs False = "style" =: "display:none"
        trigger = snd $ splitE $ ffilter (\(idsE,_) -> or $ map ((flip elem) ids) idsE) e
    asDyn <- holdDyn False trigger -- eventually add parameter for init value of holdDyn
    attrs <- mapDyn getAttrs $ asDyn
    elDynAttr "span" attrs $ widget
