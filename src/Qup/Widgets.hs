{-# LANGUAGE RecursiveDo, TypeFamilies, TemplateHaskell #-}

module Qup.Widgets where 

import Reflex
import Reflex.Dom
import Control.Monad -- (void, foldM, liftM, mapM_, when)
import Control.Lens
import Data.Maybe (catMaybes)
import Data.Default
import qualified Data.Map as Map

import GHCJS.DOM.Element
import GHCJS.DOM.EventM
import GHCJS.DOM.HTMLInputElement
import Control.Monad.IO.Class

radioButtons :: MonadWidget t m => [(String,String)] -> m (Dynamic t String)
radioButtons vals = do
    rec eClicks <- mapM (\(v,l) -> radioButton (tag (constant False) $ ffilter (/=v) $ updated dValue) v l) vals
        dValue <- holdDyn "" $ leftmost eClicks
    return dValue
    where radioButton setChecked val label = el "div" $ el "label" $ do
            let attrs = constDyn $ "type" =: "radio"
            e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
            performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ setChecked
            text label
            eClick <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
            return $ tag (constant val) eClick 
    
range :: MonadWidget t m => String -> String -> [String] -> m (Dynamic t String)
range start end vals = do
    text start
    rec eClicks <- mapM (\v -> radioButton (tag (constant False) $ ffilter (/=v) $ updated dValue) v) vals
        dValue <- holdDyn "" $ leftmost eClicks
    text end
    return dValue
    where radioButton setChecked val = do
            let attrs = constDyn $ "type" =: "radio"
            e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
            performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ setChecked
            eClick <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
            return $ tag (constant val) eClick 


data RadioButtonConfig t =
     RadioButtonConfig { _radioButtonConfig_setValue :: Event t Bool
                       , _radioButtonConfig_attributes :: Dynamic t (Map.Map String String)
                       }

data RadioButton t =
     RadioButton { _radioButton_value :: Dynamic t Bool
                 , _radioButton_true  :: Event t Bool
                 }

makeLenses ''RadioButtonConfig
makeLenses ''RadioButton

instance HasValue (RadioButton t) where
    type Value (RadioButton t) = Dynamic t Bool
    value = _radioButton_value

instance Reflex t => Default (RadioButtonConfig t) where
    def = RadioButtonConfig { _radioButtonConfig_setValue = never
                            , _radioButtonConfig_attributes = constDyn Map.empty
                            }
man = mainWidget $ do
    rec (RadioButton r1 t1) <- radioButton False $ def & radioButtonConfig_setValue .~ t2
        display r1
        (RadioButton r2 t2) <- radioButton True $ def {-& radioButtonConfig_setValue .~ t1-}
        display r2
    return ()

radioButton :: MonadWidget t m => Bool -> RadioButtonConfig t -> m (RadioButton t)
radioButton checked config = do
    attrs <- mapDyn (\r -> Map.insert "type" "radio" 
                           $ (if checked then Map.insert "checked" "checked" else Map.delete "checked") r)
                    $ config^.radioButtonConfig_attributes
    e <- liftM castToHTMLInputElement $ buildEmptyElement "input" attrs
    eClick <- wrapDomEvent e elementOnclick $ liftIO $ htmlInputElementGetChecked e
--    performEvent_ $ fmap (\v -> liftIO $ htmlInputElementSetChecked e $! v) $ _radioButtonConfig_setValue config
    dValue <- holdDyn checked $ leftmost [eClick, _radioButtonConfig_setValue config]
    trueEvent <- return $ ffilter id $ updated $ dValue
    return $ RadioButton dValue trueEvent


guardWidget visible content = do
    let getAttrs True = "style" =: "display:inherit"
        getAttrs False = "style" =: "display:none"
    attrs <- mapDyn getAttrs visible
    elDynAttr "span" attrs content

pageControl :: MonadWidget t m => String -> String -> m (Event t (), Event t ())
pageControl pt nt = el "div" $ do 
    prevE <- button pt
    nextE <- button nt
    return $ (prevE,nextE)

data PageConfig t = 
     PageConfig { _pageConfig_visible :: Bool
                --, _pageConfig_title :: String
                , _pageConfig_prev
                , _pageConfig_next :: Event t ()
                }

demo = mainWidget $ el "div" $ do
    el "h1" $ text "Pages demo"
    trans <- pages $ map p [1..5]
    return ()
    where p x = (el "h2" $ text $ "Page " ++ show x) >> text "Content"

pages :: MonadWidget t m => [m b] -> m (Event t ())
pages ps = el "div" $ mdo
    pagesChronTrans (prevE,nextE) ps
    (prevE,nextE) <- pageControl "Previous" "Next"
    return $ leftmost [prevE,nextE]
{-
pagesOnlyNextTrans (prevE,nextE) (p:ps) = do
    let configV v name = PageConfig v name prevE nextE
        config name = configV False name
    trans1 <- page (configV True "Page One") never p
    foldM (\(b,a) c -> page (config "Some Name") a c) trans1 ps
--}
pagesChronTrans :: MonadWidget t m => (Event t (), Event t ()) -> [m b] -> m (Maybe (Event t ()), Maybe (Event t ()))
pagesChronTrans (prevE,nextE) ps = do
    let f _ _ [] = return (Nothing,Nothing) 
        f (PageConfig v prevE nextE) (_,a) (c:ps) = mdo 
            (bc,ac) <- page (PageConfig v (occurMaybe a prevE) (occurMaybe b nextE)) (leftmost $ catMaybes [b,a]) c
            (b,_) <- f (PageConfig False prevE nextE) (Just bc, Just ac) ps
            return (Just bc, Just ac)
    f (PageConfig True prevE nextE) (Nothing,Nothing) ps

occurMaybe (Just _) e = e
occurMaybe Nothing _  = never
{-
demoOld = mainWidget $ el "div" $ mdo
    let configV v name = PageConfig v name prevE nextE
        config name = configV False name
    (beforeP1, afterP1) <- page (configV True "Page One") (leftmost [afterP3,beforeP2]) $ do
        text "CONTENT 1"
    (beforeP2, afterP2) <- page (config "Page TWO") (leftmost [afterP1,beforeP3]) $ do
        text "CONTENT 2"
    (beforeP3, afterP3) <- page (config "Page 3") (leftmost [afterP2,beforeP1]) $ do
        text "CONTENT 3"
    (prevE, nextE) <- pageControl "Previous" "Next"
    return ()
--}
page :: MonadWidget t m => PageConfig t -> Event t () -> m b -> m (Event t (), Event t ())
page (PageConfig v pe ne) becomeVisible c = el "div" $ do
    visible <- holdDyn v $ leftmost 
        [tag (constant True) becomeVisible
        ,tag (constant False) $ leftmost [pe,ne]]
    let dsp = el "div" $ c >> return ()
    mapDyn (\v -> when v dsp) visible >>= dyn
    let gv e = gate (current visible) e --only send becomeVisible if visible itself and e is not never
    return $ (gv pe, gv ne) 

multiAnswers :: MonadWidget t m => String -> m (Dynamic t Bool)
multiAnswers t = do
    box <- el "label" $ do 
        c <- checkbox False def
        text $ " " ++ t
        return c
    return $ _checkbox_value box

limitedTextArea :: MonadWidget t m => Int -> m (TextArea t) 
limitedTextArea u = do
    rec box <- textArea $ TextAreaConfig "" never $ constDyn ("maxlength" =: (show u))
    numChars <- mapDyn length $ value box
    el "div" $ do
        display numChars
        text " Zeichen"
    return box
        
textAnswer :: MonadWidget t m => String -> m (Dynamic t String)
textAnswer t = do
    el "div" $ text t
    box <- limitedTextArea 140
    return $ value box
