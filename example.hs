{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo                                                    #-}

module Main where

import qualified Data.Map         as Map
import           Data.String.Conv
import           Data.Text        (pack)
import           Reflex
import           Reflex.Dom
import           Safe             (readMay)

main = mainWidget $ el "div" $ do
  nx <- numberInput
  d  <- dropdown "*" (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
      resultString = fmap (toS . show) result
  text " = "
  dynText resultString

numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  let attrs = constDyn $ Map.fromList [("style", "border-color: blue")]
      errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      result <- return . fmap (readMay . toS) $ _textInput_value n
      attrs <- return $ fmap (\r -> case r of
                                      Just _  -> validState
                                      Nothing -> errorState)
                             result
  return result

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

stringToOp s = case s of
                    "-" -> (-)
                    "*" -> (*)
                    "/" -> (/)
                    _   -> (+)
