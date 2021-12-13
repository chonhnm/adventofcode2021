{-# LANGUAGE OverloadedStrings #-}

import Clay
import Data.Text
import Prelude hiding (div)

bodyStyle :: Css
bodyStyle = body ? do
  background  aquamarine
  fontFamily  ["Helvetica Neue"] [sansSerif]

codeStyle :: Css
codeStyle = code ?
  do fontFamily  ["Monaco", "Inconsolata"] [monospace]
     fontSize    (px 14)
     lineHeight  (ex 1.8)

emphasis :: Css
emphasis = do 
  fontWeight     bold
  color          black
  textTransform  uppercase

container :: Selector
container = div # ".code"

containerStyle :: Css
containerStyle = container ?
  do width (px 800)
     borderColor gray

main :: IO ()
main = putCss $ do
  bodyStyle
  codeStyle
  containerStyle