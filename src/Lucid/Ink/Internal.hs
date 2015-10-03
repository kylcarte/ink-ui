{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Lucid.Ink.Internal where

import GHC.Exts (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))

data Orientation
  = Vertical
  | Horizontal
  deriving (Eq,Ord,Show)

newtype Direction (o :: Orientation) = Direction
  { getDirection :: Text
  } deriving (Eq,Ord,Show,IsString,Monoid)

newtype Size = Size
  { getSize :: Text
  } deriving (Eq,Ord,Show,IsString,Monoid)

onAll_, onTiny_, onSmall_, onMedium_, onLarge_, onXLarge_ :: Size -> Text
onAll_    = sp . prefix "all"    . getSize
onTiny_   = sp . prefix "tiny"   . getSize
onSmall_  = sp . prefix "small"  . getSize
onMedium_ = sp . prefix "medium" . getSize
onLarge_  = sp . prefix "large"  . getSize
onXLarge_ = sp . prefix "xlarge" . getSize

auto_ :: Size
auto_ = "auto"

perc_ :: Int -> Size
perc_ = (<> "%") . gShow

getOrientation :: Orientation -> Text
getOrientation = \case
  Vertical   -> "vertical"
  Horizontal -> "horizontal"

push_ :: Direction o -> Text
push_ = sp . prefix "push" . getDirection

align_ :: Direction Horizontal -> Text
align_ = sp . prefix "align" . getDirection

top_, middle_, bottom_ :: Direction Vertical
left_, center_, right_ :: Direction Horizontal
top_    = "top"
middle_ = "middle"
bottom_ = "bottom"
left_   = "left"
center_ = "center"
right_  = "right"

padding_ :: Text -> Text
padding_ = sp . postfix "padding"

space_ :: Text -> Text
space_ = sp . postfix "space"

ink_ :: Text -> Text
ink_ = sp . prefix "ink"

over_ :: Text -> Text
over_ = sp . prefix "over"

fw_ :: Int -> Text
fw_ = sp . prefix "fw" . tShow

tShow :: Show a => a -> Text
tShow = T.pack . show

gShow :: (Show a, IsString b) => a -> b
gShow = fromString . show

sp :: Text -> Text
sp t = " " <> t <> " "

prefix :: Text -> Text -> Text
prefix p s = p <> "-" <> s

postfix :: Text -> Text -> Text
postfix = flip prefix

