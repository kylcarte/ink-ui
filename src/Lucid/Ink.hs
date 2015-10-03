{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Lucid.Ink where

import Lucid.Ink.Internal
import Data.Text (Text)

-- Alerts {{{

alertBasic_, alertBlock_ :: Text
alertBasic_ = sp "basic"
alertBlock_ = sp "block"

alertInfo_, alertSuccess_, alertError_ :: Text
alertInfo_    = sp "info"
alertSuccess_ = sp "success"
alertError_   = sp "error"

alertAddDismiss_ :: Text
alertAddDismiss_ = ink_ "dismiss"

-- }}}

-- Button {{{

inkButton_ :: Text
inkButton_ = ink_ "button"

buttonGroup_, buttonToolbar_ :: Text
buttonGroup_   = sp "button-group"
buttonToolbar_ = sp "button-toolbar"

-- }}}

-- Color {{{

red_, orange_, yellow_, green_, blue_, black_, grey_, white_ :: Text
red_    = sp "red"
orange_ = sp "orange"
yellow_ = sp "yellow"
green_  = sp "green"
blue_   = sp "blue"
black_  = sp "black"
grey_   = sp "grey"
white_  = sp "white"

-- }}}

-- Positioning/Alignment {{{

pushTop_, pushMiddle_, pushBottom_, pushLeft_, pushCenter_, pushRight_ :: Text
pushTop_    = push_ top_
pushMiddle_ = push_ middle_
pushBottom_ = push_ bottom_
pushLeft_   = push_ left_
pushCenter_ = push_ center_
pushRight_  = push_ right_

textLeft_, textCenter_, textRight_ :: Text
textLeft_   = align_ left_
textCenter_ = align_ center_
textRight_  = align_ right_

-- }}}

-- Flexbox Flow {{{

order_ :: Int -> Text
order_ = sp . prefix "order" . tShow

reverse_, vertical_, horizontal_ :: Text
reverse_  = sp "reverse"
vertical_ = sp "vertical"
horizontal_ = sp "horizontal"

-- }}}

-- Flexbox/Grid Size {{{

inkGrid_ :: Text
inkGrid_ = ink_ "grid"

columnGroup_ :: Text
columnGroup_ = sp "column-group"

gutters_ :: Text
gutters_ = sp "gutters"

autoAll_, autoTiny_, autoSmall_, autoMedium_, autoLarge_, autoXLarge_ :: Text
autoAll_    = onAll_    auto_ 
autoTiny_   = onTiny_   auto_ 
autoSmall_  = onSmall_  auto_ 
autoMedium_ = onMedium_ auto_ 
autoLarge_  = onLarge_  auto_ 
autoXLarge_ = onXLarge_ auto_ 

percAll_, percTiny_, percSmall_, percMedium_, percLarge_, percXLarge_ :: Int -> Text
percAll_    = onAll_    . perc_ 
percTiny_   = onTiny_   . perc_ 
percSmall_  = onSmall_  . perc_ 
percMedium_ = onMedium_ . perc_ 
percLarge_  = onLarge_  . perc_ 
percXLarge_ = onXLarge_ . perc_ 

-- }}}

-- Padding/Margin {{{

paddingHorizontal_, paddingVertical_, paddingTop_, paddingRight_, paddingBottom_, paddingLeft_ :: Text
paddingHorizontal_ = padding_ $ getOrientation Horizontal
paddingVertical_   = padding_ $ getOrientation Vertical
paddingTop_        = padding_ $ getDirection top_
paddingRight_      = padding_ $ getDirection right_
paddingBottom_     = padding_ $ getDirection bottom_
paddingLeft_       = padding_ $ getDirection left_

spaceHorizontal_, spaceVertical_, spaceTop_, spaceRight_, spaceBottom_, spaceLeft_ :: Text
spaceHorizontal_ = space_ $ getOrientation Horizontal
spaceVertical_   = space_ $ getOrientation Vertical
spaceTop_        = space_ $ getDirection top_
spaceRight_      = space_ $ getDirection right_
spaceBottom_     = space_ $ getDirection bottom_
spaceLeft_       = space_ $ getDirection left_

-- }}}

-- Visibility {{{

showElem_, hideElem_ :: Text
showElem_ = sp "show"
hideElem_ = sp "hide"

-- }}}

-- Images {{{

inkImage_ :: Text
inkImage_ = ink_ "image"

capOverTop_, capOverBottom_ :: Text
capOverTop_ = over_ $ getDirection top_
capOverBottom_ = over_ $ getDirection bottom_

captionDark_ :: Text
captionDark_ = sp "dark"

imageQuery_ :: Text
imageQuery_ = sp "imagequery"

-- }}}

-- Navigation {{{

inkNav_ :: Text
inkNav_ = ink_ "navigation"

navMenu_, navBreadcrumbs_, navPagination_, navPills_, navDropdown_ :: Text
navMenu_        = sp "menu"
navBreadcrumbs_ = sp "breadcrumbs"
navPagination_  = sp "pagination"
navPills_       = sp "pills"
navDropdown_    = sp "dropdown"

-- }}}

-- Tables {{{

inkTable_ :: Text
inkTable_ = ink_ "table"

tableAlternating_, tableHover_, tableBordered_ :: Text
tableAlternating_ = sp "alternating"
tableHover_       = sp "hover"
tableBordered_    = sp "bordered"

-- }}}

-- Typography {{{

font100_, font300_, font400_, font500_, font700_, font900_ :: Text
font100_ = fw_ 100
font300_ = fw_ 300
font400_ = fw_ 400
font500_ = fw_ 500
font700_ = fw_ 700
font900_ = fw_ 900

fontSans_, fontSerif_, fontMonospace_, fontCondensed_, fontSlab_ :: Text
fontSans_      = sp "sans"
fontSerif_     = sp "serif"
fontMonospace_ = sp "monospace"
fontCondensed_ = sp "condensed"
fontSlab_      = sp "slab"

fontSmall_, fontMedium_, fontLarge_, fontXlarge_, fontUpper_, fontLower_ :: Text
fontSmall_  = sp "small"
fontMedium_ = sp "medium"
fontLarge_  = sp "large"
fontXlarge_ = sp "extralarge"
fontUpper_  = sp "uppercase"
fontLower_  = sp "lowercase"

-- }}}

active_ :: Text
active_ = sp "active"

clearfix_ :: Text
clearfix_ = sp "clearfix"

note_ :: Text
note_ = sp "note"

unstyled_ :: Text
unstyled_ = sp "unstyled"

inline_ :: Text
inline_ = sp "inline"

halfSpaceVertical_, quarterSpaceVertical_ :: Text
halfSpaceVertical_ = "half-vertical-space"
quarterSpaceVertical_ = "quarter-vertical-space"

halfSpaceHorizontal_, quarterSpaceHorizontal_ :: Text
halfSpaceHorizontal_ = "half-vertical-space"
quarterSpaceHorizontal_ = "quarter-vertical-space"
