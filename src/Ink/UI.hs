{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Ink.UI where

import Lucid
import Lucid.Ink
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens.Cons
import Control.Lens.Operators hiding ((.=))
import Data.Monoid
import Control.Applicative

import Web.Scotty
import Network.Wai.Middleware.Static

hostSite :: IO ()
hostSite = do
  buildPage
  scotty 8081 server

server :: ScottyM ()
server = do
  middleware $ staticPolicy (noDots >-> addBase "site/dist")
  get "/" $ do
    setHeader "Content-Type" "text/html"
    file "site/dist/index.html"

type H m = HtmlT m ()

buildPage :: IO ()
buildPage = renderToFile "site/dist/index.html"
  $ page_ "Home" $++
    [ div_ [class_ inkGrid_] $
      div_ [classes_ [columnGroup_,pushCenter_]] $++
      [
      ]
    ]

-- TreeView {{{

data TreeView m a = Node
  { nodeContent :: HtmlT m a
  , nodeLink    :: Either (Maybe Text) [TreeView m a]
  }
deriving instance Eq   (HtmlT m a) => Eq   (TreeView m a)
deriving instance Ord  (HtmlT m a) => Ord  (TreeView m a)
deriving instance Show (HtmlT m a) => Show (TreeView m a)

type TV m = TreeView m ()

class Leaf m lf | lf -> m where
  leaf :: H m -> lf

instance (a ~ ()) => Leaf m (TreeView m a) where
  leaf n = Node n $ Left Nothing

instance (r ~ TreeView m (), txt ~ Text) => Leaf m (txt -> r) where
  leaf n addr = Node n $ Left $ Just addr

branch :: Monad m => H m -> [TV m] -> TV m
branch n ns = Node n $ Right ns

viewTree :: Monad m => TreeView m () -> HtmlT m ()
viewTree = ul_ [class_ "ink-tree-view"] . viewNode True
  where
  viewNode open t = li_ (open ? data_ "open" "true") $++
    case nodeLink t of
      Left (Just addr) -> [ a_ [href_ addr] $ nodeContent t ]
      Left _           -> [ nodeContent t ]
      Right []         -> [ nodeContent t ]
      Right ns@(_:ns') -> [ a_ [href_ "#"]  $ nodeContent t
                          , ul_ $++ viewNode (open && null ns') <$> ns
                          ]

-- }}}

page_ :: Monad m => Text -> H m -> H m
page_ t pg = do
  doctype_
  html_ [lang_ "en"] $++
    [ head_ $++
      [ meta_ [charset_ "utf-8"]
      , meta_
        [ httpEquiv_ "X-UA-Compatible"
        , content_   "IE=edge,chrome=1"
        ]
      , title_ $ toHtml t
      , metadata_
        [ "description"            .= ""
        , "author"                 .= "Kyle Carter"
        , "HandheldFriendly"       .= "True"
        , "MobileOptimized"        .= "320"
        , "mobile-web-app-capable" .= "yes"
        , "viewport"               .=  assigns
          [ "width"         .= "device-width"
          , "initial-scale" .= "1.0"
          , "maximum-scale" .= "1.0"
          , "user-scalable" .= "0"
          ]
        ]
      , cssPath_ $ "css/custom.min.css"
      , cssPath_ $ "css/font-awesome.min.css"
      , jsPath_  $ "js/modernizr.js"
      , js_ $ T.unlines
        [ "Modernizr.load({"
        , "  test: Modernizr.flexbox,"
        , "  nope: '" <> "css/ink-legacy.min.css" <> "'"
        , "});"
        ]
      , jsPath_ $ "js/autoload.js"
      , jsPath_ $ "js/ink-all.min.js"
      , css_ $ T.unlines
        [ ""
        , "body {"
        , "    background: #02402e;"
        , "}\n"
        , ".push, footer {"
        , "    height: 120px;"
        , "    margin-top: 0;"
        , "}\n"
        , "header h1 small:before {"
        , "    content: \"|\";"
        , "    margin: 0 0.5em;"
        , "    font-size: 1.6em;"
        , "}\n"
        , "footer {"
        , "    background: #ccc;"
        , "}\n"
        ]
      ]
    , body_ $
      div_
        [ classes_ [inkGrid_]
        ] $++
        [ header_ [class_ spaceVertical_] $++
          [ h1_ [class_ "ink-flex"] $++
            [ img_
              [ src_ "img/cmw-logo-white.svg"
              , width_ "64"
              ]
            , span_
              [ classes_ ["push-middle","left-space"]
              , style_ "color:white;"
              ] "Schedules"
            ]
          , nav_ [class_ inkNav_] $
            ul_ [classes_ [navMenu_,horizontal_,white_]] $++
            [ li_ [class_ active_] $ a_ [href_ "#"]
              "Entry"
            , li_ $ a_ [href_ "#"]
              "Schedules"
            , li_ $ a_ [href_ "#"]
              "Printouts"
            , li_ $ a_ [href_ "#"]
              "Settings"
            ]
          ]
        , div_ [id_ "page-content"] pg
        , div_ [class_ "push"] blank_
        , footer_ [classes_ [clearfix_]] $++
          [ div_ [class_ inkGrid_] $++
            [ ul_ [classes_ [unstyled_,inline_,halfSpaceVertical_]] $++
              [ li_ [class_ active_] $ a_ [href_ "#"]
                "About"
              , li_ $ a_ [href_ "#"]
                "Sitemap"
              , li_ $ a_ [href_ "#"]
                "Contacts"
              ]
            , p_ [class_ note_] par2
            ]
          ]
        ]
    ]
    where
    -- par1 :: Monad m => H m
    -- par1 = toHtml $ T.unwords
    --   [ "\"Red is not the right word,\" was the reply."
    --   , "\"The plaque was scarlet."
    --   , "The whole face and body turned scarlet in an hour's time."
    --   , "Don't I  know? Didn't I see enough of it?"
    --   , "And I am telling you it was scarlet"
    --   , "because—well, because it was scarlet."
    --   , "There is no other word for it.\""
    --   ]
    par2 :: Monad m => H m
    par2 = toHtml $ T.unwords
      [ "Identification of the owner of the copyright,"
      , "either by name, abbreviation, or other designation"
      , "by which it is generally known."
      ]

inksite :: Text -> Text
inksite = ("http://cdn.ink.sapo.pt/3.1.9" </>)

-- Filepath {{{

(</>) :: Text -> Text -> Text
d </> p
  | Just '/' <- p^?_head
  = p
  | Just '/' <- d^?_last
  = d <> p
  | otherwise
  = d <> "/" <> p
infixr 5 </>

(<.>) :: Text -> Text -> Text
p <.> ext = case ext^?_Cons of
  Just (x,xs) -> case x of
    '.' -> p <> xs
    _   -> p <> "." <> xs
  _           -> p
infixr 7 <.>

-- }}}

-- Builders {{{

js_ :: Monad m => Text -> H m
js_ = script_ [ type_ "text/javascript" ]

jsPath_ :: Monad m => Text -> H m
jsPath_ rf = script_
  [ type_ "text/javascript"
  , src_  rf
  ] ("" :: Text)

css_ :: Monad m => Text -> H m
css_ = style_ [ type_ "text/css" ]

cssPath_ :: Monad m => Text -> H m
cssPath_ rf = link_
  [ rel_  "stylesheet"
  , type_ "text/css"
  , href_ rf
  ]

classes_ :: [Text] -> Attribute
classes_ = class_ . T.unwords

metadata_ :: Monad m => [(Text,Text)] -> H m
metadata_ = foldMap $ uncurry metadatum_

metadatum_ :: Monad m => Text -> Text -> H m
metadatum_ n c = meta_ [ name_ n , content_ c ]

blank_ :: Monad m => H m
blank_ = mempty

-- }}}

-- Util {{{

assigns :: [(Text,Text)] -> Text
assigns = T.intercalate ", " . map (uncurry assign)

assign :: Text -> Text -> Text
assign k v = k <> "=" <> v

(.=) :: a -> b -> (a,b)
a .= b = (a,b)
infixr 0 .=

($++) :: Monoid m => (m -> a) -> [m] -> a
($++) f = f . mconcat 
infixr 0 $++
{-# INLINE ($++) #-}

(?) :: Alternative f => Bool -> a -> f a
b ? a = if b then pure a else empty
infix 4 ?

-- }}}

