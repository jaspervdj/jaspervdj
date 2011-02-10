{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***))
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M

import GHC.Exts (fromString)

import Hakyll.Web
import Hakyll.Web.CompressCss
import Hakyll.Web.Page
import Hakyll.Web.Page.Metadata
import Hakyll.Web.RelativizeUrls
import Hakyll.Web.Template
import Hakyll.Web.Tags
import Hakyll.Web.Feed
import Hakyll.Core.Identifier
import Hakyll.Core.Identifier.Pattern
import Hakyll.Core.Routes
import Hakyll.Core.Rules
import Hakyll.Core.Compiler
import Hakyll.Core.Run
import Hakyll.Core.Writable
import Hakyll.Core.Util.Arrow
import Hakyll.Main

-- | Entry point
--
main :: IO ()
main = hakyll $ do
    -- Copy images
    route   "images/*" idRoute
    compile "images/*" defaultCopyFile

    route   "favicon.ico" idRoute
    compile "favicon.ico" defaultCopyFile

    -- Copy JavaScript
    route   "js/*" idRoute
    compile "js/*" defaultCopyFile

    -- Copy files (deep)
    route   "files/**" idRoute
    compile "files/**" defaultCopyFile

    -- Compress CSS
    route   "css/*" idRoute
    compile "css/*" defaultCompressCss

    -- Render the /tmp index page
    route   "tmp/index.html" idRoute
    compile "tmp/index.html" $
        pageRead >>> defaultRelativizeUrls

    -- Render each and every post
    route   "posts/*" $ setExtension ".html"
    compile "posts/*" $
        defaultPageRead
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCaptureString "tags/*")
            >>> require "templates/post.html" (flip applyTemplate)
            >>> require "templates/default.html" (flip applyTemplate)

    -- Post list
    route  "posts.html" idRoute
    create "posts.html" $
        constA mempty
            >>> arr (setField "title" "Posts")
            >>> requireAllA "posts/*" addPostList
            >>> require "templates/posts.html" (flip applyTemplate)
            >>> require "templates/default.html" (flip applyTemplate)

    -- Index
    route  "index.html" idRoute
    create "index.html" $
        constA mempty
            >>> arr (setField "title" "Home")
            >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
            >>> requireAllA "posts/*" (id *** arr (take 3) >>> addPostList)
            >>> require "templates/index.html" (flip applyTemplate)
            >>> require "templates/default.html" (flip applyTemplate)
            >>> defaultRelativizeUrls

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    route "tags/*" $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr (M.toList . tagsMap)
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Read templates
    compile "templates/*" defaultTemplateRead

    -- Render some static pages
    forM_ ["contact.markdown", "cv.markdown", "links.markdown"] $ \p -> do
        route   p $ setExtension ".html"
        compile p $
            defaultPageRead
                >>> require "templates/default.html" (flip applyTemplate)
                >>> defaultRelativizeUrls

    -- Render RSS feed
    route  "rss.xml" idRoute
    create "rss.xml" $
        requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- End
    return ()
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier
    tagIdentifier = fromCaptureString "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" postList

-- | Auxiliary compiler: generate a post list from a list of given posts
--
postList :: Compiler [Page String] String
postList =
    require "templates/postitem.html" (\ps t -> map (applyTemplate t) ps)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> require "templates/posts.html" (flip applyTemplate)
        >>> require "templates/default.html" (flip applyTemplate)

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedRoot        = "http://jaspervdj.be"
    }
