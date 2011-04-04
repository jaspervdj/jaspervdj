{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***), second)
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M

import Hakyll

-- | Entry point
--
main :: IO ()
main = hakyll $ do
    -- Copy images
    matchPattern "images/*" $ do
        route idRoute
        compile copyFileCompiler

    matchPattern "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy JavaScript
    matchPattern "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy files (deep)
    matchPattern "files/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Compress CSS
    matchPattern "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render the /tmp index page
    matchPattern "tmp/index.html" $ do
        route idRoute
        compile $ readPageCompiler >>> relativizeUrlsCompiler

    -- Render each and every post
    matchPattern "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCaptureString "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"

    -- Post list
    matchPattern "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Index
    matchPattern "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    matchPattern "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr (M.toList . tagsMap)
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Read templates
    matchPattern "templates/*" $ compile templateCompiler

    -- Render some static pages
    forM_ ["contact.markdown", "cv.markdown", "links.markdown"] $ \p ->
        matchPattern p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Render RSS feed
    matchPattern "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

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
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedRoot        = "http://jaspervdj.be"
    }
