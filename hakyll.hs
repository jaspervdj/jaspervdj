{-# LANGUAGE OverloadedStrings, Arrows #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Monad (forM_)
import Control.Arrow (arr, (>>>), (***))
import Data.Monoid (mempty, mconcat)
import qualified Data.Map as M

import Hakyll

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
            >>> defaultApplyTemplate "templates/post.html"
            >>> defaultApplyTemplate "templates/default.html"

    -- Post list
    route  "posts.html" idRoute
    create "posts.html" $
        constA mempty
            >>> arr (setField "title" "Posts")
            >>> requireAllA "posts/*" addPostList
            >>> defaultApplyTemplate "templates/posts.html"
            >>> defaultApplyTemplate "templates/default.html"

    -- Index
    route  "index.html" idRoute
    create "index.html" $
        constA mempty
            >>> arr (setField "title" "Home")
            >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
            >>> requireAllA "posts/*" (id *** arr (take 3 . sortByBaseName) >>> addPostList)
            >>> defaultApplyTemplate "templates/index.html"
            >>> defaultApplyTemplate "templates/default.html"

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
                >>> defaultApplyTemplate "templates/default.html"
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
addPostList = setFieldA "posts" $
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
        >>> defaultApplyTemplate "templates/posts.html"
        >>> defaultApplyTemplate "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedRoot        = "http://jaspervdj.be"
    }
