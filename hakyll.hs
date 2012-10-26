{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Arrow    (arr, second, (***), (>>>))
import           Control.Category (id)
import           Control.Monad    (forM_)
import qualified Data.ByteString  as B
import qualified Data.Map         as M
import           Data.Monoid      (mconcat, mempty)
import           Prelude          hiding (id)
import           System.Cmd       (system)
import           System.Directory (getTemporaryDirectory)
import           System.FilePath  ((<.>), (</>))
import qualified Text.Pandoc      as Pandoc

import           Hakyll

-- | Entry point
--
main :: IO ()
main = hakyllWith config $ do
    -- Copy images
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    -- Copy files (deep)
    match "files/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ readPageCompiler >>> relativizeUrlsCompiler

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Post list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> setFieldPageList recentFirst
                "templates/postitem.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ readPageCompiler
            >>> requireA "tags" (setFieldA "tags" (renderTagList'))
            >>> setFieldPageList (take 3 . recentFirst)
                    "templates/postitem.html" "posts" "posts/*"
            >>> arr applySelf
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Render some static pages
    forM_ pages $ \p ->
        match p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- CV as PDF
    group "pdf-cv" $
        match "cv.markdown" $ do
            route   $ setExtension ".pdf"
            compile $ readPageCompiler
                >>> pageReadPandoc
                >>> arr (fmap $ Pandoc.writeLaTeX Pandoc.defaultWriterOptions)
                >>> applyTemplateCompiler "templates/cv.tex"
                >>> arr pageBody
                >>> pdflatex "cv"
  where
    renderTagList' :: Compiler (Tags String) String
    renderTagList' = renderTagList tagIdentifier

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

    pages =
        [ "contact.markdown"
        , "cv.markdown"
        , "links.markdown"
        , "recommendations.markdown"
        ]

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "jaspervdj@gmail.com"
    , feedRoot        = "http://jaspervdj.be"
    }

-- | Hacky.
pdflatex :: String -> Compiler String B.ByteString
pdflatex name = unsafeCompiler $ \string -> do
    tmpDir <- getTemporaryDirectory
    let tex = tmpDir </> name <.> "tex"
        pdf = tmpDir </> name <.> "pdf"

    writeFile tex string
    system $ unwords ["pdflatex",
        "-output-directory", tmpDir, tex, ">/dev/null", "2>&1"]
    B.readFile pdf
