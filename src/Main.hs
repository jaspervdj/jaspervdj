--------------------------------------------------------------------------------
{-# LANGUAGE Arrows             #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main (main) where


--------------------------------------------------------------------------------
import           Data.Monoid     ((<>))
import           Prelude         hiding (id)
import           System.Exit     (ExitCode)
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Data.Text as T
import qualified System.Process  as Process
import qualified Text.Pandoc     as Pandoc


--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | Entry point
main :: IO ()
main = hakyllWith config $ do
    -- Static files
    match ("images/*.jpg" .||. "images/*.png" .||. "images/*.gif" .||.
            "favicon.ico" .||. "files/**") $ do
        route   idRoute
        compile copyFileCompiler

    -- Formula images
    match "images/*.tex" $ do
        route   $ setExtension "png"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/formula.tex" defaultContext
            >>= xelatex >>= pdfToPng

    -- Dot images
    match "images/*.dot" $ do
        route   $ setExtension "png"
        compile $ getResourceLBS >>= traverse (unixFilterLBS "dot" ["-Tpng"])

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/content.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    -- CV as HTML
    match "cv.markdown" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/cv.html"      defaultContext
            >>= loadAndApplyTemplate "templates/content.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- CV as PDF
    match "cv.markdown" $ version "pdf" $ do
        route   $ setExtension ".pdf"
        compile $ do getResourceBody
            >>= readPandoc
            >>= writeXeTex
            >>= loadAndApplyTemplate "templates/cv.tex" defaultContext
            >>= xelatex

    -- Photographs
    match "photos/*.md" $ compile getResourceBody

    -- Photography portfolio
    photoBlog <- buildPaginateWith
        (\ids -> sortRecentFirst ids >>= return . paginateEvery 5)
        "photos/*.md"
        (\n -> if n == 1
            then "photos.html"
            else fromCapture "photos/*.html" (show n))
    paginateRules photoBlog $ \pageNum pattern -> do
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            photos <- recentFirst =<< loadAll pattern  -- Should be just one
            let paginateCtx = paginateContext photoBlog pageNum
            let ctx         =
                    constField "title" "Photos"                        <>
                    listField "photos"
                        (photographCtx <> paginateCtx) (return photos) <>
                    paginateCtx                                        <>
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/photo.html" ctx
                >>= loadAndApplyTemplate "templates/content.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Showcases
    match "photos/showcase/*.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= relativizeUrls
  where
    pages =
        [ "contact.markdown"
        , "links.markdown"
        ]

    writeXeTex :: Item Pandoc.Pandoc -> Compiler (Item String)
    writeXeTex = traverse $ \pandoc ->
        case Pandoc.runPure (Pandoc.writeLaTeX Pandoc.def pandoc) of
            Left err -> fail $ show err
            Right x  -> return (T.unpack x)


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deploySite = deploy
    }
  where
    deploy :: Configuration -> IO ExitCode
    deploy _c = do
        branch <- Process.readProcess
            "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""
        case words branch of
            ["master"] -> Process.rawSystem "rsync"
                [ "--checksum", "-ave", "ssh -p 2222"
                , "_site/", "jaspervdj@jaspervdj.be:jaspervdj.be"
                ]
            ["drafts"] -> Process.rawSystem "rsync"
                [ "--checksum", "-ave", "ssh -p 2222"
                , "_site/", "jaspervdj@jaspervdj.be:jaspervdj.be/staging"
                ]
            _ -> fail $
                "I don't know how to deploy the branch " ++ show branch

--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "jaspervdj - " ++ title
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "jaspervdj@gmail.com"
    , feedRoot        = "http://jaspervdj.be"
    }


--------------------------------------------------------------------------------
-- | Hacky.
xelatex :: Item String -> Compiler (Item TmpFile)
xelatex item = do
    TmpFile texPath <- newTmpFile "xelatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- Process.system $ unwords ["xelatex", "-halt-on-error",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath


--------------------------------------------------------------------------------
pdfToPng :: Item TmpFile -> Compiler (Item TmpFile)
pdfToPng item = do
    let TmpFile pdfPath = itemBody item
        pngPath         = replaceExtension pdfPath "png"
    unsafeCompiler $ do
        _ <- Process.system $ unwords
            ["convert", "-density", "150", "-quality", "90", pdfPath, pngPath]
        return ()
    makeItem $ TmpFile pngPath


--------------------------------------------------------------------------------
photographCtx :: Context String
photographCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , metadataField
    ]
