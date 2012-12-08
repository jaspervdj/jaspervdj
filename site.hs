--------------------------------------------------------------------------------
{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


--------------------------------------------------------------------------------
import qualified Data.ByteString  as B
import           Data.Monoid      (mappend, mconcat)
import           Prelude          hiding (id)
import           System.Cmd       (system)
import           System.Directory (getTemporaryDirectory)
import           System.FilePath  (takeFileName, (<.>), (</>))
import qualified Text.Pandoc      as Pandoc


--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
-- | Entry point
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
        compile $ getResourceBody >>= relativizeUrls

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pageCompiler
                >>= requireApplyTemplate "templates/post.html" (postCtx tags)
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    match "posts.html" $ do
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            posts       <- requireAll "posts/*"
            list        <- applyTemplateList postItemTpl (postCtx tags) $
                recentFirst posts

            makeItem ""
                >>= requireApplyTemplate "templates/posts.html"
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            posts       <- requireAll pattern
            list        <- applyTemplateList postItemTpl (postCtx tags) $
                recentFirst posts

            makeItem ""
                >>= requireApplyTemplate "templates/posts.html"
                        (constField "title" ("Posts tagged " ++ tag) `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            posts       <- requireAll "posts/*"
            list        <- applyTemplateList postItemTpl (postCtx tags) $
                take 3 $ recentFirst posts

            let indexContext = constField "posts" list `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applySelf indexContext
                >>= requireApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>= requireApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pageCompiler
            >>= requireApplyTemplate "templates/default.html" defaultContext

    -- Render RSS feed
    match "rss.xml" $ do
        route idRoute
        compile $ do
            requireAll "posts/*"
                >>= renderAtom feedConfiguration defaultContext

    -- CV as HTML
    match "cv.markdown" $ do
        route   $ setExtension ".html"
        compile $ do
            cvTpl      <- requireBody "templates/cv.html"
            defaultTpl <- requireBody "templates/default.html"
            pageCompiler
                >>= applyTemplate cvTpl defaultContext
                >>= applyTemplate defaultTpl defaultContext
                >>= relativizeUrls

    -- CV as PDF
    match "cv.markdown" $ version "pdf" $ do
        route   $ setExtension ".pdf"
        compile $ do
            cvTpl <- requireBody "templates/cv.tex"
            getResourceBody
                >>= (return . readPandoc)
                >>= (return .
                     fmap (Pandoc.writeLaTeX Pandoc.defaultWriterOptions))
                >>= applyTemplate cvTpl defaultContext
                >>= pdflatex
  where
    pages =
        [ "contact.markdown"
        , "links.markdown"
        , "recommendations.markdown"
        ]


--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    , verbosity = Debug
    }


--------------------------------------------------------------------------------
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    , feedAuthorEmail = "jaspervdj@gmail.com"
    , feedRoot        = "http://jaspervdj.be"
    }


--------------------------------------------------------------------------------
-- | Hacky.
pdflatex :: Item String -> Compiler (Item B.ByteString)
pdflatex item = unsafeCompiler $ do
    tmpDir <- getTemporaryDirectory
    let name = takeFileName $ toFilePath $ itemIdentifier item
        tex  = tmpDir </> name <.> "tex"
        pdf  = tmpDir </> name <.> "pdf"

    writeFile tex $ itemBody item
    _ <- system $ unwords ["pdflatex",
        "-output-directory", tmpDir, tex, ">/dev/null", "2>&1"]
    body <- B.readFile pdf
    return $ itemSetBody body item
