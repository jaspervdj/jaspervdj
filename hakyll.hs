--------------------------------------------------------------------------------
{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Arrow    (arr, second, (***), (>>>))
import           Control.Category (id)
import           Control.Monad    (forM_)
import qualified Data.ByteString  as B
import qualified Data.Map         as M
import           Data.Monoid      (mappend)
import           Data.Monoid      (mconcat, mempty)
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

    -- Render each and every post
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ do
            pageCompiler
            -- >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            -- >>> renderTagsField "prettytags" (fromCapture "tags/*")
                >>= requireApplyTemplate "templates/post.html" postContext
                >>= requireApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    -- Post list
    match "posts.html" $ do
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            postsTpl    <- requireBody "templates/posts.html"
            defaultTpl  <- requireBody "templates/default.html"

            posts <- requireAll "posts/*"
            list  <- applyTemplateList postItemTpl postContext $
                recentFirst posts

            makeItem ""
                >>= applyTemplate postsTpl
                        (constField "title" "Posts" `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= applyTemplate defaultTpl defaultContext
                >>= relativizeUrls

    -- Post tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            postsTpl    <- requireBody "templates/posts.html"
            defaultTpl  <- requireBody "templates/default.html"

            posts <- requireAll pattern
            list  <- applyTemplateList postItemTpl postContext $
                recentFirst posts

            makeItem ""
                >>= applyTemplate postsTpl
                        (constField "title" ("Posts tagged " ++ tag) `mappend`
                            constField "posts" list `mappend`
                            defaultContext)
                >>= applyTemplate defaultTpl defaultContext
                >>= relativizeUrls

    -- Index
    match "index.html" $ do
        route idRoute
        compile $ do
            postItemTpl <- requireBody "templates/postitem.html"
            posts       <- requireAll "posts/*"
            list        <- applyTemplateList postItemTpl postContext $
                take 3 $ recentFirst posts

            let indexContext = constField "posts" list `mappend`
                    field "tags" (\_ -> renderTagList tags) `mappend`
                    defaultContext

            getResourceBody
                >>= applySelf indexContext
                >>= requireApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
    {-
    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
    -}

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

    -- Render some static pages
    {-
    forM_ pages $ \p ->
        match p $ do
            route   $ setExtension ".html"
            compile $ pageCompiler
                >>> applyTemplateCompiler "templates/default.html" defaultContext
                >>> relativizeUrlsCompiler
    -}

    -- Render the 404 page, we don't relativize URL's here.
    {-
    match "404.html" $ do
        route idRoute
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html" defaultContext
    -}

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
    {-
    renderTagList' :: Compiler (Tags String) String
    renderTagList' = renderTagList tagIdentifier
    -}

    pages =
        [ "contact.markdown"
        , "links.markdown"
        , "recommendations.markdown"
        ]

{-
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
-}

postContext :: Context String
postContext = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum -ave 'ssh -p 2222' \
                      \_site/* jaspervdj@jaspervdj.be:jaspervdj.be"
    , verbosity = Debug
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
pdflatex :: Item String -> Compiler (Item B.ByteString)
pdflatex item = unsafeCompiler $ do
    tmpDir <- getTemporaryDirectory
    let name = takeFileName $ toFilePath $ itemIdentifier item
        tex  = tmpDir </> name <.> "tex"
        pdf  = tmpDir </> name <.> "pdf"

    writeFile tex $ itemBody item
    system $ unwords ["pdflatex",
        "-output-directory", tmpDir, tex, ">/dev/null", "2>&1"]
    body <- B.readFile pdf
    return $ itemSetBody body item
