module Main where

import qualified Data.Map as M
import Control.Arrow ((>>>))
import Data.List (sort, intercalate)
import Control.Monad (liftM, mapM_)
import Control.Monad.Reader (liftIO)
import Data.Either (Either(..))

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Feed (FeedConfiguration (..), renderRss)
import Text.Hakyll.Util (trim, link)
import Text.Hakyll.File (getRecursiveContents, directory, removeSpaces)
import Text.Hakyll.CreateContext (createPage, createCustomPage, createListing)
import Text.Hakyll.Tags (readTagMap, renderTagCloud, renderTagLinks, withTagMap)
import Text.Hakyll.ContextManipulations (renderDate, renderValue)

main = hakyll "http://jaspervdj.be" $ do
    directory static "images"
    directory static "js"
    directory static "files"
    directory css "css"
    static "favicon.ico"

    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map ((>>> postManipulation) . createPage) postPaths

    let tagMap = readTagMap "postTagMap" postPaths

    let tagCloud = renderTagCloud tagToURL 100 120
        index = createListing "index.html"
                              ["templates/postitem.html"]
                              (take 3 renderablePosts)
                              [ ("title", Left "Home")
                              , ("tagcloud", Right $ tagMap >>> tagCloud)
                              ]
    renderChain ["index.html", "templates/default.html"] index

    renderRss feedConfiguration $ take 5 renderablePosts

    renderPostList "posts.html" "All posts" renderablePosts

    mapM_ (renderChain ["templates/post.html", "templates/default.html"])
          renderablePosts

    withTagMap tagMap $ \tag posts ->
        renderPostList (tagToURL tag) ("Posts tagged " ++ tag)
                       (map (>>> postManipulation) posts)

    -- Some static pages.
    mapM_ (renderChain ["templates/default.html"] . createPage)
            [ "contact.markdown"
            , "cv.markdown"
            , "404.html"
            ]

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedUrl         = "rss.xml"
    , feedTitle       = "jaspervdj - a personal blog"
    , feedDescription = "Personal blog of jaspervdj"
    , feedAuthorName  = "Jasper Van der Jeugt"
    }

tagToURL :: String -> String
tagToURL tag = "$root/tags/" ++ (removeSpaces tag) ++ ".html"

postManipulation =   renderTagLinks tagToURL
                 >>> renderDate "prettydate" "%B %e, %Y" "Date unknown"

renderPostList url title posts = do
    let page = createListing url ["templates/postitem.html"]
                             posts [("title", Left title)]
    renderChain ["posts.html", "templates/default.html"] page
