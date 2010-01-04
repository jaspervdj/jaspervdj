module Main where

import Text.Hakyll (hakyll)
import Text.Hakyll.Render
import Text.Hakyll.Util (trim, split)
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.Renderables (createPagePath, createCustomPage)
import Text.Hakyll.Tags (readTagMap, renderTagCloud)
import Text.Hakyll.Context (renderDate, renderValue, ContextManipulation)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (sort, intercalate)
import Control.Monad (liftM, mapM_)
import Data.Either (Either(..))

main = hakyll $ do
    putStrLn "Copying static directories and compressing css..."
    directory static "images"
    directory static "js"
    directory css "css"

    putStrLn "Finding posts..."
    postPaths <- liftM (reverse . sort) $ getRecursiveContents "posts"
    let renderablePosts = map createPagePath postPaths

    putStrLn "Getting tags..."
    tagMap <- readTagMap postPaths

    putStrLn "Generating index..."
    let recentPosts = renderAndConcatWith postManipulation
                                          "templates/postitem.html"
                                          (take 3 renderablePosts)
    renderChain ["index.html", "templates/default.html"] $
        createCustomPage "index.html" ("templates/postitem.html" : postPaths)
            [("title", Left "Home"), ("posts", Right recentPosts),
             ("tagcloud", Left $ renderTagCloud tagMap (\t -> "/tags/" ++ t ++ ".html") 100 120)]

    putStrLn "Generating rss feed..."
    let recentItems = renderAndConcatWith postManipulation
                                          "templates/rssitem.xml"
                                          (take 5 renderablePosts)
    renderChain ["templates/rss.xml"] $
        createCustomPage "rss.xml" ("templates/rssitem.xml" : postPaths) [("items", Right recentItems)]

    putStrLn "Generating general post list..."
    renderPostList "posts.html" "All posts" postPaths

    putStrLn "Generating all posts..."
    mapM_ (renderChainWith postManipulation ["templates/post.html", "templates/default.html"])
          renderablePosts

    putStrLn "Creating tag post lists..."
    mapM_ (\(t, p) -> renderPostList ("tags/" ++ t ++ ".html")
                        ("Posts tagged " ++ t) (sort $ reverse p)) $ M.toList tagMap

    putStrLn "Generating simple pages..."
    mapM_ (renderChain ["templates/default.html"] . createPagePath)
            [ "contact.markdown"
            , "projects.markdown"
            , "404.html"
            ]

    putStrLn "Succes!"

postManipulation :: ContextManipulation
postManipulation = renderValue "tags" "taglinks" renderTags
                 . renderDate "prettydate" "%B %e, %Y" "Date unknown"
    where renderTags = B.pack . intercalate ", " . map renderTag
                     . map trim . split "," . B.unpack
          renderTag tag = " <a href=\"/tags/" ++ tag ++ ".html\">" ++ tag ++ "</a>"

renderPostList url title posts = do
    putStrLn $ "Generating post list " ++ title ++ "..."
    let postItems = renderAndConcatWith postManipulation "templates/postitem.html" $ map createPagePath posts
    renderChain ["posts.html", "templates/default.html"] $
        createCustomPage url ("templates/postitem.html" : posts)
        [("title", Left title), ("posts", Right postItems)]
