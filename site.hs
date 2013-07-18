--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith ytConfiguration $ do
    tags <- buildTags "posts/*" $ fromCapture "tags/*.html"
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Публікації з тегом '" ++ tag ++ "'"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html"
                        (constField "title" title `mappend`
                         listField "posts" (tagsCtx tags) (return posts) `mappend`
                         defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                         defaultContext)
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (tagsCtx tags)
            -- TODO Make URLs external here
            -- TODO Make sure Disqus comments don't appear in feed text
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Архів"               `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        loadAllSnapshots "posts/*" "content"
        >>= recentFirst
        >>= renderRss feedConfiguration feedCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , defaultContext
    ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
  [ tagsField "prettytags" tags
  , postCtx
  ]

feedCtx :: Context String
feedCtx = mconcat
 [ bodyField "description"
 , postCtx
 ]

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/post.html"
    posts <- preprocess' =<< loadAll pattern
    applyTemplateList postItemTpl (tagsCtx tags) posts

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  {feedTitle = "Lviv FP User group"
  , feedDescription = "Блог львівської групи користувачів функціонального програмування"
  , feedAuthorName = "Yura Taras"
  , feedAuthorEmail = "yura.taras@gmail.com"
  , feedRoot = "http://ythakyll.herokuapp.com/"
  }

ytConfiguration = defaultConfiguration
  { deployCommand = "./publish.sh"
  }
