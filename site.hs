--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import           IkiWiki
import qualified Data.HashMap.Strict as M

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "css/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"

        compile $ do
            body <- getResourceBody
            let (body', md) = handleWikiLinks (itemBody body)
            let metadataCtxs =
                 map (\(k,v) -> constField k v) (M.toList md)
            let ctx = (mconcat metadataCtxs) `mappend` postCtx
            do
                renderPandoc (itemSetBody body' body)
                >>= loadAndApplyTemplate "templates/post.html"    ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
