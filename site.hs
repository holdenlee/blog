--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
import           Data.Monoid
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options
import           System.FilePath (takeBaseName, takeDirectory, joinPath, splitPath)
import           Control.Lens hiding (Context)
import           Control.Monad
import           Data.List
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import           Text.Printf
--import qualified Data.Tree as T
import Debug.Trace
import Utilities
import FunTree

{-| Main method -}
main :: IO ()
main = hakyll $ do
    --do nothing to the images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    --compress css files
    match ("css/*.css"
           .||. "highlight/styles/*.css") $ do
        route   idRoute
        compile compressCssCompiler
    --js files
    match ("js/*"
           .||. "highlight/highlight.pack.js"
           .||. "MathJax/config/local/local.js") $ do
      -- .||. "favicon.ico"
      route idRoute
      compile copyFileCompiler

    --build tags http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags postPattern (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" defCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    --building categories is similar to building tags. Exclude uncategorized items.
    categories <- buildNestedCategories (postPattern .&&. (complement "posts/*.md")) (fromCapture "posts/**/index.html")
                  -- .&&. (complement "posts/*.md")
                  --(fromCapture "posts/**/index.html")
    let tagsMapList = nestCategories $ tagsMap categories
    let tagsMap' = tagsMapList & ((mapped . _1) %~ joinPath)
    --let tagsMapList2 = tagsMapList & (mapped . _1 . mapped) %~ removeTrailingSlash
    let treeMap = treeCategories $ tagsMap categories
    let categories' = debugShow $ categories{tagsMap = tagsMap'}
    tagsRules categories' $ \tag pattern -> do
      let title = "Posts in category \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" defCtx (return posts) <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    
    --for each post, apply the post template, then the default template.
    match postPattern $ postRules (return $ (defCtxWithTags tags <> constField "isPost" "true"))

    match topLevelPages $ postRules (return $ defCtx)

    match "index.html" $ do
        route idRoute
        compile $ do
            pandocMathCompiler
            posts <- recentFirst =<< loadAll postPattern
            let indexCtx =
                    listField "posts" defCtx (return posts) <>
                    constField "title" "Mental Wilderness"   <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/post.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["sitemap.html"] $ do
      route idRoute
      compile $ do
        let mapCtx = constField "title" "Sitemap" <> defaultContext
        let pt = debugShow $ makePostTree treeMap
        outline <- compileTree pt
        makeItem outline
          >>= loadAndApplyTemplate "templates/post.html" mapCtx
          >>= loadAndApplyTemplate "templates/default.html" mapCtx
          >>= relativizeUrls

    atomCompiler "content" tags

topLevelPages :: Pattern
topLevelPages = "about.md"

postPattern :: Pattern
postPattern = "posts/**.md"

postRules :: Rules (Context String) -> Rules ()
postRules ctx' = do
        ctx <- ctx'
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= return . fmap demoteHeaders -- h1 -> h2; h2 -> h3; etc
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            --before applying the website template, save a snapshot to send to Atom feed. saveSnapshot :: String -> Item a -> Compiler (Item a)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

{-| Math compiler. See http://jdreaver.com/posts/2014-06-22-math-programming-blog-hakyll.html. -}
pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerReferenceLinks = True,
                          writerHtml5 = True,
                          writerHighlight = True,
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions


--------------------------------------------------------------------------------
defCtx :: Context String
defCtx =
  --"%B %e, %Y" see https://hackage.haskell.org/package/time-1.5.0.1/docs/Data-Time-Format.html for how to format date
    dateField "date" "%F" <>
    defaultContext

defCtxWithTags :: Tags -> Context String
defCtxWithTags tags = tagsField "tags" tags <>
                       defCtx

{-| Feed configuration -}
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Mental Wilderness"
    , feedDescription = "Holden Lee's Blog"
    , feedAuthorName  = "Holden Lee"
    , feedAuthorEmail = "oldheneel@gmail.com"
    , feedRoot        = "http://holdenlee.github.io/blog"
    }

{-| Compiler for feed -}
feedCompiler :: [Identifier] -> (FeedConfiguration -> Context String -> [Item String] -> Compiler (Item String)) -> String -> Tags -> Rules ()
feedCompiler li renderer content tags =
  create li $ do
    route idRoute
    compile $ do
        let feedCtx = (defCtxWithTags tags) `mappend`
                bodyField "description"
        --take 10 most recent posts
        posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" content
        renderer myFeedConfiguration feedCtx posts

{-| Why use Atom? http://nullprogram.com/blog/2013/09/23/ -}
atomCompiler = feedCompiler ["atom.xml"] renderAtom

--rssCompiler = feedCompiler ["rss.xml"] renderRss

{-| keep the entire directory name, cutting off the first part (which will be posts/) -}
getNestedCategory :: MonadMetadata m => Identifier -> m [String]
getNestedCategory = return . return . joinPath . tail . splitPath . takeDirectory . toFilePath
--getCategory = return . return . takeBaseName . takeDirectory . toFilePath

buildNestedCategories :: MonadMetadata m => Pattern -> (String -> Identifier)
                -> m Tags
buildNestedCategories = buildTagsWith getNestedCategory

treeCategories :: [(String, [Identifier])] -> [([String], [Identifier])]
treeCategories = ((mapped . _1) %~ splitPath) .&
                 ((mapped . _1 . mapped) %~ removeTrailingSlash)

nestCategories :: [(String, [Identifier])] -> [([String], [Identifier])]
nestCategories = (((mapped . _1) %~ splitPath) >=>
                 (\(li, y) -> map (,y) (tail $ inits li))) .&
                 ((mapped . _1) %~ removeTrailingSlashList) .&
                 reduce

(.&):: (a -> b) -> (b -> c) -> (a -> c) 
(.&) = flip (.)
  
{- As in, MapReduce -}
reduce :: (Monoid m, Ord a, Ord m) => [(a, m)] -> [(a, m)]
reduce = M.toList . M.map mconcat . MM.toMap . MM.fromList

removeTrailingSlash :: String -> String
removeTrailingSlash = reverse . (\li -> if head li == '/' then tail li else li) . reverse

removeTrailingSlashList :: [String] -> [String]
removeTrailingSlashList = reverse . (ix 0 %~ removeTrailingSlash) . reverse

makePostTree :: [([String], [Identifier])] -> FunTree [String] [Identifier]
makePostTree li =
  for li ([], M.singleton [] ([], [])) (\(li, ids) (rt, m) ->
--note I'm assuming there are no uncategorized posts. Otherwise you have to add a check here if li is already in the map
    (rt, m & M.insert li (ids, [])
-- I want to do this but it isn't right
-- & at (head li) . _2 .~ li
           & M.adjust (_2 %~ (li:)) (li & reversed %~ tail)))
  
--type FunTree l b = (l, M.Map l (b,[l]))
compileTree :: FunTree [String] [Identifier] -> Compiler String
compileTree p@(rt, m) = do
  let (li, ls) = m M.! rt -- :: ([Identifier], [[String]])
  listItemString <- loadAll $ foldl (.||.) "" (map (fromGlob . toFilePath) li)
  postItems <- applyTemplateList postItemTemplate defCtx listItemString
  childrenListStrings <- mapM compileTree (funTreeChildren p)
  let childrenOutline = mconcat $ zipWith (\catPath str -> printf "<li><b>%s</b> %s </li>" (last catPath) str) ls childrenListStrings
  -- \catPath str -> "<li><b>"++(last catPath)++"</b>"++postItems++"</li>"
  return ("<ul>"++childrenOutline++postItems++"</ul>")

postItemTemplate :: Template
postItemTemplate = readTemplate "<li><a href=\"$url$\">$title$</a> ($date$)$if(subtitle)$: <span class=\"italic\">$subtitle$</span> $endif$</li>"

{-mapLi :: [String] -> String
mapLi = mconcat . map (printf "<li>%s</li>\n")-}

{-
getNestedCategory :: MonadMetadata m => Identifier -> m [String]
getNestedCategory = return . return . joinPath . tail . splitPath . takeDirectory . toFilePath
--getCategory = return . return . takeBaseName . takeDirectory . toFilePath

buildNestedCategories :: MonadMetadata m => Pattern -> (String -> Identifier)
                -> m Tags
buildNestedCategories = buildTagsWith getNestedCategory

nestCategories :: [(String, [Identifier])] -> [(String, [Identifier])]
nestCategories = (((mapped . _1) %~ splitPath) >=>
                 (\(li, y) -> map (,y) (tail $ inits li))) .&
                 ((mapped . _1) %~ (removeTrailingSlash . joinPath)) .&
                 -- ^ removeTrailingSlash is necessary here because there is a trailing slash, ex. "math/".
                 reduce

(.&):: (a -> b) -> (b -> c) -> (a -> c) 
(.&) = flip (.)
  
{- As in, MapReduce -}
reduce :: (Monoid m, Ord a, Ord m) => [(a, m)] -> [(a, m)]
reduce = M.toList . M.map mconcat . MM.toMap . MM.fromList

removeTrailingSlash :: String -> String
removeTrailingSlash = reverse . (\li -> if head li == '/' then tail li else li) . reverse
-}
