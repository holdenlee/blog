--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, TupleSections #-}
--http://julien.jhome.fr/posts/2013-05-14-adding-toc-to-posts.html
module TableOfContents where
import           Data.Monoid
import qualified Data.Set as S
import           Hakyll
import           Text.Pandoc.Options
import           Control.Monad
import           Data.List
--import qualified Text.DocTemplates.Internal as TDI
--import qualified Data.Text.Internal as DTI

compileTOCVersion = version "toc" $
   compile $ pandocCompilerWith defaultHakyllReaderOptions
                                defaultHakyllWriterOptions {
                                    writerTableOfContents = True
                                  , writerTemplate = ( "$if(toc)$ $toc$ $endif$")
								  -- Just 
								  -- readTemplate
								  -- :: TDI.Template DTI.Text
                                  -- , writerStandalone = True
                                  }

blankTOCCtx :: Context String
blankTOCCtx = constField "toc" ""

tocCtx :: Context String
tocCtx = field "toc" $ \item ->
            loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
