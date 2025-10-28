{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)

import Hakyll
import qualified Data.Text                  as T
import Text.Pandoc.Highlighting (Style, kate, styleToCss)
import Text.Pandoc --(compileTemplate, runPure, Pandoc, runWithDefaultPartials)
import Text.Pandoc.Options

import System.FilePath

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do
        makeItem $ styleToCss kate

    match "bib/*.bib" $ compile biblioCompiler
    match "csl/*.csl" $ compile cslCompiler

    match "autogen/nav.html" $ compile templateCompiler

    match "autogen/*/*.html" $ do
      route $ cleanRoute
      compile $ do
        getResourceBody
        >>= applyAsTemplate postCtx
        >>= loadAndApplyTemplate "templates/quiz.html" postCtx
        >>= relativizeUrls

    match "autogen/*.html" $ do
      route $ cleanRoute
      compile $ do
        getResourceBody
        >>= applyAsTemplate postCtx
        >>= loadAndApplyTemplate "templates/quiz.html" postCtx
        >>= relativizeUrls

    match "content/*" $ do
      route $ cleanRoute
      compile $ pageCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls


    match "content/*/*" $ do
      route $ cleanRoute
      compile $ pageCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls


    match "templates/*" $ compile templateCompiler



postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

pageCompiler :: Compiler (Item String)
pageCompiler = do
  let readerOptions = setReaderWith defaultHakyllReaderOptions
  writerOptions <- getTocOptionsWith defaultHakyllWriterOptions
  bibFile <- load "bib/biblio.bib"
  cslFile <- load "csl/acm-sig-proceedings.csl"
  getResourceBody
    >>= readPandocWith readerOptions
    >>= processPandocBiblio cslFile bibFile
    >>= pure . writePandocWith writerOptions

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    dropHead [] = []
    dropHead (x:xs) = xs

    dropRoot fp = joinPath $ dropHead (splitPath fp)

    createIndexRoute ident = replaceExtension (dropRoot p) ".html"
                            where p = toFilePath ident


setReaderWith :: ReaderOptions -> ReaderOptions
setReaderWith options =
  options
    { readerExtensions =
        readerExtensions options
          <> extensionsFromList
            [ Ext_tex_math_single_backslash,
              Ext_tex_math_double_backslash,
              Ext_tex_math_dollars,
              Ext_latex_macros,
              Ext_raw_attribute
            ]
    }

getTocOptionsWith :: WriterOptions -> Compiler WriterOptions
getTocOptionsWith options = do
  identifier <- getUnderlying
  tocField <- getMetadataField identifier "toc"
  return $ getOptions tocField
  where
    getOptions Nothing
      = options
        {
          writerHighlightStyle = Just kate,
          writerHTMLMathMethod = MathML,
        writerExtensions = writerExtensions options <> extensionsFromList [ Ext_alerts ]
        }
    getOptions (Just _)
      = options
        { writerTableOfContents = True,
          writerTOCDepth = 2,
          writerTemplate = tocTemplate,
          writerHighlightStyle = Just kate,
        writerHTMLMathMethod = MathML,
        writerExtensions = writerExtensions options <> extensionsFromList [ Ext_alerts ]
        }
    tocTemplate
      | Right (Right t) <- build templateSource = Just t
      | otherwise = Nothing
    build = runPure . runWithDefaultPartials . compileTemplate ""
    templateSource = "<aside><nav class='toc'><strong>Contents</strong>\n$toc$\n</nav>\n</aside>\n$body$"


{-
readPandocWith'
    :: ReaderOptions           -- ^ Parser options
    -> Item String             -- ^ String to read
    -> Compiler (Item Pandoc)  -- ^ Resulting document
readPandocWith' ropt item =
    case runPure $ traverse (reader ropt (itemFileType item)) (fmap T.pack item) of
        Left err    -> fail $
            "Hakyll.Web.Pandoc.readPandocWith: parse failed: " ++ show err
        Right item' -> return item'
  where
    reader ro t = case t of
        DocBook            -> readDocBook ro
        Html               -> readHtml ro
        Jupyter            -> readIpynb ro
        LaTeX              -> readLaTeX ro
        LiterateHaskell t' -> reader (addExt ro Ext_literate_haskell) t'
        Markdown           -> readCommonMark ro
        MediaWiki          -> readMediaWiki ro
        OrgMode            -> readOrg ro
        Rst                -> readRST ro
        Textile            -> readTextile ro
        _                  -> error $
            "Hakyll.Web.readPandocWith: I don't know how to read a file of " ++
            "the type " ++ show t ++ " for: " ++ show (itemIdentifier item)

    addExt ro e = ro {readerExtensions = enableExtension e $ readerExtensions ro}
-}
-- [ EOF ]
