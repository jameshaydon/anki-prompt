module AnkiPrompt
  ( ankiPrompt,
  )
where

import Anki.Misc
import Anki.Note
import Anki.UserProfile
import Data.Default
import qualified Data.Text as Text
import Data.Tree
import Database.SQLite.Simple (close, open, query_)
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Prettyprinter.Render.Terminal as RTerm
import Prettyprinter.Render.Text as Text
import Protolude
import qualified System.Console.ANSI as Term
import qualified System.Console.Terminal.Size as Term
import System.Directory (getHomeDirectory)
import System.FilePath (joinPath)
import Text.HTML.Parser
import Text.HTML.Tree

ankiPrompt :: IO ()
ankiPrompt = do
  as <- getArgs
  case as of
    [user, ankiDir] -> do
      cs <- getCa pathCfg userCfg
      for_ cs $ \c -> renderTerm c
      where
        pathCfg = def {apcRoot = ankiDir} -- "Library/Application Support/Anki2"
        userCfg = UserProfile user
    _ -> putStrLn ("Expecting exactly two arguments." :: Text)

-- | Extract all cards from from the given profile.
getCa :: AnkiPathsConfiguration -> UserProfile -> IO [D]
getCa AnkiPathsConfiguration {..} UserProfile {..} = do
  ns <-
    bracket
      (open . joinPath . (: [apcRoot, upName, apcCollection]) =<< getHomeDirectory)
      close
      (`query_` "select * from notes order by random() limit 1")
  pure (foo <$> ns)
  where
    foo Note {..} = maybe mempty dispAnsi (disp [getNoteField f | f <- take 3 noteFlds])

-- <> show n

disp :: [Text] -> Maybe Prompt
disp (traverse (tokensToForest . parseTokens) . filter (not . Text.null) -> fs_) = case fs_ of
  Right (f : fs) -> Just $ Prompt (dispForest f) (dispForest <$> fs)
  _ -> Nothing

dispForest :: Forest Token -> Inline
dispForest = foldMap dispTree

dispTree :: Tree Token -> Inline
dispTree = \case
  Node (TagSelfClose "br" _) _ -> Br
  Node (TagOpen "b" _) f -> B $ dispForest f
  Node (TagOpen "e" _) f -> E $ dispForest f
  Node (ContentText t) _ -> T t
  Node _ f -> dispForest f

data Inline = T Text | B Inline | E Inline | Span [Inline] | Br
  deriving stock (Show)

instance Semigroup Inline where
  Span xs <> Span ys = Span (xs <> ys)
  x <> Span xs = Span (x : xs)
  Span xs <> x = Span (xs ++ [x])
  x <> y = Span [x, y]

instance Monoid Inline where
  mempty = Span []

data Prompt = Prompt Inline [Inline]
  deriving stock (Show)

type D = Doc AnsiStyle

dispAnsi :: Prompt -> D
dispAnsi (Prompt p ts) =
  vsep [annotate (color Red) (dispInline p), sep ["➤" <+> dispInline t | t <- ts]]

dispInline :: Inline -> D
dispInline = \case
  T t -> pretty t
  Span is -> foldMap dispInline is
  B b -> annotate bold $ dispInline b
  E e -> annotate italicized $ dispInline e
  Br -> " "

renderTerm :: D -> IO ()
renderTerm x = do
  hasAnsi <- Term.hSupportsANSI stdout
  w_ <- Term.size
  let wid = maybe 80 Term.width w_
      opts = LayoutOptions {layoutPageWidth = AvailablePerLine wid 1.0}
      docStream = layoutSmart opts x
      rdr = if hasAnsi then RTerm.renderStrict else Text.renderStrict
  putStrLn (rdr docStream)
