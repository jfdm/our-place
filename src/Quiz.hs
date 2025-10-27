{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where

import Prelude hiding (div, span)
import GHC.Generics
import Data.Yaml
import Data.Functor
import Data.List hiding (span)
import Data.Maybe
import qualified Data.ByteString as BS

import Control.Monad

import System.Environment
import System.Directory
import System.FilePath

generateQuizID :: Int -> String
generateQuizID ctr = "quiz-" ++ show ctr

generateQuestionID :: Int -> String
generateQuestionID ctr = "q" ++ show ctr

data Kind
  = TrueFalse
  | MCQ
  | Short
  deriving (Show, Eq, Generic)

instance FromJSON Kind where
  parseJSON (String "truefalse") = pure TrueFalse
  parseJSON (String "mcq") = pure MCQ
  parseJSON (String "short") = pure Short
  parseJSON _ = fail "invalid question type"

toInput :: Kind -> String
toInput TrueFalse = "radio"
toInput MCQ = "checkbox"
toInput Short = "text"

data Choice
  = Choice
  { value :: String
  , correct :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON Choice

choice :: String -> Kind -> Int -> Choice -> [String]
choice qid k ctr c
  = [ "\t\t<li>"
    , unwords $
      [ "\t\t\t<label><input type=\"" ++ toInput k ++ "\""
      , "name=\"" ++ qid ++ "\""
      ] ++ showWhat k ++
      [ "</label>"]
    , "\t\t</li>"
    ]

  where
    showWhat :: Kind -> [String]
    showWhat Short = ["/>"]
    showWhat _     = ["value=\"" ++ (show ctr) ++ "\"/>"
                     , showStrip $ value c
                     ]
choices' :: Kind -> String -> Int -> [Choice] -> [String]
choices' _ _ _ [] = []
choices' k qid i (c:cs)
  = choice qid k i c ++ choices' k qid (i+1) cs

data Question
  = Question
  { title    :: String
  , kind     :: Kind
  , choices  :: [Choice]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Question

question :: Int -> Question -> [String]
question ctr q
  = ["<div class=\"quizlib-question\">"
    , "\t<div class=\"quizlib-question-title\">" ++  (show ctr) ++ ") " ++ showStrip (title q) ++ "</div>"
    , "\t <div class=\"quizlib-question-answers\">"
    , "\t<ul>"
    ]
      ++
      choices' (kind q) (generateQuestionID ctr) 1 (choices q)
      ++
    [ "\t</ul>"
    , "\t</div>"
    , "</div>"
    ]

data Quiz
  = Quiz
  { name :: String
  , questions :: [Question]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Quiz

questions' :: Int -> [Question] -> [ String ]
questions' i [] = []
questions' i (q:qs)
  = question i q ++ questions' (i + 1) qs

quiz :: Quiz -> [String]
quiz = quiz' 1

quiz' :: Int -> Quiz -> [String]
quiz' i q
  =
    [ "<fieldset id=\"" ++ generateQuizID i ++ "\">"
    , "\t<legend>" ++ showStrip (name q) ++ "</legend>"
    ]
      ++
      questions' i (questions q)
      ++
      [ "<button type=\"button\" onclick=\"showResults(this.parentNode.id);\">Check Answers</button>"
      , "<div id=\"quiz-result\" class=\"button\">"
      ,  "You Scored&nbsp;<span id=\"quiz-percent\"></span>%&nbsp;-&nbsp;<span id=\"quiz-score\"></span>/<span id=\"quiz-max-score\"></span><br/>"
      , "</div>"
      , "</fieldset>"
      ]

fchoice :: Kind -> Int -> Choice -> Maybe String
fchoice k i c = maybe Nothing (canShow k) (correct c)
  where
    canShow :: Kind -> Bool -> Maybe String
    canShow Short True = Just (showStrip (value c))
    canShow _     b = if b then (Just (show i)) else Nothing

fchoices :: Int -> Kind -> [Choice] -> [Maybe String]
fchoices _ k [] = []
fchoices i k [c] = [fchoice k i c]
fchoices i k (c:x:cs)
    = fchoice k i c : fchoices (i + 1) k (x:cs)

fquestion :: Question -> String
fquestion q
  = show $ mapMaybe (id) $ fchoices 1 (kind q) $ (choices q)

fquestions :: Int -> [Question] -> [String]
fquestions i [] = []
fquestions i [x] = [fquestion x]
fquestions i (x:y:xs)
    = [fquestion x, ","] ++ fquestions (i + 1) (y:xs)

fquiz :: Int -> Quiz -> [String]
fquiz i qs
  = [ "quizzes[\'"++ generateQuizID i ++ "\'] = new Quiz(\'" ++ generateQuizID i ++ "\', ["
    ]
      ++
      fquestions i (questions qs)
      ++
    ["]);"
    ]

footer :: Quiz -> [String]
footer q
  = [ "<script type=\"text/javascript\">"
    , "window.onload = function() {"
    ]
      ++ fquiz 1 q
      ++
    [ "};"
    , "</script>"
    ]

readQuizFile :: FilePath -> IO Quiz
readQuizFile file = do
  f <- BS.readFile file
  case decodeEither' f of
    Left err ->
      error (show err)
    Right input -> do
      pure input

parseArgs :: [String] -> Maybe (String, String)
parseArgs [x,y]
  = Just (x,y)
parseArgs _
  = Nothing

usageStr :: String
usageStr
  = unlines
    [ "Incorrect arguments specified"
    , ""
    , "Usage cabal run --builddir _dist GenerateQuiz -- FILE_IN FILE_OUT"
    , ""
    , "FILE_IN is input YAML program specification"
    , "FILE_OUT is where to store the result"
    ]

myMain :: (String, String) -> IO ()
myMain (fin, fout)
  = do qz <- readQuizFile fin

       let body = unlines $ quiz qz ++ footer qz
       writeFile fout body


main :: IO ()
main
  = do args <- getArgs
       maybe (putStrLn usageStr)
             myMain
             (parseArgs args)

showStrip :: String -> String
showStrip = (init . tail . show)

-- [ EOF ]
