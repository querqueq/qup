module Qup.Parse (quParser, example) where

-- Parses a string into an ADT defined in Qup.Data

import Control.Applicative
import Control.Monad (void)
import Text.Parsec hiding (optional, many, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Control.Lens hiding (noneOf)

import qualified Qup.Data as Data

quParser :: Parser Data.Element
quParser = do
    h <- heading
    ps <- some page
    return $ Data.document h ps

heading :: Parser String
heading = do 
    text <- line 
    string $ replicate (length text) '='
    endOfLine
    endOfLine
    return text

pageStart :: Parser String
pageStart = do 
    title <- parseTitle <* endOfLine
    string $ replicate (length title) '-'
    endOfLine
    endOfLine
    return title

page :: Parser Data.Element
page = do 
    title <- pageStart
    cs <- (guard <|> content) `manyTill` eop
    return $ Data.page title cs & Data.element_meta . _Just . Data.meta_id ?~ nospace title

guard :: Parser Data.Element
guard = do
    cond <- char '[' *> some (alphaNum <|> char '.') <* char ']' <* endOfLine
    c <- guard <|> content <|> answer
    return $ Data.guard cond c

content :: Parser Data.Element
content = do
    start <- contentStart
    id <- optionMaybe atId
    endOfLine
    element <- questionBody start <|> paragraphBody start
    endOfLine
    return $ element & Data.element_meta . _Just . Data.meta_id .~ id

paragraphBody :: String -> Parser Data.Element
paragraphBody start = do
    more <- many $ (some $ noneOf "\n\r") <* endOfLine 
    return $ Data.paragraph $ unlines $ start : more

questionBody :: String -> Parser Data.Element
questionBody text = do
    answers <- some $ guard <|> answer
    return $ Data.question text answers

-- | @eop@ succeeds when the end of a page or the end
-- of a file is reached. This parser does not consume 
-- any input
eop :: Parser ()
eop = eof <|> (try (lookAhead pageStart) >> return ())

parseTitle :: Parser String
parseTitle = do 
    start <- noneOf answerTypes
    title <- lineContent
    return $ start:title

answer :: Parser Data.Element
answer = do 
    answerType <- oneOf answerTypes
    space
    text <- lineContent
    maybeId <- optionMaybe atId
    endOfLine
    return $ Data.answer (answerFor answerType) text & Data.element_meta . _Just . Data.meta_id .~ maybeId
    -- Answer (set meta_id maybeId defs) (answerFor answerType) text

answerTypes = "#*>"
answerFor '#' = Data.MultiAnswer
answerFor '*' = Data.SingleAnswer
answerFor '>' = Data.FreeAnswer 

line :: Parser String
line = (many $ noneOf "\n\r") <* endOfLine

lineContent :: Parser String
lineContent = (many $ noneOf "@\n\r")

contentStart :: Parser String
contentStart = do 
    s <- noneOf answerTypes
    e <- many $ noneOf "@\n\r"
    return $ s : e

atId :: Parser String
atId = char '@' *> many alphaNum

nospace = tail . foldl (\x y -> x ++ "_" ++ y) ""  . words

parsedEx = parse quParser "An error occured" $ unlines example
rightParsedEx = case parsedEx of
                     (Right d) -> d
                     (Left _)  -> undefined

example :: [String]
example = 
    [ "Fragebogen Titel"
    , "================"
    , ""                                            
    , "Seite 1"
    , "-------"
    , ""
    , "Eine Frage mit Single-Choice-Antworten?"
    , "* A"
    , "* B"
    , "* C"
    , ""
    , "Eine Frage mit Multiple-Choice-Answer? @question2"
    , "# A"
    , "# B @answerB"
    , "[answerB]"
    , "# C"
    , ""
    , "Seite 2"
    , "-------"
    , ""
    , "Eine Frage mit Text-Answer?"
    , "> "
    , ""
    , "Seite 3"
    , "-------"
    , ""
    , "[answerB]"
    , "Ein Paragraph mit mehreren"
    , "Zeilen."
    , ""
    ]
