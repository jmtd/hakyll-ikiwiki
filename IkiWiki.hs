{-
 - IkiWiki.hs: noodling around with Parsec for IkiWiki links and
 - directives, for injecting into Hakyll
 -
 - Copyright © Jonathan Dowland 2019
 - License: BSD-3-Clause (see COPYING)
 -}

module IkiWiki (ikiRoute, handleWikiLinks) where

import Text.Parsec hiding (label)
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Pandoc.Parsing  hiding (Parser)-- many1Till etc
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (intercalate)
import Data.Either (fromRight)
import Data.Char
import System.FilePath
import Hakyll (toFilePath)

import qualified Data.HashMap.Strict as M

--------------------------------------------------------------------------------
-- "foo/bar.html" → "foo/bar/index.html"
ikiRoute x = dropExtension (toFilePath x) </> "index.html"

-- boilerplate ---------------------------------------------------------------
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""
test1 = "blah blah blah ignore me [[aha]] moo."
moo = regularParse wikiLink -- forces type unification
baa = regularParse wikiDirective-- forces type unification

-- wikilinks -----------------------------------------------------------------
-- http://source.ikiwiki.branchable.com/?p=source.git;a=blob;f=IkiWiki/Plugin/link.pm;h=1ba28eafd6d4912f16d8aa7fa5c16e01ca9f8dea;hb=HEAD#l32
wikiLink = do
    string "[["
    notFollowedBy (char '!')
    label  <- optionMaybe (try wikiLabel)
    link   <- wikiTarget
    anchor <- optionMaybe (try wikiAnchor)
    string "]]"
    return $ "<a href=\"" 
        ++ link 
        ++ (maybe "" ('#':) anchor) 
        ++ "\">" 
        ++ (fromMaybe link label) 
        ++ "</a>"

wikiLabel  = noneOf "]|" `manyTill` char '|'
wikiTarget = many1 (noneOf "\r\n]#")
wikiAnchor = char '#' >> many1 (noneOf " ]")

-- directives ----------------------------------------------------------------
-- http://source.ikiwiki.branchable.com/?p=source.git;a=blob;f=IkiWiki.pm;h=efb48293a3790db6dd6f6f98fd7e753ae43f845a;hb=HEAD#l1700
wikiDirective = do
    string "[[!"
    cmd <- wikiDirectiveCommand
    spaces
    opts <- paramVal `sepEndBy` spaces
    spaces
    string "]]"
    return (cmd, opts)

regexW = oneOf ('_':['a'..'z']++['A'..'Z']++['0'..'9']) --"a-zA-Z0-9_"

wikiDirectiveCommand = many1 (regexW <|> char '-')

-- XXX or use a union type? LabelledParam label value | Param value
data DirectiveParameter = DirectiveParameter
    { label :: Maybe String
    , value :: String
    } deriving (Show)

paramVal = do
    label <- optionMaybe $ try $
        (regexW <|> oneOf "-.") `many1Till` char '='
    value <-  try tripleQuotedValue
          <|> try singleQuotedValue
          <|> try tripleSingleValue
          <|> try hereDoc
          <|> unquotedValue
    return $ DirectiveParameter label value

tripleQuotedValue = string "\"\"\"" >> anyChar     `manyTill` try (string "\"\"\"")
singleQuotedValue = char '"'        >> noneOf "\"" `manyTill` char '"'
tripleSingleValue = string "'''"    >> anyChar     `manyTill` try (string "'''")

hereDoc = do
    string "<<"
    delim <- many1 (oneOf ['a'..'z'] <|> oneOf ['A'..'Z'])
    char '\n'
    anyChar `manyTill` try (char '\n' >> string delim)

unquotedValue     = many1 (noneOf "\"\f\t\n\r ]")
--regexS = oneOf "\f\t\n\r "

-- handling directives -------------------------------------------------------
-- directives can generate in-place text, and/or changes to metadata

-- represents a parsed chunk of ikiwiki content; either text (to pass
-- onto a markup parser) or metadata (to be collated and actioned)
data IkiChunk = IkiChunk
    { text :: String
    , meta :: M.HashMap String String
    } deriving (Show)

handleDirective :: String -> [DirectiveParameter] -> IkiChunk
handleDirective directive params = 
    case directive of
        -- all parameters need to be labelled for meta
        "meta" -> let
            metaparams = M.unions
                       $ reverse -- make it right-biased. Is this necessary?
                       $ map (\(k,v) -> M.singleton k v)
                       $ map (\(DirectiveParameter l v)-> (fromJust l,v))
                       $ filter (\(DirectiveParameter l v)->isJust l) params
            in IkiChunk "" metaparams

        -- labels ignored for tag
        "tag"  -> let v = show (map value params)
                  in  IkiChunk "" (M.singleton "tags" v)

        -- templates: Id parameter required. XXX no other parameters supported yet
        "template" -> let
            params' = map (\(DirectiveParameter l v) -> (l,v)) params
            txt = case lookup (Just "id") params' of
                Nothing  -> "template missing id parameter"
                Just idv -> "$partial(\"templates/"++idv++".html\")$"
            in IkiChunk txt M.empty

        _      -> IkiChunk ("<!-- unhandled directive: " ++ directive ++ "-->") M.empty

-- finding wikilinks in arbitrary text --------------------------------------
-- count 1 here to fix types (need [Char], not anyChar :: Char)
-- but this is kinda sucky, we end up with a list of one-letter strings
-- it'll do for now.
oink = manyTill ((try $ do
                    txt <- wikiLink
                    return $ IkiChunk txt M.empty

                 ) <|> (try $ do
                      (txt, params) <- wikiDirective -- :: (String, [DirectiveParameter])
                      return $ handleDirective txt params
                     )
                 <|> do
                     txt <- count 1 anyChar
                     return $ IkiChunk txt M.empty

                ) eof

-- collate any metadata chunks and join text chunks together
handleWikiLinks :: String -> (String, M.HashMap String String)
handleWikiLinks s =
    case (parse oink "" s) of -- :: Either ParseError [IkiChunk]
        Left  _ -> (s, M.empty)
        Right chunks -> let txt = concat $ map text chunks
                            md  = M.unions $ reverse $ map meta chunks
                        in (txt, md)

-- tests! --------------------------------------------------------------------
-- upstream directive tests:
-- http://source.ikiwiki.branchable.com/?p=source.git;a=blob;f=t/preprocess.t;h=2211e8471dce0b3dad7a54d0a19e2562a3637596;hb=HEAD
