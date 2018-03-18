{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsers needed for Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Xmobar.Parsers
    ( parseString
    , parseTemplate
    , parseConfig
    , Widget(..)
    ) where

import Xmobar.Config
import Xmobar.Runnable
import Xmobar.Commands
import Xmobar.Actions

import Control.Monad (guard, mzero)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Perm
import Graphics.X11.Types (Button)

data Widget = Icon String | Text String

type ColorString = String
type FontIndex   = Int

-- | Runs the string parser
parseString :: Config -> String -> IO [(Widget, ColorString, FontIndex, Maybe [Action])]
parseString c s =
    case parse (stringParser (fgColor c) 0 Nothing) "" s of
      Left  _ -> return [(Text $ "Could not parse string: " ++ s
                          , fgColor c
                          , 0
                          , Nothing)]
      Right x -> return (concat x)

allParsers :: ColorString
           -> FontIndex
           -> Maybe [Action]
           -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
allParsers c f a =
        textParser c f a
  <|> try (iconParser c f a)
  <|> try (rawParser c f a)
  <|> try (actionParser c f a)
  <|> try (fontParser c a)
  <|> colorParser f a

-- | Gets the string and combines the needed parsers
stringParser :: String -> FontIndex -> Maybe [Action]
                -> Parser [[(Widget, ColorString, FontIndex, Maybe [Action])]]
stringParser c f a = manyTill (allParsers c f a) eof

-- | Parses a maximal string without color markup.
textParser :: String -> FontIndex -> Maybe [Action]
              -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
textParser c f a = do s <- many1 $
                            noneOf "<" <|>
                              try (notFollowedBy' (char '<')
                                    (try (string "fc=")  <|>
                                     try (string "fn=")  <|>
                                     try (string "action=") <|>
                                     try (string "/action>") <|>
                                     try (string "icon=") <|>
                                     try (string "raw=") <|>
                                     try (string "/fn>") <|>
                                     string "/fc>"))
                      return [(Text s, c, f, a)]

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: ColorString
          -> FontIndex
          -> Maybe [Action]
          -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
rawParser c f a = do
  string "<raw="
  lenstr <- many1 digit
  char ':'
  case reads lenstr of
    [(len,[])] -> do
      guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      s <- count (fromIntegral len) anyChar
      string "/>"
      return [(Text s, c, f, a)]
    _ -> mzero

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        notFollowedBy $ try (e >> return '*')
                        return x

iconParser :: String -> FontIndex -> Maybe [Action]
              -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
iconParser c f a = do
  string "<icon="
  i <- manyTill (noneOf ">") (try (string "/>"))
  return [(Icon i, c, f, a)]

actionParser :: String -> FontIndex -> Maybe [Action] -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
actionParser c f act = do
  string "<action="
  command <- choice [between (char '`') (char '`') (many1 (noneOf "`")),
                   many1 (noneOf ">")]
  buttons <- (char '>' >> return "1") <|> (space >> spaces >>
    between (string "button=") (string ">") (many1 (oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- manyTill (allParsers c f a') (try $ string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons = map (\x -> read [x])

-- | Parsers a string wrapped in a color specification.
colorParser :: FontIndex -> Maybe [Action] -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
colorParser f a = do
  c <- between (string "<fc=") (string ">") colors
  s <- manyTill (allParsers c f a) (try $ string "</fc>")
  return (concat s)

-- | Parsers a string wrapped in a font specification.
fontParser :: ColorString -> Maybe [Action] -> Parser [(Widget, ColorString, FontIndex, Maybe [Action])]
fontParser c a = do
  f <- between (string "<fn=") (string ">") colors
  s <- manyTill (allParsers c (read f) a) (try $ string "</fn>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char '#')

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c = do
  s   <- allTillSep c
  com <- templateCommandParser c
  ss  <- allTillSep c
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser String
templateCommandParser c =
  let chr = char . head . sepChar
  in  between (chr c) (chr c) (allTillSep c)

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser = many . templateStringParser

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(Runnable,String,String)]
parseTemplate c s =
    do str <- case parse (templateParser c) "" s of
                Left _  -> return [("", s, "")]
                Right x -> return x
       let cl = map alias (commands c)
           m  = Map.fromList $ zip cl (commands c)
       return $ combine c m str

-- | Given a finite "Map" and a parsed template produce the resulting
-- output string.
combine :: Config -> Map.Map String Runnable
           -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine c m ((ts,s,ss):xs) = (com, s, ss) : combine c m xs
    where com  = Map.findWithDefault dflt ts m
          dflt = Run $ Com ts [] [] 10

allTillSep :: Config -> Parser String
allTillSep = many . noneOf . sepChar

stripComments :: String -> String
stripComments =
  unlines . map (drop 5 . strip False . (replicate 5 ' '++)) . lines
    where strip m ('-':'-':xs) = if m then "--" ++ strip m xs else ""
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, logging a list of fields that were missing and replaced
-- by the default definition.
parseConfig :: String -> Either ParseError (Config,[String])
parseConfig = runParser parseConf fields "Config" . stripComments
    where
      parseConf = do
        many space
        sepEndSpc ["Config","{"]
        x <- perms
        eof
        s <- getState
        return (x,s)

      perms = permute $ Config
              <$?> pFont <|?> pFontList <|?> pWmClass <|?> pWmName
              <|?> pBgColor <|?> pFgColor
              <|?> pPosition <|?> pTextOffset <|?> pIconOffset <|?> pBorder
              <|?> pBdColor <|?> pBdWidth <|?> pAlpha <|?> pHideOnStart
              <|?> pAllDesktops <|?> pOverrideRedirect <|?> pPickBroadest
              <|?> pLowerOnStart <|?> pPersistent <|?> pIconRoot
              <|?> pCommands <|?> pSepChar <|?> pAlignSep <|?> pTemplate


      fields    = [ "font", "additionalFonts","bgColor", "fgColor"
                  , "wmClass", "wmName", "sepChar"
                  , "alignSep" , "border", "borderColor" ,"template"
                  , "position" , "textOffset", "iconOffset"
                  , "allDesktops", "overrideRedirect", "pickBroadest"
                  , "hideOnStart", "lowerOnStart", "persistent", "iconRoot"
                  , "alpha", "commands"
                  ]

      pFont = strField font "font"
      pFontList = strListField additionalFonts "additionalFonts"
      pWmClass = strField wmClass "wmclass"
      pWmName = strField wmName "windowName"
      pBgColor = strField bgColor "bgColor"
      pFgColor = strField fgColor "fgColor"
      pBdColor = strField borderColor "borderColor"
      pSepChar = strField sepChar "sepChar"
      pAlignSep = strField alignSep "alignSep"
      pTemplate = strField template "template"

      pTextOffset = readField textOffset "textOffset"
      pIconOffset = readField iconOffset "iconOffset"
      pPosition = readField position "position"
      pHideOnStart = readField hideOnStart "hideOnStart"
      pLowerOnStart = readField lowerOnStart "lowerOnStart"
      pPersistent = readField persistent "persistent"
      pBorder = readField border "border"
      pBdWidth = readField borderWidth "borderWidth"
      pAllDesktops = readField allDesktops "allDesktops"
      pOverrideRedirect = readField overrideRedirect "overrideRedirect"
      pPickBroadest = readField pickBroadest "pickBroadest"
      pIconRoot = readField iconRoot "iconRoot"
      pAlpha = readField alpha "alpha"

      pCommands = field commands "commands" readCommands

      staticPos = do string "Static"
                     wrapSkip (string "{")
                     p <- many (noneOf "}")
                     wrapSkip (string "}")
                     string ","
                     return ("Static {"  ++ p  ++ "}")
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd  = wrapSkip (string "]") >> (string "}" <|> notNextRun)
      notNextRun = do {string ","
                      ; notFollowedBy $ wrapSkip $ string "Run"
                      ; return ","
                      }
      readCommands = manyTill anyChar (try commandsEnd) >>=
                        read' commandsErr . flip (++) "]"
      strField e n = field e n strMulti

      strMulti = scan '"'
          where
            scan lead = do
                spaces
                char lead
                s <- manyTill anyChar (rowCont <|> unescQuote)
                (char '"' >> return s) <|> fmap (s ++) (scan '\\')
            rowCont    = try $ char '\\' >> string "\n"
            unescQuote = lookAhead (noneOf "\\") >> lookAhead (string "\"")

      strListField e n = field e n strList
      strList = do
        spaces
        char '['
        list <- sepBy (strMulti >>= \x -> spaces >> return x) (char ',')
        spaces
        char ']'
        return list

      wrapSkip   x = many space >> x >>= \r -> many space >> return r
      sepEndSpc    = mapM_ (wrapSkip . try . string)
      fieldEnd     = many $ space <|> oneOf ",}"
      field  e n c = (,) (e defaultConfig) $
                     updateState (filter (/= n)) >> sepEndSpc [n,"="] >>
                     wrapSkip c >>= \r -> fieldEnd >> return r
      readField a n = field a n $ tillFieldEnd >>= read' n
      read' d s = case reads s of
                    [(x, _)] -> return x
                    _ -> fail $ "error reading the " ++ d ++ " field: " ++ s

commandsErr :: String
commandsErr = "commands: this usually means that a command could not" ++
              "\nbe parsed." ++
              "\nThe error could be located at the begining of the command" ++
              "\nwhich follows the offending one."
