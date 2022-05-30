{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Bnf (
  Identifier,
  fromString,
  LanguageString (LanguageString),
  Production (Production, name, rules),
  getProduction,
  addProduction,
) where

import Data.Char (isDigit)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map qualified as Map
import Data.Set.NonEmpty (NESet)

newtype Identifier = Identifier String
  deriving stock (Eq, Ord)

instance Show Identifier where
  show (Identifier s) = s

data Name
  = RawName Identifier
  | PlusName Int Name
  | StarName Int Name
  | OptionName Int Name
  | FunctionName Int Identifier (List.NonEmpty.NonEmpty Identifier)
  deriving stock (Eq)

instance Show Name where
  show (RawName name) = show name
  show (PlusName n name) = "_" <> show n <> "Plus_" <> show name
  show (StarName n name) = "_" <> show n <> "Star_" <> show name
  show (OptionName n name) = "_" <> show n <> "Option_" <> show name
  show (FunctionName n name args) =
    "_" <> show n <> "Function_" <> show name <> show args

instance Ord Name where
  a <= b = show a <= show b

fromString :: String -> Maybe Identifier
fromString str =
  case str of
    (x : _) ->
      if x == '_' || isDigit x
        then Nothing
        else Just $ Identifier str
    _ -> Nothing

newtype LanguageString = LanguageString (List.NonEmpty.NonEmpty Name)
  deriving stock (Eq, Ord, Show)

data Production = Production
  { name :: Name
  , rules :: NESet LanguageString
  }

newtype Grammar = Grammar (Map.Map Name (NESet LanguageString))

getProduction :: Grammar -> Name -> Maybe Production
getProduction (Grammar grammar) name =
  Production name <$> Map.lookup name grammar

addProduction :: Production -> Grammar -> Grammar
addProduction prod@Production{name} wrappedGrammar@(Grammar grammar) =
  case getProduction wrappedGrammar name of
    Just value ->
      let newRules = (rules prod <> rules value)
          innerGrammar :: Map.Map Name (NESet LanguageString)
          innerGrammar = Map.insert name newRules grammar
       in Grammar innerGrammar
    _ ->
      Grammar $ Map.insert name (rules prod) grammar
