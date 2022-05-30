{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Bnf (
  VariableName,
  fromString,
  LanguageString (Variable, Concatenate),
  SingleProduction (SingleProduction),
  Production (Production, name, rules),
  getProduction,
) where

import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map

newtype VariableName = VariableName {unVariablename :: String}
  deriving stock (Eq, Ord)

instance Show VariableName where
  show (VariableName s) = s

fromString :: String -> Maybe VariableName
fromString str =
  case str of
    (x : _) ->
      if x == '_' || isDigit x
        then Nothing
        else Just $ VariableName str
    _ -> Nothing

data LanguageString where
  Variable :: VariableName -> LanguageString
  Concatenate :: LanguageString -> LanguageString -> LanguageString
  deriving stock (Eq)

data SingleProduction
  = SingleProduction VariableName LanguageString
  deriving stock (Eq)

data Production = Production
  { name :: VariableName
  , rules :: NonEmpty LanguageString
  }
  deriving stock (Eq)

newtype Grammar = Grammar (Map.Map VariableName (NonEmpty LanguageString))

getProduction :: Grammar -> String -> Maybe Production
getProduction (Grammar grammar) name =
  let ruleName = VariableName name
   in Production ruleName <$> Map.lookup ruleName grammar

mergeRules ::
  NonEmpty LanguageString ->
  NonEmpty LanguageString ->
  NonEmpty LanguageString
mergeRules r1 r2 =
  NonEmpty.nub (r1 <> r2)

addProduction :: Production -> Grammar -> Grammar
addProduction Production{} g@(Grammar grammar) =
  case getProduction g (unVariablename name) of
    Just value ->
      let newRules = mergeRules value.rules rules
       in 
        Map.insert name value{rules=newRules}
    _ -> 
        Map.insert name value{rules=newRules}
