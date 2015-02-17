{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, LambdaCase      #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction       #-}
{-# LANGUAGE OverloadedStrings, StandaloneDeriving, TemplateHaskell #-}
module Text.LaTeX.QQ (hat, hat', mkHaTeXQQ) where
import Text.LaTeX.QQ.Orphans ()

import           Control.Monad                       ((<=<))
import           Data.Data                           (Typeable)
import           Data.Generics.Uniplate.Direct       (transform)
import qualified Data.Text                           as T
import           Language.Haskell.AntiQuoter         (AntiQuoterPass, (<>>))
import           Language.Haskell.Meta.Parse.Careful (parseExp, parsePat)
import           Language.Haskell.TH                 (Exp, ExpQ, Pat, PatQ)
import           Language.Haskell.TH                 (stringL)
import           Language.Haskell.TH                 (litE, litP, sigE)
import           Language.Haskell.TH                 (appE, viewP)
import           Language.Haskell.TH.Quote           (QuasiQuoter (..))
import           Language.Haskell.TH.Quote           (dataToExpQ, dataToPatQ)
import           Text.LaTeX                          (texy)
import           Text.LaTeX.Base.Parser              (parseLaTeX)
import           Text.LaTeX.Base.Pretty              (prettyLaTeX)
import           Text.LaTeX.Base.Syntax              (LaTeX (..), TeXArg (..))

-- | HaTeX quasiquoter. antiquote should be of the form of @\hask{...}@.
-- This quasiquoter is whitespace-sensitive both in input and output.
-- You need @OverloadedStrings@ language extension for pattern quotes.
hat :: QuasiQuoter
hat = mkHaTeXQQ "hask" False

-- | Whitespace-insensitive version of 'hat'.
-- Pattern quote requires @ViewPatterns@ in addition.
hat' :: QuasiQuoter
hat' = mkHaTeXQQ "hask" True

-- | General macro to generate quasiquoter for HaTeX.
-- You need @OverloadedStrings@ for pattern quotes,
-- and if the second argument is @True@, you also need @ViewPatterns@.
--
-- NOTE: Due to TH's stage restriction, you have to use this function
-- in an other module than you call the resulting quasiquotes.
mkHaTeXQQ :: String -- ^ Name for antiquoting latex command.
          -> Bool   -- ^ Ignore whitespaces?
          -> QuasiQuoter
mkHaTeXQQ cmd triming =
  let trimer | triming   = trim
             | otherwise = id
      texTrimer | triming   = viewP [| trimTeX |]
                | otherwise = id
  in QuasiQuoter { quoteType = const $ error "Type quoter not defined"
                 , quoteDec  = const $ error "Dec quoter not defined"
                 , quoteExp  = dataToExpQ (antiE cmd) <=< texExp . trimer
                 , quotePat  = texTrimer . dataToPatQ (antiP cmd) <=< texExp . trimer
                 }


trim :: String -> String
trim = T.unpack . T.strip . T.pack

texExp :: Monad m => String -> m LaTeX
texExp src = case parseLaTeX (T.pack src) of
  Right t -> return t
  Left err  -> error $ "malformed latex: " ++ show err

antiE :: Typeable e => String -> AntiQuoterPass e Exp
antiE cmd = antiTextE <>> antiE0 cmd <>> const Nothing

antiE0 :: String -> LaTeX -> Maybe ExpQ
antiE0 cmd (TeXComm s [FixArg src]) | cmd == s =
  case parseExp $ prettyLaTeX src of
    Right e -> Just [| texy $(return e) |]
    Left  e -> error $ "haskell parsing error " ++ show e
antiE0 _ _ = Nothing

antiP :: Typeable e => String -> AntiQuoterPass e Pat
antiP cmd = antiTextP <>> antiP0 cmd <>> const Nothing

antiTextE :: T.Text -> Maybe ExpQ
antiTextE = Just . flip sigE [t| T.Text |] . appE [| T.pack |] .
            litE . stringL . T.unpack

antiTextP :: T.Text -> Maybe PatQ
antiTextP = Just . litP . stringL . T.unpack

trimTeX :: LaTeX -> LaTeX
trimTeX = transform trimTeX0

trimTeX0 :: LaTeX -> LaTeX
trimTeX0 (TeXComment _) = TeXEmpty
trimTeX0 (TeXRaw t) =
  let t' = T.strip t
  in if T.null t'
     then TeXEmpty
     else TeXRaw t'
trimTeX0 TeXEmpty = TeXEmpty
trimTeX0 (TeXSeq TeXEmpty r) = r
trimTeX0 (TeXSeq l TeXEmpty) = l
trimTeX0 t = t

antiP0 :: String -> LaTeX -> Maybe PatQ
antiP0 cmd (TeXComm name [FixArg src]) | cmd == name =
  case parsePat $ prettyLaTeX src of
    Right p -> Just $ return p
    Left  e -> error $ "haskell parsing error " ++ show e
antiP0 _ _ = Nothing

