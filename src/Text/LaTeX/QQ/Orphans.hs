{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.LaTeX.QQ.Orphans () where
import Data.Data                     (Data, Typeable)
import Data.Generics.Uniplate.Direct (Biplate (..), Uniplate (..), plate, (|*))
import Data.Generics.Uniplate.Direct ((|-), (||*), (||+))
import Text.LaTeX.Base.Syntax        (LaTeX (..), MathType (..), Measure (..))
import Text.LaTeX.Base.Syntax        (TeXArg (..))

instance Biplate TeXArg LaTeX where
  biplate (FixArg t) = plate FixArg |* t
  biplate (OptArg t) = plate OptArg |* t
  biplate (MOptArg ts) = plate MOptArg ||* ts
  biplate (SymArg t) = plate SymArg |* t
  biplate (MSymArg ts) = plate MSymArg ||* ts
  biplate (ParArg t) = plate ParArg |* t
  biplate (MParArg ts) = plate MParArg ||* ts

instance Uniplate LaTeX where
  uniplate (TeXRaw w) = plate TeXRaw |- w
  uniplate (TeXComm s args) = plate TeXComm |- s ||+ args
  uniplate (TeXCommS s)     = plate TeXCommS |- s
  uniplate (TeXEnv env args lat) = plate TeXEnv |- env ||+ args |* lat
  uniplate (TeXMath typ lat)   = plate TeXMath |- typ |* lat
  uniplate (TeXLineBreak mm b) = plate TeXLineBreak |- mm |- b
  uniplate (TeXBraces l)       = plate TeXBraces |* l
  uniplate (TeXComment t)      = plate TeXComment |- t
  uniplate (TeXSeq l r)        = plate TeXSeq |* l |* r
  uniplate TeXEmpty            = plate TeXEmpty
