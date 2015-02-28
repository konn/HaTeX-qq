-- | Since 0.0.1.0
module Text.LaTeX.Utils (stripTeX, stripTeXR, stripTeXL) where
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Text.LaTeX.Base.Syntax (LaTeX (..))

-- | Strip whitespaces on both sides of LaTeX.
--
-- Since 0.0.1.0
stripTeX :: LaTeX -> LaTeX
stripTeX = stripTeXL . stripTeXR

-- | Strip whitespaces on the left side of LaTeX.
--
-- Since 0.0.1.0
stripTeXL :: LaTeX -> LaTeX
stripTeXL (TeXRaw str) =
  let str' = T.stripStart str
  in if T.null str'
     then TeXEmpty
     else TeXRaw str'
stripTeXL (TeXSeq l r) =
  case stripTeXL l of
    TeXEmpty -> stripTeXL r
    l' -> l' <> r
stripTeXL l = l

-- | Strip whitespaces on the right side of LaTeX.
--
-- Since 0.0.1.0
stripTeXR :: LaTeX -> LaTeX
stripTeXR (TeXRaw str) =
  let str' = T.stripEnd str
  in if T.null str'
     then TeXEmpty
     else TeXRaw str'
stripTeXR (TeXSeq l r) =
  case stripTeXR r of
    TeXEmpty -> stripTeXR l
    r' -> l <> r'
stripTeXR l = l

