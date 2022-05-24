module SafeString where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Rec.Class (whileJust)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Data.String.CodePoints (CodePoint)
import Partial.Unsafe (unsafePartial)
import Undefined (undefined)

newtype SafeString = SafeString String

instance Newtype SafeString String
derive newtype instance Eq SafeString
derive newtype instance Ord SafeString
derive newtype instance Show SafeString
derive newtype instance Semigroup SafeString
derive newtype instance Monoid SafeString

uncons :: String -> Maybe { head :: CodePoint, tail :: SafeString }
uncons = undefined

toCodePointArray :: SafeString -> Array CodePoint
toCodePointArray = undefined

fromCodePointArray :: Array CodePoint -> SafeString
fromCodePointArray = undefined

takeWhile :: (CodePoint -> Boolean) -> SafeString -> SafeString
takeWhile = undefined

splitAt :: Int -> SafeString -> { after :: SafeString, before :: SafeString }
splitAt = undefined

singleton :: CodePoint -> SafeString
singleton = undefined

length :: SafeString -> Int
length = undefined

newtype Pattern = Pattern SafeString

derive instance eqPattern :: Eq Pattern
derive instance ordPattern :: Ord Pattern
derive instance newtypePattern :: Newtype Pattern _

instance showPattern :: Show Pattern where
  show (Pattern s) = "(Pattern " <> show s <> ")"

indexOf :: Pattern -> SafeString -> Maybe Int
indexOf = undefined

stripSuffix :: Pattern -> SafeString -> Maybe SafeString
stripSuffix = undefined

contains :: Pattern -> SafeString -> Boolean
contains = undefined

toUpper :: SafeString -> SafeString
toUpper = undefined

split :: Pattern -> SafeString -> Array SafeString
split = undefined

-- | A newtype used in cases to specify a replacement for a pattern.
newtype Replacement = Replacement SafeString

derive instance eqReplacement :: Eq Replacement
derive instance ordReplacement :: Ord Replacement
derive instance newtypeReplacement :: Newtype Replacement _

instance showReplacement :: Show Replacement where
  show (Replacement s) = "(Replacement " <> show s <> ")"

replace :: Pattern -> Replacement -> SafeString -> SafeString
replace = undefined

-- newtype Offset = Offset { cuOffset :: Int, cpOffset :: Int }
-- startOffset :: Offset
-- endOffset :: SafeString -> Offset
-- nextOffset :: Partial => SafeString -> Offset -> Maybe Offset
-- prevOffset :: Partial => SafeString -> Offset -> Maybe Offset
-- charAfter :: Partial => SafeString -> Offset -> Maybe Char
-- subString :: Partial => SafeString -> Offset -> Offset -> SafeString

-- An offset refers to a position in between characters
--  h e l l o    This is a string consisting of 5 characters
-- ^ ^ ^ ^ ^ ^   These are the 6 offsets, numbered 0 to 5
newtype Offset = Offset { cuOffset :: Int, cpOffset :: Int, str :: SafeString }

unOffset :: Offset -> { cpOffset :: Int, cuOffset :: Int, str :: SafeString }
unOffset (Offset o) = o

instance Eq Offset where
  eq :: Offset -> Offset -> Boolean
  eq (Offset x) (Offset y) = x.cuOffset == y.cuOffset && x.cpOffset == y.cpOffset && x.str == y.str

startOffset :: SafeString -> Offset
startOffset s = Offset { cuOffset: 0, cpOffset: 0, str: s }

endOffset :: SafeString -> Offset
endOffset = undefined

nextOffset :: Offset -> Maybe Offset
nextOffset = undefined

prevOffset :: Offset -> Maybe Offset
prevOffset = undefined

charAfter :: Offset -> Maybe Char
charAfter = undefined

subString :: Partial => Offset -> Offset -> SafeString
subString = undefined

prop_emptyStringOffset = startOffset mempty == endOffset mempty
prop_charAfterEmptyString = charAfter (startOffset mempty) == Nothing
prop_charAfterEndOffset s = charAfter (endOffset s) == Nothing

prop_prevStartOffset s = prevOffset (startOffset s) == Nothing
prop_nextEndOffset s = nextOffset (endOffset s) == Nothing
prop_iterOffset s = applyN (length s) (bindFlipped nextOffset) (Just (startOffset s)) == Just (endOffset s)

prop_emptySubString s = unsafePartial subString (startOffset s) (startOffset s) == mempty
prop_negativeSubString s = unsafePartial subString (endOffset s) (startOffset s) == mempty
prop_fullSubString s = unsafePartial subString (startOffset s) (endOffset s) == s

-- An index refers to a character position
--  h e l l o   This is a string consisting of 5 characters
--  ^ ^ ^ ^ ^   These are the 5 indices, numbered 0 to 4
newtype Index = Index { cuIndex :: Int, cpIndex :: Int, str :: SafeString }

unIndex :: Index -> { cpIndex :: Int, cuIndex :: Int, str :: SafeString }
unIndex (Index o) = o

instance Eq Index where
  eq :: Index -> Index -> Boolean
  eq (Index x) (Index y) = x.cuIndex == y.cuIndex && x.cpIndex == y.cpIndex && x.str == y.str

startIndex :: SafeString -> Maybe Index
startIndex s = undefined

endIndex :: SafeString -> Maybe Index
endIndex = undefined

nextIndex :: Index -> Maybe Index
nextIndex = undefined

prevIndex :: Index -> Maybe Index
prevIndex = undefined

charAt :: Index -> Char
charAt = undefined

prop_emptyStringIndex = startIndex mempty == Nothing && endIndex mempty == Nothing

prop_prevStartIndex s = map prevIndex (startIndex s) == Nothing
prop_nextEndIndex s = map nextIndex (endIndex s) == Nothing
prop_iterIndex s = applyN (length s - 1) (bindFlipped nextIndex) (startIndex s) == endIndex s
prop_chartAtCodomain s = Array.length (chars s) == length s

indices :: SafeString -> Array Index
indices s = applyWhileJust nextIndex (startIndex s)

chars :: SafeString -> Array Char
chars = map charAt <<< indices

applyWhileJust :: forall a. (a -> Maybe a) -> Maybe a -> Array a
applyWhileJust f Nothing = []
applyWhileJust f (Just a) = applyWhileJust f (f a)

applyN 0 f x = x
applyN n f x = f (applyN (n - 1) f x)
