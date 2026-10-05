module Alphababy.Content
  ( Group(..)
  , allGroups
  , groupCode
  , groupFromCode
  , groupTitle
  , groupBadge
  , SkillId(..)
  , Face(..)
  , Segment
  , Label
  , Skill
  , CurriculumOptions
  , curriculum
  , letterName
  , spell
  , stickers
  , WritingTask
  , WritingRival
  , writingTask
  , tracePrompt
  , writePrompt
  , traceNudge
  , traceNextStroke
  , writeNudge
  , writingFeedback
  , writingPraise
  , writingGoodTry
  ) where

import Prelude

import Alphababy.Handwriting (Verdict(..))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (find)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))

-- | A family of skills. The adult can enable or disable whole groups.
data Group
  = NameLetters
  | CapitalLetters
  | LittleLetters
  | LetterPairs
  | Digits
  | Counting
  | BigNumbers
  | PictureSounds
  | LetterSounds
  | Syllables
  | SightWords
  | Words
  | Vowels
  | Spellings
  | MagicE

derive instance Eq Group
derive instance Ord Group

allGroups :: Array Group
allGroups =
  [ NameLetters
  , CapitalLetters
  , LittleLetters
  , LetterPairs
  , Digits
  , Counting
  , BigNumbers
  , PictureSounds
  , LetterSounds
  , Syllables
  , SightWords
  , Words
  , Vowels
  , Spellings
  , MagicE
  ]

-- | Stable persisted identifier.
groupCode :: Group -> String
groupCode = case _ of
  NameLetters -> "name"
  CapitalLetters -> "capitals"
  LittleLetters -> "littles"
  LetterPairs -> "pairs"
  Digits -> "digits"
  Counting -> "counting"
  BigNumbers -> "numbers"
  PictureSounds -> "first-sounds"
  LetterSounds -> "letter-sounds"
  Syllables -> "syllables"
  SightWords -> "sight-words"
  Words -> "words"
  Vowels -> "vowels"
  Spellings -> "spellings"
  MagicE -> "magic-e"

groupFromCode :: String -> Maybe Group
groupFromCode code = find ((_ == code) <<< groupCode) allGroups

groupTitle :: Group -> String
groupTitle = case _ of
  NameLetters -> "My name"
  CapitalLetters -> "Big letters"
  LittleLetters -> "Little letters"
  LetterPairs -> "Big & little"
  Digits -> "Numbers 0–9"
  Counting -> "Counting"
  BigNumbers -> "Numbers to 25"
  PictureSounds -> "First sounds"
  LetterSounds -> "Letter sounds"
  Syllables -> "Clapping beats"
  SightWords -> "Sight words"
  Words -> "Reading words"
  Vowels -> "Vowel sounds"
  Spellings -> "Letter teams"
  MagicE -> "Magic e"

groupBadge :: Group -> String
groupBadge = case _ of
  NameLetters -> "💖"
  CapitalLetters -> "ABC"
  LittleLetters -> "abc"
  LetterPairs -> "Aa"
  Digits -> "123"
  Counting -> "⭐"
  BigNumbers -> "25"
  PictureSounds -> "🐍"
  LetterSounds -> "s a t"
  Syllables -> "👏"
  SightWords -> "the"
  Words -> "cat"
  Vowels -> "a e i"
  Spellings -> "sh"
  MagicE -> "🪄"

newtype SkillId = SkillId String

derive instance Eq SkillId
derive instance Ord SkillId

type Segment = { text :: String, emphasis :: Boolean }

-- | What is drawn on a ball. Text is never rotated when drawn, so that
-- | b/d/p/q stay distinguishable.
data Face
  = Glyphs (Array Segment)
  | Picture String
  | Pips Int

-- | `key` identifies a label for equality: two balls with the same key
-- | look and sound the same.
type Label =
  { key :: String
  , face :: Face
  , spoken :: String
  }

type Skill =
  { id :: SkillId
  , group :: Group
  , order :: Number
  , name :: String
  , prompt :: String
  , reminder :: String
  , finale :: String
  , targets :: NonEmptyArray Label
  , confusables :: Array Label
  -- | Keys that must never be used as distractors, because they would also
  -- | be a correct answer (e.g. `k` when looking for the sound in "cat").
  , avoid :: Array String
  , requires :: Array SkillId
  , cheer :: Label -> String
  , explain :: Label -> String
  }

type CurriculumOptions =
  { childName :: String
  , extraSightWords :: Array String
  }

-- | Every skill, in rough teaching order (`order` ascending).
curriculum :: CurriculumOptions -> Array Skill
curriculum options =
  Array.sortWith _.order $ Array.concat
    [ nameSkills options.childName
    , Array.mapWithIndex capitalSkill letters
    , Array.mapWithIndex littleSkill letters
    , Array.mapWithIndex pairSkill letters
    , Array.mapWithIndex digitSkill digitOrder
    , map countingSkill (Array.range 1 6)
    , Array.mapWithIndex bigNumberSkill (Array.range 10 25)
    , Array.mapWithIndex pictureSoundSkill pictureSounds
    , Array.mapWithIndex letterSoundSkill letters
    , map syllableSkill [ 1, 2, 3 ]
    , Array.mapWithIndex sightWordSkill (sightWords <> customSightWords options.extraSightWords)
    , Array.mapWithIndex wordSkill cvcWords
    , Array.mapWithIndex vowelSkill shortVowels
    , Array.mapWithIndex spellingSkill spellings
    , Array.mapWithIndex magicESkill magicEWords
    ]

-- Speech engines read isolated letters inconsistently ("a" as "uh"), so
-- letters are spoken by their written-out names.
letterName :: String -> String
letterName s = case String.toLower s of
  "a" -> "ei"
  "b" -> "bee"
  "c" -> "see"
  "d" -> "dee"
  "e" -> "ee"
  "f" -> "eff"
  "g" -> "jee"
  "h" -> "aitch"
  "i" -> "eye"
  "j" -> "jay"
  "k" -> "kay"
  "l" -> "el"
  "m" -> "em"
  "n" -> "en"
  "o" -> "oh"
  "p" -> "pee"
  "q" -> "cue"
  "r" -> "are"
  "s" -> "ess"
  "t" -> "tee"
  "u" -> "you"
  "v" -> "vee"
  "w" -> "double you"
  "x" -> "ex"
  "y" -> "why"
  "z" -> "zee"
  "-" -> ""
  other -> other

-- | Spells a word out loud: "cat" becomes "see, ay, tee".
spell :: String -> String
spell word =
  String.joinWith ", "
    $ Array.filter (not <<< String.null)
    $ map (letterName <<< CodeUnits.singleton) (CodeUnits.toCharArray word)

stickers :: NonEmptyArray String
stickers = NonEmptyArray.cons' "🦄"
  [ "👑", "🌈", "🦋", "🌸", "💎", "🧚", "🍭", "🎀", "⭐", "🐱", "🌷", "🍓", "🧁", "🐬", "🦩", "🌙", "🎠", "🏰", "💖", "🐞", "🌻", "🍩", "🐰", "🌟", "🍰", "🐝", "🦚", "🪷" ]

-- Labels ---------------------------------------------------------------------

plain :: String -> Array Segment
plain text = [ { text, emphasis: false } ]

glyph :: String -> Label
glyph s = { key: "g:" <> s, face: Glyphs (plain s), spoken: letterName s }

numberLabel :: Int -> Label
numberLabel n = { key: "n:" <> show n, face: Glyphs (plain (show n)), spoken: show n }

pipsLabel :: Int -> Label
pipsLabel n = { key: "pips:" <> show n, face: Pips n, spoken: countWord n <> (if n == 1 then " star" else " stars") }

-- | Words drawn with their vowels picked out in a different color.
vowelWord :: String -> Label
vowelWord w =
  { key: "w:" <> w
  , face: Glyphs (map (\c -> { text: CodeUnits.singleton c, emphasis: isVowel c }) (CodeUnits.toCharArray w))
  , spoken: w
  }

plainWord :: String -> Label
plainWord w = { key: "w:" <> w, face: Glyphs (plain w), spoken: w }

-- | Graphemes are spoken by spelling them.
graphemeLabel :: String -> Label
graphemeLabel g = { key: "g:" <> g, face: Glyphs (plain g), spoken: spell g }

pictureLabel :: Picture -> Label
pictureLabel p = { key: "p:" <> p.name, face: Picture p.emoji, spoken: p.name }

isVowel :: Char -> Boolean
isVowel c = Array.elem c [ 'a', 'e', 'i', 'o', 'u' ]

nea :: forall a. a -> Array a -> NonEmptyArray a
nea = NonEmptyArray.cons'

withArticle :: String -> String
withArticle w = case CodeUnits.charAt 0 w of
  Just c | isVowel c -> "an " <> w
  _ -> "a " <> w

countWord :: Int -> String
countWord n = fromMaybe (show n) (Array.index [ "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten" ] n)

countUpTo :: Int -> String
countUpTo n = String.joinWith ", " (map countWord (Array.range 1 n))

thatsIt :: Label -> String
thatsIt l = "That's " <> l.spoken <> "."

-- Letters --------------------------------------------------------------------

type Letter =
  { lower :: String
  , upper :: String
  , keyword :: String
  , soundHint :: String
  , lookAlikes :: Array String
  , capitalLookAlikes :: Array String
  , soundAlikes :: Array String
  }

-- Ordered roughly as synthetic-phonics programs introduce them: common
-- letters that quickly form words (s a t p i n) first.
letters :: Array Letter
letters =
  [ letter "s" "snake" "the first sound in snake" [ "z", "c", "a" ] [ "Z", "C", "G" ] [ "c" ]
  , letter "a" "apple" "the first sound in apple" [ "o", "e", "d" ] [ "V", "H", "R" ] []
  , letter "t" "tiger" "the first sound in tiger" [ "f", "l", "i" ] [ "I", "L", "Y" ] []
  , letter "p" "princess" "the first sound in princess" [ "q", "b", "d" ] [ "R", "B", "F" ] []
  , letter "i" "igloo" "the first sound in igloo" [ "l", "j", "t" ] [ "L", "T", "J" ] [ "y" ]
  , letter "n" "nose" "the first sound in nose" [ "m", "h", "u" ] [ "M", "Z", "H" ] []
  , letter "m" "moon" "the first sound in moon" [ "n", "w", "u" ] [ "W", "N", "H" ] []
  , letter "d" "dog" "the first sound in dog" [ "b", "p", "q" ] [ "O", "B", "P" ] []
  , letter "g" "goat" "the first sound in goat" [ "q", "y", "p" ] [ "C", "O", "Q" ] []
  , letter "o" "octopus" "the first sound in octopus" [ "c", "a", "e" ] [ "Q", "C", "D" ] []
  , letter "c" "cat" "the first sound in cat" [ "o", "e", "a" ] [ "G", "O", "Q" ] [ "k", "q", "s" ]
  , letter "k" "kite" "the first sound in kite" [ "h", "x", "b" ] [ "X", "R", "Y" ] [ "c", "q" ]
  , letter "e" "egg" "the first sound in egg" [ "c", "o", "a" ] [ "F", "B", "L" ] []
  , letter "u" "umbrella" "the first sound in umbrella" [ "n", "v", "y" ] [ "V", "J", "O" ] []
  , letter "r" "rainbow" "the first sound in rainbow" [ "n", "v", "h" ] [ "P", "B", "K" ] []
  , letter "h" "horse" "the first sound in horse" [ "n", "b", "k" ] [ "N", "A", "K" ] []
  , letter "b" "butterfly" "the first sound in butterfly" [ "d", "p", "h" ] [ "R", "P", "D" ] []
  , letter "f" "fish" "the first sound in fish" [ "t", "l", "j" ] [ "E", "P", "T" ] []
  , letter "l" "lion" "the first sound in lion" [ "i", "t", "j" ] [ "I", "T", "J" ] []
  , letter "j" "jellyfish" "the first sound in jellyfish" [ "i", "g", "y" ] [ "I", "L", "U" ] [ "g" ]
  , letter "v" "violin" "the first sound in violin" [ "w", "y", "u" ] [ "W", "U", "Y" ] []
  , letter "w" "whale" "the first sound in whale" [ "m", "v", "u" ] [ "M", "V", "N" ] []
  , letter "x" "fox" "the last sound in fox" [ "k", "y", "z" ] [ "K", "Y", "Z" ] []
  , letter "y" "yo-yo" "the first sound in yo-yo" [ "v", "g", "j" ] [ "V", "X", "T" ] [ "i" ]
  , letter "z" "zebra" "the first sound in zebra" [ "s", "x", "n" ] [ "S", "N", "X" ] [ "s" ]
  , letter "q" "queen" "the first sound in queen" [ "p", "g", "d" ] [ "O", "G", "P" ] [ "c", "k" ]
  ]
  where
  letter lower keyword soundHint lookAlikes capitalLookAlikes soundAlikes =
    { lower, upper: String.toUpper lower, keyword, soundHint, lookAlikes, capitalLookAlikes, soundAlikes }

letterFact :: Letter -> String
letterFact l
  | l.lower == "x" = "Ex is in fox!"
  | otherwise = String.toUpper (String.take 1 (letterName l.lower)) <> String.drop 1 (letterName l.lower) <> " is for " <> l.keyword <> "!"

letterSkill
  :: { group :: Group, prefix :: String, order :: Number, name :: String, prompt :: String, targets :: NonEmptyArray Label, confusables :: Array Label, requires :: Array SkillId, letter :: Letter }
  -> Skill
letterSkill r =
  { id: SkillId (r.prefix <> r.letter.lower)
  , group: r.group
  , order: r.order
  , name: r.name
  , prompt: r.prompt
  , reminder: "Find the " <> letterName r.letter.lower <> "!"
  , finale: letterFact r.letter
  , targets: r.targets
  , confusables: r.confusables
  , avoid: []
  , requires: r.requires
  , cheer: \l -> l.spoken <> "!"
  , explain: thatsIt
  }

capitalSkill :: Int -> Letter -> Skill
capitalSkill i l = letterSkill
  { group: CapitalLetters
  , prefix: "cap:"
  , order: 0.1 + toNumber i
  , name: l.upper
  , prompt: "Find every letter " <> letterName l.lower <> "!"
  , targets: nea (glyph l.upper) []
  , confusables: map glyph l.capitalLookAlikes
  , requires: []
  , letter: l
  }

littleSkill :: Int -> Letter -> Skill
littleSkill i l = letterSkill
  { group: LittleLetters
  , prefix: "low:"
  , order: 2.0 + toNumber i
  , name: l.lower
  , prompt: "Find every little " <> letterName l.lower <> "!"
  , targets: nea (glyph l.lower) []
  , confusables: map glyph l.lookAlikes
  , requires: [ SkillId ("cap:" <> l.lower) ]
  , letter: l
  }

pairSkill :: Int -> Letter -> Skill
pairSkill i l = letterSkill
  { group: LetterPairs
  , prefix: "pair:"
  , order: 6.0 + 1.5 * toNumber i
  , name: l.upper <> l.lower
  , prompt: "Find every " <> letterName l.lower <> ", big and little!"
  , targets: nea (glyph l.upper) [ glyph l.lower ]
  , confusables: map glyph (l.lookAlikes <> l.capitalLookAlikes)
  , requires: [ SkillId ("cap:" <> l.lower), SkillId ("low:" <> l.lower) ]
  , letter: l
  }

letterSoundSkill :: Int -> Letter -> Skill
letterSoundSkill i l =
  { id: SkillId ("snd:" <> l.lower)
  , group: LetterSounds
  , order: 10.0 + 1.2 * toNumber i
  , name: "/" <> l.lower <> "/ " <> l.keyword
  , prompt: "Find the letter that makes " <> l.soundHint <> ". " <> l.keyword <> "!"
  , reminder: "Which letter makes " <> l.soundHint <> "?"
  , finale: letterName l.lower <> " makes " <> l.soundHint <> "!"
  , targets: nea (glyph l.lower) []
  , confusables: map glyph (Array.filter (not <<< flip Array.elem l.soundAlikes) l.lookAlikes)
  , avoid: map ("g:" <> _) l.soundAlikes
  , requires: [ SkillId ("low:" <> l.lower) ]
  , cheer: \t -> t.spoken <> ", " <> l.keyword <> "!"
  , explain: thatsIt
  }

nameSkills :: String -> Array Skill
nameSkills rawName = do
  let
    name = String.trim rawName
    chars = CodeUnits.toCharArray name
    shown = Array.mapWithIndex (\i c -> if i == 0 then String.toUpper (CodeUnits.singleton c) else String.toLower (CodeUnits.singleton c)) chars
    distinct = Array.nub (Array.filter (\s -> s /= " " && s /= "-") shown)
    inName = Array.concatMap (\c -> [ String.toLower c, String.toUpper c ]) distinct
  case NonEmptyArray.fromArray (map glyph distinct) of
    Nothing -> []
    Just targets ->
      [ { id: SkillId "name"
        , group: NameLetters
        , order: 0.0
        , name
        , prompt: "Find the letters in your name! " <> name <> ": " <> spell (String.toLower name) <> "."
        , reminder: "Find the letters in " <> name <> "!"
        , finale: spell (String.toLower name) <> ". That spells " <> name <> "!"
        , targets
        , confusables: map glyph (Array.filter (\c -> not (Array.elem c inName)) (Array.concatMap (\l -> [ l.lower, l.upper ]) letters))
        , avoid: []
        , requires: []
        , cheer: \l -> l.spoken <> "!"
        , explain: \l -> "That's " <> l.spoken <> ". It's not in " <> name <> "."
        }
      ]

-- Numbers --------------------------------------------------------------------

digitOrder :: Array Int
digitOrder = [ 1, 2, 3, 4, 5, 0, 6, 7, 8, 9 ]

digitSkill :: Int -> Int -> Skill
digitSkill i n =
  { id: SkillId ("dig:" <> show n)
  , group: Digits
  , order: 0.5 + 2.5 * toNumber i
  , name: show n
  , prompt: "Find every number " <> show n <> "!"
  , reminder: "Find the " <> show n <> "!"
  , finale: case n of
      0 -> "Zero means none at all!"
      1 -> "One! Just one!"
      _ -> countUpTo n <> "! " <> countWord n <> "!"
  , targets: nea (numberLabel n) []
  , confusables: map numberLabel (lookAlikes n)
  , avoid: []
  , requires: []
  , cheer: \l -> l.spoken <> "!"
  , explain: thatsIt
  }
  where
  lookAlikes = case _ of
    0 -> [ 8, 6, 9 ]
    1 -> [ 7, 4 ]
    2 -> [ 5, 7 ]
    3 -> [ 8, 5 ]
    4 -> [ 1, 9 ]
    5 -> [ 2, 6, 3 ]
    6 -> [ 9, 0, 5 ]
    7 -> [ 1, 2 ]
    8 -> [ 3, 0, 6 ]
    _ -> [ 6, 4, 0 ]

countingSkill :: Int -> Skill
countingSkill n =
  { id: SkillId ("pips:" <> show n)
  , group: Counting
  , order: 1.5 + 3.0 * toNumber (n - 1)
  , name: show n <> " ⭐"
  , prompt: "Find the balls with " <> (pipsLabel n).spoken <> "! Count them!"
  , reminder: "Find " <> (pipsLabel n).spoken <> "!"
  , finale: (if n == 1 then "" else countUpTo n <> "! ") <> (pipsLabel n).spoken <> "!"
  , targets: nea (pipsLabel n) []
  , confusables: map pipsLabel (Array.filter (\m -> m >= 1 && m <= 6) [ n - 1, n + 1 ])
  , avoid: []
  , requires: if n > 1 then [ SkillId ("pips:" <> show (n - 1)) ] else []
  , cheer: \_ -> (if n == 1 then "one star" else countUpTo n) <> "!"
  , explain: \l -> "That's " <> l.spoken <> "."
  }

bigNumberSkill :: Int -> Int -> Skill
bigNumberSkill i n = do
  let
    tens = n / 10
    ones = n `mod` 10
    reversed = ones * 10 + tens
  { id: SkillId ("num:" <> show n)
  , group: BigNumbers
  , order: 20.0 + 1.5 * toNumber i
  , name: show n
  , prompt: "Find the number " <> show n <> "!"
  , reminder: "Find " <> show n <> "!"
  , finale: show n <> " is a " <> show tens <> " and a " <> show ones <> "!"
  , targets: nea (numberLabel n) []
  , confusables: map numberLabel (Array.nub (Array.filter (\m -> m /= n && m >= 0) [ reversed, n + 1, n - 1, n + 10, n - 10, ones ]))
  , avoid: []
  , requires: map (\d -> SkillId ("dig:" <> show d)) (Array.nub [ tens, ones ])
  , cheer: \l -> l.spoken <> "!"
  , explain: thatsIt
  }

-- Pictures -------------------------------------------------------------------

type Picture = { emoji :: String, name :: String, beats :: Int }

-- `beats` of 0 marks a word whose syllable count is commonly disputed
-- ("flower", "chocolate"), which is kept out of clapping rounds.
pictureSounds :: Array { sound :: String, keyword :: String, pictures :: Array Picture }
pictureSounds =
  [ group "s" [ p "🐍" "snake" 1, p "☀️" "sun" 1, p "🧦" "sock" 1, p "🥪" "sandwich" 2, p "🦢" "swan" 1 ]
  , group "b" [ p "🐝" "bee" 1, p "🍌" "banana" 3, p "🎈" "balloon" 2, p "🐻" "bear" 1, p "🦋" "butterfly" 3, p "🚌" "bus" 1 ]
  , group "m" [ p "🌙" "moon" 1, p "🐒" "monkey" 2, p "🍄" "mushroom" 2, p "🐭" "mouse" 1, p "🧲" "magnet" 2 ]
  , group "p" [ p "🐷" "pig" 1, p "🍕" "pizza" 2, p "🐧" "penguin" 2, p "🍐" "pear" 1, p "👸" "princess" 2, p "🥞" "pancake" 2 ]
  , group "d" [ p "🐶" "dog" 1, p "🦆" "duck" 1, p "🍩" "donut" 2, p "🐬" "dolphin" 2, p "🥁" "drum" 1, p "🦕" "dinosaur" 3 ]
  , group "c" [ p "🐱" "cat" 1, p "🍪" "cookie" 2, p "🥕" "carrot" 2, p "🚗" "car" 1, p "🐄" "cow" 1, p "🪁" "kite" 1, p "🦘" "kangaroo" 3, p "🔑" "key" 1 ]
  , group "t" [ p "🐯" "tiger" 2, p "🍅" "tomato" 3, p "🐢" "turtle" 2, p "🌮" "taco" 2, p "🚂" "train" 1, p "🌳" "tree" 1 ]
  , group "f" [ p "🐟" "fish" 1, p "🐸" "frog" 1, p "🦊" "fox" 1, p "🌸" "flower" 0, p "🦩" "flamingo" 3, p "🍟" "fries" 1 ]
  , group "h" [ p "🐴" "horse" 1, p "🎩" "hat" 1, p "❤️" "heart" 1, p "🏠" "house" 1, p "🦔" "hedgehog" 2, p "🍯" "honey" 2 ]
  , group "r" [ p "🌈" "rainbow" 2, p "🐰" "rabbit" 2, p "🤖" "robot" 2, p "🚀" "rocket" 2, p "🌹" "rose" 1, p "💍" "ring" 1 ]
  , group "l" [ p "🦁" "lion" 2, p "🍋" "lemon" 2, p "🍭" "lollipop" 3, p "🐞" "ladybug" 3, p "🍃" "leaf" 1 ]
  , group "g" [ p "🐐" "goat" 1, p "🍇" "grapes" 1, p "🦍" "gorilla" 3, p "🎁" "gift" 1, p "👻" "ghost" 1 ]
  , group "w" [ p "🐋" "whale" 1, p "🍉" "watermelon" 4, p "🐺" "wolf" 1, p "🪱" "worm" 1 ]
  , group "sh" [ p "🐑" "sheep" 1, p "🦈" "shark" 1, p "🐚" "shell" 1, p "👟" "shoe" 1, p "🚢" "ship" 1 ]
  , group "ch" [ p "🧀" "cheese" 1, p "🍒" "cherry" 2, p "🐔" "chicken" 2, p "🍫" "chocolate" 0, p "🪑" "chair" 1 ]
  , group "a" [ p "🍎" "apple" 2, p "🐜" "ant" 1, p "🐊" "alligator" 4 ]
  , group "e" [ p "🥚" "egg" 1, p "🐘" "elephant" 3 ]
  , group "o" [ p "🐙" "octopus" 3, p "🦦" "otter" 2 ]
  , group "u" [ p "☂️" "umbrella" 3 ]
  ]
  where
  p emoji name beats = { emoji, name, beats }
  group sound pictures =
    { sound
    , keyword: maybe "" _.name (Array.head pictures)
    , pictures
    }

allPictures :: Array Picture
allPictures = Array.concatMap _.pictures pictureSounds

pictureSoundSkill :: Int -> { sound :: String, keyword :: String, pictures :: Array Picture } -> Skill
pictureSoundSkill i ps =
  { id: SkillId ("pic:" <> ps.sound)
  , group: PictureSounds
  , order: 4.0 + 3.0 * toNumber i
  , name: String.joinWith "" (map _.emoji (Array.take 3 ps.pictures))
  , prompt: "Find the things that start like " <> ps.keyword <> "! " <> ps.keyword <> "!"
  , reminder: "What starts like " <> ps.keyword <> "?"
  , finale: "They all start like " <> ps.keyword <> "!"
  , targets: fromMaybe (nea (pictureLabel { emoji: "⭐", name: "star", beats: 1 }) []) (NonEmptyArray.fromArray (map pictureLabel ps.pictures))
  , confusables: []
  , avoid: []
  , requires: []
  , cheer: \l -> l.spoken <> "!"
  , explain: \l -> "That's " <> withArticle l.spoken <> "."
  }

syllableSkill :: Int -> Skill
syllableSkill n = do
  let
    claps = countWord n <> (if n == 1 then " clap" else " claps")
  { id: SkillId ("beat:" <> show n)
  , group: Syllables
  , order: 14.0 + 6.0 * toNumber (n - 1)
  , name: countWord n <> " 👏"
  , prompt: "Find the things with " <> claps <> "! Clap them out!"
  , reminder: "Find the things with " <> claps <> "!"
  , finale: "They all have " <> claps <> "!"
  , targets: fromMaybe (nea (pictureLabel { emoji: "🐝", name: "bee", beats: 1 }) [])
      (NonEmptyArray.fromArray (map pictureLabel (Array.filter ((_ == n) <<< _.beats) allPictures)))
  , confusables: []
  , avoid: []
  , requires: []
  , cheer: \l -> l.spoken <> "! " <> claps <> "!"
  , explain: \l -> do
      let
        beats = maybe 0 _.beats (find ((_ == l.spoken) <<< _.name) allPictures)
      "That's " <> withArticle l.spoken <> "." <>
        if beats > 0 then " " <> countWord beats <> (if beats == 1 then " clap." else " claps.") else ""
  }

-- Words ----------------------------------------------------------------------

sightWords :: Array (Tuple String (Array String))
sightWords =
  [ Tuple "the" [ "they", "then", "she" ]
  , Tuple "a" [ "an", "at", "as" ]
  , Tuple "I" [ "it", "is", "in" ]
  , Tuple "and" [ "an", "ant", "end" ]
  , Tuple "to" [ "too", "two", "go" ]
  , Tuple "is" [ "it", "in", "if" ]
  , Tuple "it" [ "is", "in", "at" ]
  , Tuple "in" [ "is", "it", "on" ]
  , Tuple "you" [ "your", "yes", "out" ]
  , Tuple "he" [ "she", "we", "the" ]
  , Tuple "she" [ "he", "see", "the" ]
  , Tuple "we" [ "me", "he", "was" ]
  , Tuple "me" [ "we", "my", "he" ]
  , Tuple "my" [ "me", "may", "by" ]
  , Tuple "go" [ "no", "do", "so" ]
  , Tuple "see" [ "she", "bee", "sea" ]
  , Tuple "look" [ "book", "like", "took" ]
  , Tuple "can" [ "cat", "man", "an" ]
  , Tuple "like" [ "look", "lick", "bike" ]
  , Tuple "no" [ "on", "go", "so" ]
  , Tuple "yes" [ "yet", "eyes", "you" ]
  , Tuple "up" [ "us", "pup", "cup" ]
  , Tuple "mom" [ "man", "map", "wow" ]
  , Tuple "dad" [ "bad", "had", "bed" ]
  , Tuple "love" [ "live", "move", "dove" ]
  , Tuple "said" [ "sad", "sat", "and" ]
  , Tuple "was" [ "saw", "as", "we" ]
  , Tuple "of" [ "off", "if", "for" ]
  , Tuple "for" [ "of", "from", "four" ]
  , Tuple "are" [ "ear", "arm", "car" ]
  , Tuple "they" [ "the", "then", "hey" ]
  , Tuple "play" [ "pay", "day", "plan" ]
  , Tuple "here" [ "her", "there", "where" ]
  , Tuple "come" [ "came", "home", "some" ]
  , Tuple "big" [ "dig", "bag", "pig" ]
  , Tuple "little" [ "bottle", "kettle", "later" ]
  , Tuple "one" [ "on", "own", "won" ]
  , Tuple "two" [ "to", "too", "top" ]
  , Tuple "red" [ "bed", "rod", "read" ]
  , Tuple "blue" [ "blow", "glue", "clue" ]
  ]

customSightWords :: Array String -> Array (Tuple String (Array String))
customSightWords =
  map (\w -> Tuple w [])
    <<< Array.nub
    <<< Array.filter (\w -> not (String.null w) && not (Array.elem w (map (\(Tuple s _) -> s) sightWords)))
    <<< map String.trim

sightWordSkill :: Int -> Tuple String (Array String) -> Skill
sightWordSkill i (Tuple word lookAlikes) =
  { id: SkillId ("sight:" <> word)
  , group: SightWords
  , order: 18.0 + 1.3 * toNumber i
  , name: word
  , prompt: "Find the word " <> word <> "! " <> word <> "."
  , reminder: "Find the word " <> word <> "!"
  , finale: spell (String.toLower word) <> ". That spells " <> word <> "!"
  , targets: nea (plainWord word) []
  , confusables: map plainWord lookAlikes
  , avoid: []
  , requires: map (\c -> SkillId ("low:" <> c)) (Array.nub (map CodeUnits.singleton (CodeUnits.toCharArray (String.toLower word))))
  , cheer: \l -> l.spoken <> "!"
  , explain: \l -> "That says " <> l.spoken <> "."
  }

cvcWords :: Array String
cvcWords =
  [ "cat"
  , "sat"
  , "pat"
  , "tap"
  , "map"
  , "pan"
  , "can"
  , "man"
  , "hat"
  , "bag"
  , "fan"
  , "pin"
  , "tin"
  , "sit"
  , "pig"
  , "dig"
  , "big"
  , "lip"
  , "wig"
  , "dog"
  , "top"
  , "pot"
  , "hot"
  , "mop"
  , "log"
  , "box"
  , "fox"
  , "bed"
  , "red"
  , "hen"
  , "pen"
  , "ten"
  , "net"
  , "leg"
  , "pet"
  , "sun"
  , "bun"
  , "run"
  , "cup"
  , "pup"
  , "bug"
  , "hug"
  , "mud"
  ]

differsByOne :: String -> String -> Boolean
differsByOne a b = do
  let
    xs = CodeUnits.toCharArray a
    ys = CodeUnits.toCharArray b
  Array.length xs == Array.length ys && Array.length (Array.filter identity (Array.zipWith (/=) xs ys)) == 1

wordSkill :: Int -> String -> Skill
wordSkill i word =
  { id: SkillId ("word:" <> word)
  , group: Words
  , order: 30.0 + 1.2 * toNumber i
  , name: word
  , prompt: "Find the word " <> word <> "! " <> word <> "."
  , reminder: "Find the word " <> word <> "!"
  , finale: spell word <> ". " <> word <> "!"
  , targets: nea (vowelWord word) []
  , confusables: map vowelWord (Array.filter (differsByOne word) cvcWords)
  , avoid: []
  , requires: map (\c -> SkillId ("snd:" <> CodeUnits.singleton c)) (Array.nub (CodeUnits.toCharArray word))
  , cheer: \l -> l.spoken <> "!"
  , explain: \l -> "That says " <> l.spoken <> "."
  }

shortVowels :: Array (Tuple String String)
shortVowels = [ Tuple "a" "cat", Tuple "i" "pig", Tuple "o" "dog", Tuple "e" "bed", Tuple "u" "sun" ]

vowelSkill :: Int -> Tuple String String -> Skill
vowelSkill i (Tuple vowel keyword) =
  { id: SkillId ("vowel:" <> vowel)
  , group: Vowels
  , order: 35.0 + 5.0 * toNumber i
  , name: vowel <> " as in " <> keyword
  , prompt: "Find the words with the same middle sound as " <> keyword <> "!"
  , reminder: "Which words sound like " <> keyword <> " in the middle?"
  , finale: "They all have " <> letterName vowel <> " in the middle, like " <> keyword <> "!"
  , targets: fromMaybe (nea (vowelWord keyword) []) (NonEmptyArray.fromArray (map vowelWord (Array.filter (String.contains (Pattern vowel)) cvcWords)))
  , confusables: []
  , avoid: []
  , requires: [ SkillId ("snd:" <> vowel), SkillId ("word:" <> keyword) ]
  , cheer: \l -> l.spoken <> "!"
  , explain: \l -> "That says " <> l.spoken <> "."
  }

type Spelling =
  { code :: String
  , keyword :: String
  , hint :: String
  , graphemes :: Array String
  , requires :: Array String
  }

spellings :: Array Spelling
spellings =
  [ sp "sh" "ship" "the first sound in ship" [ "sh" ] [ "s", "h" ]
  , sp "ch" "cheese" "the first sound in cheese" [ "ch" ] [ "c", "h" ]
  , sp "th" "thumb" "the first sound in thumb" [ "th" ] [ "t", "h" ]
  , sp "f" "fish" "the first sound in fish" [ "f", "ph" ] [ "f", "p" ]
  , sp "k" "kite" "the first sound in kite" [ "c", "k", "ck" ] [ "c", "k" ]
  , sp "ee" "tree" "the ee sound in tree" [ "ee", "ea" ] [ "e" ]
  , sp "ai" "rain" "the ay sound in rain" [ "ai", "ay", "a-e" ] [ "a" ]
  , sp "qu" "queen" "the first sound in queen" [ "qu" ] [ "q" ]
  , sp "ng" "ring" "the last sound in ring" [ "ng" ] [ "n", "g" ]
  , sp "oa" "boat" "the oh sound in boat" [ "oa", "o-e" ] [ "o" ]
  , sp "igh" "night" "the eye sound in night" [ "igh", "ie", "i-e" ] [ "i" ]
  , sp "oo" "moon" "the oo sound in moon" [ "oo", "ew", "ue" ] [ "o" ]
  , sp "are" "car" "the ar sound in car" [ "are" ] [ "a", "r" ]
  , sp "or" "fork" "the or sound in fork" [ "or", "aw" ] [ "o", "r" ]
  , sp "er" "bird" "the er sound in bird" [ "er", "ir", "ur" ] [ "e", "r" ]
  , sp "ow" "cow" "the ow sound in cow" [ "ow", "ou" ] [ "o", "u" ]
  , sp "oy" "boy" "the oy sound in boy" [ "oy", "oi" ] [ "o", "i" ]
  , sp "w" "whale" "the first sound in whale" [ "w", "wh" ] [ "w", "h" ]
  ]
  where
  sp code keyword hint graphemes requires = { code, keyword, hint, graphemes, requires }

spellingSkill :: Int -> Spelling -> Skill
spellingSkill i s = do
  let
    spelled = map spell s.graphemes
    many = Array.length s.graphemes > 1
  { id: SkillId ("team:" <> s.code)
  , group: Spellings
  , order: 40.0 + 2.5 * toNumber i
  , name: String.joinWith " " s.graphemes
  , prompt: "Find the letters that make " <> s.hint <> ". " <> s.keyword <> "!"
      <> if many then " There's more than one way to write it!" else ""
  , reminder: "Which letters make " <> s.hint <> "?"
  , finale: String.joinWith ", and " spelled <> (if many then " all make " else " makes ") <> s.hint <> "!"
  , targets: fromMaybe (nea (graphemeLabel s.code) []) (NonEmptyArray.fromArray (map graphemeLabel s.graphemes))
  , confusables: []
  , avoid: []
  , requires: map (\r -> SkillId ("snd:" <> r)) s.requires
  , cheer: \l -> l.spoken <> ", like " <> s.keyword <> "!"
  , explain: thatsIt
  }

magicEWords :: Array (Tuple String String)
magicEWords =
  [ Tuple "cap" "cape"
  , Tuple "kit" "kite"
  , Tuple "hop" "hope"
  , Tuple "cub" "cube"
  , Tuple "tap" "tape"
  , Tuple "pin" "pine"
  , Tuple "not" "note"
  , Tuple "tub" "tube"
  , Tuple "can" "cane"
  , Tuple "bit" "bite"
  , Tuple "rob" "robe"
  , Tuple "cut" "cute"
  , Tuple "hat" "hate"
  , Tuple "rid" "ride"
  , Tuple "man" "mane"
  ]

magicLabel :: String -> Label
magicLabel w = do
  let
    chars = CodeUnits.toCharArray w
    lastIx = Array.length chars - 1
  { key: "w:" <> w
  , face: Glyphs (Array.mapWithIndex (\ix c -> { text: CodeUnits.singleton c, emphasis: isVowel c && (ix == lastIx || ix == lastIx - 2) }) chars)
  , spoken: w
  }

magicESkill :: Int -> Tuple String String -> Skill
magicESkill i (Tuple short long) = do
  let
    vowel = String.take 1 (String.drop 1 long)
  { id: SkillId ("magic:" <> long)
  , group: MagicE
  , order: 60.0 + 2.0 * toNumber i
  , name: short <> " → " <> long
  , prompt: "Find the word " <> long <> "! Magic e makes the " <> letterName vowel <> " say its name."
  , reminder: "Find the word " <> long <> "!"
  , finale: short <> " plus magic e makes " <> long <> "!"
  , targets: nea (magicLabel long) []
  , confusables: [ vowelWord short ]
  , avoid: []
  , requires: [ SkillId ("vowel:" <> vowel) ]
  , cheer: \l -> l.spoken <> "!"
  , explain: \l -> "That says " <> l.spoken <> "."
  }

toNumber :: Int -> Number
toNumber = Int.toNumber

-- Writing --------------------------------------------------------------------

-- | Something to trace or write: a single letter or digit. `name` is how
-- | it is spoken ("big pee"), `called` the same with an article.
type WritingTask =
  { glyph :: String
  , name :: String
  , called :: String
  , finale :: String
  , rivals :: Array WritingRival
  }

-- | A look-alike she might write instead.
type WritingRival = { glyph :: String, called :: String }

-- | Capital letters, little letters and digits can be written.
writingTask :: Skill -> Maybe WritingTask
writingTask skill = do
  naming <- case skill.group of
    CapitalLetters -> Just \g -> "big " <> letterName g
    LittleLetters -> Just \g -> "little " <> letterName g
    Digits -> Just \g -> "number " <> g
    _ -> Nothing
  glyphText <- single (NonEmptyArray.head skill.targets)
  let
    called g = if skill.group == Digits then "the " <> naming g else "a " <> naming g
  pure
    { glyph: glyphText
    , name: naming glyphText
    , called: called glyphText
    , finale: skill.finale
    , rivals: map (\g -> { glyph: g, called: called g }) (Array.mapMaybe single skill.confusables)
    }
  where
  single l = case l.face of
    Glyphs [ s ] -> Just s.text
    _ -> Nothing

tracePrompt :: WritingTask -> String
tracePrompt t = "Let's trace " <> t.called <> "! Watch the sparkle, then follow the path with your finger."

-- | `model` is whether a model to copy is shown.
writePrompt :: Boolean -> WritingTask -> String
writePrompt model t =
  "Can you write " <> t.called <> " all by yourself? "
    <> (if model then "Watch me first! " else "")
    <> "Make it big, and tap the star when you're done!"

traceNudge :: String
traceNudge = "Follow the sparkly path with your finger!"

traceNextStroke :: String
traceNextStroke = "Now start at the sparkly dot!"

writeNudge :: WritingTask -> String
writeNudge t = "Draw " <> t.called <> " with your finger! Tap the star when you're done."

-- | What to say about an attempt that didn't work, before trying again.
-- | `traced` is whether she was tracing rather than writing freehand.
writingFeedback :: Boolean -> WritingTask -> Verdict WritingRival -> String
writingFeedback traced t = case _ of
  Neat -> writingPraise t
  OffPath -> "Oops, you went off the path! Let's try again. Stay on the sparkles!"
  Unfinished
    | traced -> "Almost! Let's try again, and trace all of it."
    | otherwise -> "Almost! Your " <> t.name <> " needs all its lines. Let's watch, and try again!"
  Backwards -> "Oh! Your " <> t.name <> " is facing the other way. Let's watch, and try again!"
  LooksLike r -> "Hmm, that looks like " <> r.called <> "! Let's watch how to make " <> t.called <> "."
  Messy -> "Good try! Let's watch how " <> t.called <> " goes, and try again."
  TooSmall -> "Let's make it really big! Try again."

writingPraise :: WritingTask -> String
writingPraise t = "That's a beautiful " <> t.name <> "! " <> t.finale

-- | After the second attempt, whatever happened.
writingGoodTry :: WritingTask -> String
writingGoodTry t = "Good trying! That's how " <> t.called <> " looks. We'll practise it again soon!"
