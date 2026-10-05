module Test.Main (main) where

import Prelude

import Alphababy.Content (Group(..), Skill, SkillId(..))
import Alphababy.Content as Content
import Alphababy.Handwriting (Verdict(..))
import Alphababy.Handwriting as Handwriting
import Alphababy.Progress (Progress)
import Alphababy.Progress as Progress
import Alphababy.Random as Random
import Alphababy.Round (Activity(..), Plan, RoundKind(..), Theme(..), WritingMode(..))
import Alphababy.Round as Round
import Alphababy.Settings as Settings
import Alphababy.Settings.Codec as Codec
import Alphababy.Strokes (Template)
import Alphababy.Strokes as Strokes
import Alphababy.World (TapOutcome(..))
import Alphababy.World as World
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (all, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Test.Assert (assert')

skills :: Array Skill
skills = Content.curriculum { childName: "Mia", extraSightWords: [ "unicorn", "the" ] }

ids :: Array SkillId
ids = map _.id skills

showId :: SkillId -> String
showId (SkillId s) = s

main :: Effect Unit
main = do
  testContent
  testProgress
  testRounds
  testWorld
  testHandwriting
  testWriting
  testCodec
  log "All tests passed."

testContent :: Effect Unit
testContent = do
  assert' "skill ids are unique" (Array.length (Array.nub ids) == Array.length ids)
  assert' "there is plenty of material" (Array.length skills > 200)
  for_ skills \skill -> do
    let
      name = showId skill.id
      targetKeys = map _.key (NonEmptyArray.toArray skill.targets)
    for_ skill.requires \req ->
      assert' (name <> " requires an existing skill " <> showId req) (Array.elem req ids)
    assert' (name <> " has no look-alike that is also a target")
      (all (\l -> not (Array.elem l.key targetKeys)) skill.confusables)
    assert' (name <> " does not avoid its own targets")
      (all (\k -> not (Array.elem k targetKeys)) skill.avoid)
  assert' "every group has skills" (all (\g -> Array.any ((_ == g) <<< _.group) skills) Content.allGroups)
  assert' "custom sight words are added once" (Array.length (Array.filter ((_ == SkillId "sight:the") <<< _.id) skills) == 1 && Array.elem (SkillId "sight:unicorn") ids)
  assert' "no name, no name game" (not (Array.any ((_ == NameLetters) <<< _.group) (Content.curriculum { childName: "", extraSightWords: [] })))
  assert' "letters are spoken by name" (Content.spell "cat" == "see, ei, tee")
  assert' "group codes round-trip" (all (\g -> Content.groupFromCode (Content.groupCode g) == Just g) Content.allGroups)

testProgress :: Effect Unit
testProgress = do
  let
    start = Progress.refreshUnlocks skills Progress.emptyProgress
    unlocked = Array.fromFoldable (Map.keys start.skills)
  assert' "starts with a handful of skills" (Array.length unlocked == Progress.learningCap)
  assert' "starting skills have no prerequisites" (all (Array.null <<< requiresOf) unlocked)
  assert' "starts with the name game" (Array.elem (SkillId "name") unlocked)
  assert' "no new skills while at the learning cap" (Progress.refreshUnlocks skills start == start)
  let
    perfect id = { skill: id, found: 3, mistakes: 0, hinted: false }
    capS = SkillId "cap:s"
    afterPerfect = Progress.recordRound (perfect capS) "🦄" start
  assert' "a perfect first round masters a skill" (Progress.isMastered afterPerfect capS)
  assert' "a hinted round does not master a skill"
    (not (Progress.isMastered (Progress.recordRound ((perfect capS) { hinted = true }) "🦄" start) capS))
  let
    refreshed = Progress.refreshUnlocks skills afterPerfect
  assert' "mastering unlocks something new" (Map.size refreshed.skills == Map.size start.skills + 1)
  assert' "little s unlocks only after big S" (not (Map.member (SkillId "low:s") start.skills))
  for_ (Array.range 1 50) \seed -> do
    let
      chosen = Random.evalGen (Progress.chooseSkill skills refreshed) (Random.mkSeed seed)
    assert' "chooses an unlocked skill" (maybe' (\s -> Map.member s.id refreshed.skills) chosen)
    assert' "does not repeat the last skill" (maybe' (\s -> s.id /= capS) chosen)
  assert' "nothing to choose when nothing is enabled" (not (isJust (Random.evalGen (Progress.chooseSkill [] refreshed) (Random.mkSeed 1))))
  assert' "every fifth round is a party" (map (\n -> Progress.isPartyRound (Progress.emptyProgress { rounds = n })) (Array.range 0 9) == [ false, false, false, false, true, false, false, false, false, true ])
  where
  requiresOf id = Array.concatMap _.requires (Array.filter ((_ == id) <<< _.id) skills)
  maybe' f = case _ of
    Just a -> f a
    Nothing -> false

masteredEverything :: Progress
masteredEverything = Array.foldl (\p g -> Progress.markLearned g skills p) Progress.emptyProgress Content.allGroups

testRounds :: Effect Unit
testRounds = do
  for_ skills \skill -> for_ [ 1, 2, 3 ] \seed -> for_ [ Progress.emptyProgress, masteredEverything ] \progress -> do
    let
      plan = Random.evalGen (Round.planLesson skills progress Nothing skill) (Random.mkSeed (seed * 7919))
      name = showId skill.id
      targetKeys = map _.key (NonEmptyArray.toArray skill.targets)
      targets = Array.filter _.target plan.balls
      others = Array.filter (not <<< _.target) plan.balls
    assert' (name <> ": has targets") (Array.length targets >= 2 || NonEmptyArray.length skill.targets == 1 && Array.length targets >= 1)
    assert' (name <> ": targets are targets") (all (\b -> Array.elem b.label.key targetKeys) targets)
    assert' (name <> ": distractors are wrong answers") (all (\b -> not (Array.elem b.label.key targetKeys) && not (Array.elem b.label.key skill.avoid)) others)
    assert' (name <> ": has distractors") (Array.length others >= 3)
    assert' (name <> ": fits on screen") (Array.length plan.balls <= 14)
    assert' (name <> ": is a lesson") (isLesson plan)
  let
    party = Random.evalGen (Round.planParty (Just Bubbles)) (Random.mkSeed 5)
  assert' "party rounds are all targets" (all _.target party.balls)
  assert' "party avoids the previous theme" (party.theme /= Bubbles)
  where
  isLesson :: Plan -> Boolean
  isLesson p = case p.kind of
    Lesson _ -> true
    Party -> false

testWorld :: Effect Unit
testWorld = do
  let
    size = { width: 400.0, height: 800.0 }
    label key = { key, face: Content.Picture "⭐", spoken: key }
    planned = [ { label: label "a", target: true }, { label: label "b", target: false } ]
  for_ [ Bubbles, Balloons, Stars, Hearts ] \theme -> do
    let
      world = World.create 1.0 size theme planned (Random.mkSeed 42)
      stepped = Array.foldl (\w _ -> World.step (1.0 / 60.0) w) world (Array.range 1 1200)
    assert' "all balls are created" (Array.length world.balls == 2)
    assert' "balls stay within the sides" (all (\b -> b.x >= b.radius - 0.5 && b.x <= size.width - b.radius + 0.5) stepped.balls)
    assert' "balls stay on screen" (all (\b -> b.y >= b.radius - 0.5 && b.y <= size.height - b.radius + 0.5) stepped.balls)
    for_ (Array.find _.target world.balls) \target -> do
      let
        result = World.tap target.x target.y world
      assert' "tapping a target finds it" case result.outcome of
        Found _ -> true
        _ -> false
      assert' "a found target is removed" (World.remainingTargets result.world == 0)
      assert' "a found target bursts into sparkles" (Array.length result.world.particles > 10)
    for_ (Array.find (not <<< _.target) world.balls) \other -> do
      let
        result = World.tap other.x other.y world
      assert' "tapping a distractor is a mistake" case result.outcome of
        Mistake b -> b.anger == 1.0
        _ -> false
      assert' "a mistake keeps the ball" (Array.length result.world.balls == 2)
    assert' "tapping empty space misses" case (World.tap (-500.0) (-500.0) world).outcome of
      Missed -> true
      _ -> false

testCodec :: Effect Unit
testCodec = do
  let
    settings = Settings.defaultSettings
      { childName = "Mia"
      , disabledGroups = Set.fromFoldable [ Syllables, MagicE ]
      , disabledSkills = Set.fromFoldable [ SkillId "cap:q" ]
      , extraSightWords = [ "unicorn" ]
      , voiceUri = Just "voice-1"
      , motionSpeed = 0.6
      , writing = false
      }
    progress = Progress.recordWriting { skill: SkillId "cap:s", score: 0.8, traced: true, firstTry: true } "🦋"
      (Progress.recordRound { skill: SkillId "cap:s", found: 2, mistakes: 1, hinted: false } "🦄" Progress.emptyProgress)
    saved = { settings, progress }
  case Codec.decode (Codec.encode saved) of
    Left _ -> assert' "saved data decodes" false
    Right decoded -> do
      assert' "settings survive a save" (decoded.settings.childName == "Mia" && decoded.settings.disabledGroups == settings.disabledGroups && decoded.settings.disabledSkills == settings.disabledSkills && decoded.settings.voiceUri == settings.voiceUri && decoded.settings.motionSpeed == 0.6 && not decoded.settings.writing)
      assert' "progress survives a save" (decoded.progress == progress)
  let
    v1 = encodeJson
      { version: 1
      , settings:
          { childName: "Mia"
          , voiceUri: (Nothing :: Maybe String)
          , speechRate: 0.8
          , soundEffects: false
          , disabledGroups: [] :: Array String
          , disabledSkills: [] :: Array String
          , extraSightWords: [] :: Array String
          }
      , progress: { skills: Object.empty :: Object Progress.SkillStats, rounds: 3, lastSkill: (Nothing :: Maybe String), stars: 1, stickers: [] :: Array String }
      }
  case Codec.decode v1 of
    Left _ -> assert' "version 1 data decodes" false
    Right decoded ->
      assert' "version 1 settings migrate" (decoded.settings.speechRate == 0.8 && decoded.settings.motionSpeed == Settings.defaultSettings.motionSpeed && decoded.progress.rounds == 3)
  let
    v2 = encodeJson
      { version: 2
      , settings:
          { childName: "Mia"
          , voiceUri: (Nothing :: Maybe String)
          , speechRate: 0.8
          , soundEffects: false
          , motionSpeed: 1.5
          , disabledGroups: [] :: Array String
          , disabledSkills: [] :: Array String
          , extraSightWords: [] :: Array String
          }
      , progress: { skills: Object.singleton "cap:s" { mastery: 0.5, rounds: 2, lastRound: 1 }, rounds: 3, lastSkill: Just "cap:s", stars: 1, stickers: [] :: Array String }
      }
  case Codec.decode v2 of
    Left _ -> assert' "version 2 data decodes" false
    Right decoded -> do
      assert' "version 2 settings migrate" (decoded.settings.motionSpeed == 1.5 && decoded.settings.writing == Settings.defaultSettings.writing)
      assert' "version 2 progress migrates" (Progress.statsFor (SkillId "cap:s") decoded.progress == Just { mastery: 0.5, rounds: 2, lastRound: 1, writing: 0.0 })

-- | The template for a glyph, or an empty one (failing the tests that use it).
templateOf :: String -> Template
templateOf g = case Strokes.template g of
  Just t -> t
  Nothing -> []

moved :: Number -> Number -> Number -> Template -> Template
moved scale dx dy = map (map (\p -> { x: p.x * scale + dx, y: p.y * scale + dy }))

verdictName :: forall a. Verdict a -> String
verdictName = case _ of
  Neat -> "neat"
  OffPath -> "off the path"
  Unfinished -> "unfinished"
  Backwards -> "backwards"
  LooksLike _ -> "a look-alike"
  Messy -> "messy"
  TooSmall -> "too small"

freehand :: String -> Template -> Verdict String
freehand g ink = (Handwriting.judgeFreehand { target: templateOf g, rivals: rivalsOf g } ink).verdict
  where
  rivalsOf glyph = Array.mapMaybe (\r -> map (Tuple r) (Strokes.template r)) (Array.filter (_ /= glyph) [ "b", "d", "p", "q", "O", "Q", "C", "L", "E", "F" ])

testHandwriting :: Effect Unit
testHandwriting = do
  let
    writable = Array.mapMaybe (\s -> map (Tuple s) (Content.writingTask s)) skills
  assert' "capitals, little letters and digits can be written" (Array.length writable == 26 + 26 + 10)
  for_ writable \(Tuple skill task) -> do
    let
      name = showId skill.id
      t = templateOf task.glyph
      rivals = Array.mapMaybe (\r -> map (Tuple r) (Strokes.template r.glyph)) task.rivals
      judged ink = (Handwriting.judgeFreehand { target: t, rivals } ink).verdict
    assert' (name <> " has a template") (Strokes.hasTemplate task.glyph)
    assert' (name <> " look-alikes have templates") (all (Strokes.hasTemplate <<< _.glyph) task.rivals)
    assert' (name <> " written neatly is neat, not " <> verdictName (judged t)) (judged t == Neat)
    assert' (name <> " written anywhere at any size is neat") (judged (moved 2.5 3.0 (-1.0) t) == Neat)
    assert' (name <> " traced exactly is neat") ((Handwriting.judgeTrace t t).verdict == Neat)
    assert' (name <> " traced exactly lights it all up")
      (Handwriting.traceComplete (Array.foldl (\lit s -> Array.foldl (\l p -> Handwriting.lightUp t p p l) lit s) (Handwriting.startTrace t) t))
  for_ [ "b", "d", "p", "q", "J", "S", "N", "Z", "2", "3", "7", "9" ] \g ->
    assert' (g <> " written backwards is backwards, not " <> verdictName (freehand g (Strokes.mirror (templateOf g))))
      (freehand g (Strokes.mirror (templateOf g)) == Backwards)
  for_ [ "A", "O", "X", "8", "o" ] \g ->
    assert' (g <> " mirrored is still neat") (freehand g (Strokes.mirror (templateOf g)) == Neat)
  assert' "b written for d is backwards or a look-alike" (Array.elem (freehand "d" (templateOf "b")) [ Backwards, LooksLike "b" ])
  assert' "an L is not an O" (freehand "O" (templateOf "L") /= Neat)
  assert' "a C for an O looks like a C" (freehand "O" (templateOf "C") == LooksLike "C")
  assert' "a tiny letter is too small" (freehand "A" (moved 0.1 0.0 0.0 (templateOf "A")) == TooSmall)
  for_ (Array.range 1 20) \seed -> do
    let
      scribble = Random.evalGen (traverse (const point) (Array.range 1 40)) (Random.mkSeed seed)
      point = { x: _, y: _ } <$> Random.range 0.0 1.0 <*> Random.range 0.0 1.0
    assert' "a scribble is not an A" (freehand "A" [ scribble ] /= Neat)
  let
    a = templateOf "A"
    firstStroke = Array.take 1 a
    lit = Array.foldl (\l p -> Handwriting.lightUp a p p l) (Handwriting.startTrace a) (Array.concat firstStroke)
  assert' "tracing off to one side is off the path" ((Handwriting.judgeTrace a (moved 1.0 0.6 0.0 a)).verdict == OffPath)
  assert' "tracing one stroke of three is unfinished" ((Handwriting.judgeTrace a firstStroke).verdict == Unfinished)
  assert' "tracing one stroke is not complete" (not (Handwriting.traceComplete lit))
  assert' "after the first stroke comes the second" (Handwriting.nextStroke lit == Just 1)
  assert' "nothing traced is unfinished" ((Handwriting.judgeTrace a []).verdict == Unfinished)
  let
    start = Strokes.trail 0.3 0.0 a
    end = Strokes.trail 0.3 100.0 a
  assert' "a demonstration starts at the first stroke" (start.pen == (Array.head a >>= Array.head) && not start.finished)
  assert' "a demonstration ends with the whole letter" (end.finished && end.drawn == a)

testWriting :: Effect Unit
testWriting = do
  let
    capS = SkillId "cap:s"
    start = Progress.refreshUnlocks skills Progress.emptyProgress
    met = Progress.recordRound { skill: capS, found: 3, mistakes: 1, hinted: false } "🦄" start
    skillS = Array.find ((_ == capS) <<< _.id) skills
    wrote score traced p = Progress.recordWriting { skill: capS, score, traced, firstTry: true } "🦄" p
    afterTracing = wrote 1.0 true (wrote 1.0 true met)
    plan p = skillS >>= Round.planWriting p
  assert' "nothing to write before meeting it" (not (isJust (plan start)))
  assert' "first she traces" (map _.mode (plan met) == Just Tracing)
  assert' "after two good traces she writes freehand" (map _.mode (plan afterTracing) == Just Freehand)
  assert' "writing freehand shows a model at first" (map _.showModel (plan afterTracing) == Just true)
  assert' "writing doesn't change recognition" (map _.mastery (Progress.statsFor capS afterTracing) == map _.mastery (Progress.statsFor capS met))
  assert' "writing counts as a round" (afterTracing.rounds == met.rounds + 2 && afterTracing.stars == met.stars + 4)
  assert' "a poor freehand letter goes back to tracing" (map _.mode (plan (wrote 0.1 false (wrote 0.1 false afterTracing))) == Just Tracing)
  assert' "sight words aren't written" (not (isJust (Array.find ((_ == SkillId "sight:the") <<< _.id) skills >>= Round.planWriting masteredEverything)))
  let
    capitals = Array.filter ((_ == CapitalLetters) <<< _.group) skills
    practised = Array.foldl (\p s -> Progress.recordRound { skill: s.id, found: 3, mistakes: 0, hinted: false } "🦄" p) Progress.emptyProgress capitals
    activities writing = map (\seed -> Random.evalGen (Round.planActivity { writing } capitals practised Nothing) (Random.mkSeed seed)) (Array.range 1 60)
    draws = Array.length <<< Array.filter isDraw
    isDraw = case _ of
      Just (Draw _) -> true
      _ -> false
  assert' "some rounds are writing rounds" (draws (activities true) > 5)
  assert' "no writing rounds when they are turned off" (draws (activities false) == 0)
