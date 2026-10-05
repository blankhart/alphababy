# Alphababy

Sparkly reading games for a four-year-old who can't read yet: letters,
sounds, numbers and first words on floating bubbles, balloons, stars and
hearts. Everything is spoken aloud, and the game adapts to what she knows.

## Playing

1. `npm install`, then `npm run dev` and open the printed URL (use the
   "Network" URL to open it on a phone or tablet on the same Wi-Fi).
2. For real full-screen play, add it to the home screen (iPhone/iPad:
   Share → Add to Home Screen; Android: ⋮ → Install app). It works offline
   once installed.
3. Tap ▶ and hand it over.

Grown-up controls are hold-to-activate so a child can't trigger them:
hold ⚙️ (home and between rounds) for settings, hold ✕ (during a round) to
go home. Settings include her name (adds a "letters in my name" game and
personalised praise), voice and speed, which groups and items to practise,
extra sight words, turning tracing and writing rounds off, "mark learned"
to skip ahead, and resetting progress.

## How it works

- Each round speaks a prompt ("Find every letter pee!", "Find the things
  that start like snake!", "Find the word cat!"). Right taps burst into
  sparkles with a chime and praise; wrong taps turn the ball red and grumpy,
  make it wobble and speed off, and say what it actually was. After 12
  seconds without progress the answers glow and the target is shown.
- Some rounds for letters and numbers she has met are writing rounds. A
  sparkle first shows how the letter is written, stroke by stroke. She
  traces it with a finger along a path that lights up gold, starting at a
  pulsing star. After a couple of good traces she writes it freehand on
  handwriting lines, with a small model to copy (dropped once she writes it
  well), and taps the big star when she's done. A poor attempt gets
  specific spoken feedback ("it's facing the other way", "that looks like a
  dee", "you went off the path") and one more try; the second attempt ends
  the round either way.
- Between rounds she gets a sticker and stars, and sees a panel of
  everything she's learning (pink) and has mastered (gold crowns).
- About 230 items in 15 groups: big/little letters and matching pairs,
  digits, counting, numbers to 25, first sounds with pictures, letter
  sounds, clapping syllables, sight words, CVC words, short vowel sounds,
  letter teams (sh, ch, th, ph/f, ai/ay/a-e, …) and magic e.
- At most four new items are in rotation. The next item (in phonics order:
  s a t p i n m d …) is introduced as items are mastered; a perfect first
  round counts as mastered so things she already knows aren't drilled.
  Mastered items come back for review, more often the longer it's been.
  Every fifth round is a sparkle party with nothing to get wrong.
- Progress and settings are saved in the browser (localStorage).

## Development

See `CLAUDE.md`. `npm run validate` formats-checks, tests and builds.
