# CLAUDE.md

This file provides guidance to Claude Code when working with code in this repository.

## Project

Alphababy is a PureScript PWA of reading/phonics games for a 4-year-old who cannot read yet. Key requirements:

- Instructions and feedback are spoken via text-to-speech (the player can't read). The app opens full-screen.
- Core game: floating/bouncing/spinning balls labelled with letters; the child taps the ones matching a prompt ("find all the P's"). Wrong tap → visual feedback (color change, deformation, speed-up). Right tap → sparkle-burst animation plus spoken praise.
- A progress panel appears between rounds. Target selection adapts to her success: reinforce what she knows, introduce new material starting with the most common letters/sounds.
- Content is configurable (letters, digits, numbers up to 25, digraphs like `ph`/`th`, phonics, sight words), with a default set covering upper/lowercase letters, digits, and English phonics by progression level.

## Architecture

A Halogen app with a canvas playfield. Pure logic is kept separate from browser bindings:

- `Alphababy.Content` — every skill (what to find, prompts, look-alikes, prerequisites) in teaching order. All spoken wording lives here; letters are spoken by written-out names (`letterName`) because TTS reads isolated letters inconsistently.
- `Alphababy.Progress` — mastery tracking, unlocking (at most `learningCap` items being learned), and skill selection.
- `Alphababy.Round` — builds a round `Plan` (theme, balls, prompt) from a skill; difficulty follows mastery. `planActivity` picks the next round: a party, a finding round, or (for capitals, little letters and digits she has met) a `WritingPlan` to trace or write freehand.
- `Alphababy.Strokes` — hand-authored stroke templates (ball-and-stick print, in stroke order) for A–Z, a–z and 0–9 in three-line-paper coordinates. `Alphababy.Handwriting` scores ink against them with oriented nearest-point matching (coverage and precision): in place for tracing, normalized for size and position for freehand, with the mirror image and look-alikes as rivals so it can say "backwards" or "that looks like a b". Thresholds were tuned on synthetically wobbled templates; the tests pin the key cases.
- `Alphababy.World` — pure physics/particles per theme, tap resolution. `Alphababy.Render` draws a `World` on a canvas. Text is never rotated far, so b/d/p/q stay distinct.
- `Alphababy.Random` — pure seeded generator (`Gen` monad); world and plan generation are deterministic given a seed.
- `Alphababy.Settings` / `Alphababy.Settings.Codec` — settings model and the versioned persisted format (bump the version and migrate in `decode` when changing it).
- `Alphababy.Capability.*` — thin FFI: speech synthesis, Web Audio, fullscreen/wake lock/animation frames, localStorage, pointer events. `Alphababy.Chimes` holds the sound-effect note choices.
- `Progress.SkillStats` keeps writing skill (`writing`) apart from recognition (`mastery`); tracing moves on to freehand once `canWrite`.
- `Alphababy.Component.App` (screens and round flow), `.Game` (one finding round: canvas, taps, speech), `.Writing` (one tracing/writing round: demonstration, pointer input, judging, at most one retry; drawn by `Alphababy.Render.Writing`), `.Settings` (grown-up settings), `.Hold` (hold-to-activate button). `Alphababy.UI.*` are stateless HTML helpers.

Web shell: `index.html` → `entry.js` (imports Andika font, `styles.css`, and the `app.js` bundle). `vite.config.js` configures the PWA; `npm run icons` regenerates PNG icons from `public/icons/app-icon.svg`.

## Commands

- `npm run dev` — compile, bundle with purs-backend-es, and serve with vite
- `npm run build` — production build into `dist/`
- `npm test` / `spago test` — run `Test.Main` (pure logic: content integrity, scheduling, round plans, physics, handwriting scoring, codec)
- `npm run format` / `npm run format:check` — purs-tidy
- `npm run validate` — format check, tests, build
- `spago build`, `spago install <pkg>` — spago uses `spago.yaml` (registry package set)
