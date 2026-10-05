// Web Audio synthesis of short sound effects. The notes to play are chosen
// in PureScript; this only schedules oscillators.

export const createImpl = () => {
  const Ctor = window.AudioContext || window.webkitAudioContext;
  if (!Ctor) return null;
  const ctx = new Ctor();
  const master = ctx.createGain();
  master.gain.value = 0.35;
  master.connect(ctx.destination);
  return { ctx, master };
};

export const resume = (audio) => () => {
  if (audio.ctx.state !== "running") void audio.ctx.resume().catch(() => {});
};

export const playImpl = (audio) => (notes) => () => {
  const { ctx, master } = audio;
  if (ctx.state !== "running") return;
  const t0 = ctx.currentTime + 0.01;
  for (const n of notes) {
    const osc = ctx.createOscillator();
    const gain = ctx.createGain();
    osc.type = n.wave;
    osc.frequency.setValueAtTime(n.frequency, t0 + n.start);
    if (n.slideTo > 0) {
      osc.frequency.exponentialRampToValueAtTime(n.slideTo, t0 + n.start + n.duration);
    }
    gain.gain.setValueAtTime(0.0001, t0 + n.start);
    gain.gain.exponentialRampToValueAtTime(n.gain, t0 + n.start + 0.012);
    gain.gain.exponentialRampToValueAtTime(0.0001, t0 + n.start + n.duration);
    osc.connect(gain);
    gain.connect(master);
    osc.start(t0 + n.start);
    osc.stop(t0 + n.start + n.duration + 0.05);
  }
};
