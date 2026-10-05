// The ink extents of `text` drawn at the origin with the context's current
// font, alignment and baseline. Browsers without the extended metrics fall
// back to the advance width and no vertical correction.
export const inkBoundsImpl = (ctx) => (text) => () => {
  const m = ctx.measureText(text);
  if (m.actualBoundingBoxLeft === undefined) {
    return { left: 0, right: m.width, ascent: 0, descent: 0 };
  }
  return {
    left: m.actualBoundingBoxLeft,
    right: m.actualBoundingBoxRight,
    ascent: m.actualBoundingBoxAscent,
    descent: m.actualBoundingBoxDescent,
  };
};
