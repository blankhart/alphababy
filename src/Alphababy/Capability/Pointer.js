export const pointerId = (e) => e.pointerId;

// Every position since the last event, oldest first. Browsers batch fast
// finger movements into one event per frame; the batched positions keep
// quick strokes from turning into straight lines.
export const positions = (e) => {
  const events = typeof e.getCoalescedEvents === "function" ? e.getCoalescedEvents() : [];
  const all = events.length > 0 ? events : [e];
  return all.map((ev) => ({ x: ev.clientX, y: ev.clientY }));
};
