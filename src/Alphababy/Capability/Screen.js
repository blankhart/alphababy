export const requestFullscreen = () => {
  const el = document.documentElement;
  if (document.fullscreenElement || !el.requestFullscreen) return;
  void el.requestFullscreen({ navigationUI: "hide" }).catch(() => {});
};

export const exitFullscreen = () => {
  if (document.fullscreenElement && document.exitFullscreen) {
    void document.exitFullscreen().catch(() => {});
  }
};

// The screen must not dim while she is playing. Wake locks are released
// when the page is hidden, so the lock is re-acquired on return.
let wakeLock = null;
let wakeWanted = false;
const acquire = () => {
  if (!wakeWanted || !navigator.wakeLock || document.visibilityState !== "visible") return;
  navigator.wakeLock.request("screen").then(
    (lock) => { wakeLock = lock; },
    () => {},
  );
};
document.addEventListener("visibilitychange", acquire);

export const keepAwake = () => {
  wakeWanted = true;
  acquire();
};

export const allowSleep = () => {
  wakeWanted = false;
  if (wakeLock) void wakeLock.release().catch(() => {});
  wakeLock = null;
};

export const fontsReadyImpl = (fonts) => (onDone) => () => {
  if (!document.fonts || !document.fonts.load) {
    onDone();
    return;
  }
  const timer = setTimeout(onDone, 3000);
  Promise.all(fonts.map((f) => document.fonts.load(f))).then(
    () => { clearTimeout(timer); onDone(); },
    () => { clearTimeout(timer); onDone(); },
  );
};

export const devicePixelRatio = () => window.devicePixelRatio || 1;

export const elementSize = (el) => () => {
  const rect = el.getBoundingClientRect();
  if (rect.width > 0 && rect.height > 0) return { width: rect.width, height: rect.height };
  return { width: window.innerWidth, height: window.innerHeight };
};

export const observeResizeImpl = (el) => (onResize) => () => {
  const fire = () => onResize();
  if (typeof ResizeObserver !== "undefined") {
    const observer = new ResizeObserver(fire);
    observer.observe(el);
    return () => observer.disconnect();
  }
  window.addEventListener("resize", fire);
  return () => window.removeEventListener("resize", fire);
};
