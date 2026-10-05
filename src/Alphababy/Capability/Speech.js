// Thin binding to the Web Speech API. Voice choice and wording live in
// PureScript.

const synth = () => (typeof window !== "undefined" ? window.speechSynthesis : undefined);

export const isAvailable = () => synth() !== undefined;

const toVoice = (v) => ({ name: v.name, lang: v.lang, uri: v.voiceURI, local: v.localService, handle: v });

export const getVoicesImpl = (onVoices) => () => {
  const s = synth();
  if (!s) {
    onVoices([])();
    return () => {};
  }
  const now = s.getVoices();
  if (now.length > 0) {
    onVoices(now.map(toVoice))();
    return () => {};
  }
  // Chrome populates voices asynchronously; Safari sometimes never fires
  // the event, so a timeout reports whatever is available.
  let done = false;
  const finish = () => {
    if (done) return;
    done = true;
    s.removeEventListener("voiceschanged", finish);
    clearTimeout(timer);
    onVoices(s.getVoices().map(toVoice))();
  };
  s.addEventListener("voiceschanged", finish);
  const timer = setTimeout(finish, 1500);
  return () => {
    done = true;
    s.removeEventListener("voiceschanged", finish);
    clearTimeout(timer);
  };
};

export const speakImpl = (request) => (onDone) => () => {
  const s = synth();
  if (!s) {
    onDone();
    return () => {};
  }
  s.cancel();
  // Some engines stay paused after the page was backgrounded.
  s.resume();
  const u = new SpeechSynthesisUtterance(request.text);
  if (request.voice) {
    u.voice = request.voice.handle;
    u.lang = request.voice.lang;
  } else {
    u.lang = "en-US";
  }
  u.rate = request.rate;
  u.pitch = request.pitch;
  let finished = false;
  const finish = () => {
    if (finished) return;
    finished = true;
    clearTimeout(timer);
    onDone();
  };
  u.onend = finish;
  u.onerror = finish;
  // Chrome occasionally never fires `end`; don't let a round hang on it.
  const timer = setTimeout(finish, request.timeoutMs);
  s.speak(u);
  return () => {
    if (!finished) {
      finished = true;
      clearTimeout(timer);
      s.cancel();
    }
  };
};

export const cancel = () => {
  const s = synth();
  if (s) s.cancel();
};

// iOS only allows speech after speech was first started from inside a user
// gesture handler; a silent utterance satisfies that.
export const unlock = () => {
  const s = synth();
  if (!s) return;
  const u = new SpeechSynthesisUtterance(" ");
  u.volume = 0;
  s.speak(u);
};
