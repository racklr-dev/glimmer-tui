;; glimmer-tui.terminal — the terminal-protocol surface beyond ncurses:
;; synchronized output (CSI ? 2026), clipboard write (OSC 52), desktop
;; notifications (OSC 99 / 777 / 9) and best-effort capability detection via
;; XTVERSION (CSI > 0 q). These sequences have no terminfo capability and no
;; ncurses entry point, so they are written to fd 1 by hand, exactly like the
;; bracketed-paste and mouse modes in glimmer-tui.curses.
;;
;; Two of the three are safe unconditionally: CSI ? 2026 h/l is an unknown
;; private mode on a terminal that lacks it and is ignored, and an OSC 52
;; clipboard WRITE is ignored by a terminal that does not speak it. Only
;; notifications need a decision — OSC 99 (kitty), OSC 777 (iTerm2/WezTerm) or
;; OSC 9 (title-only fallback) — so that is the one thing detection chooses;
;; sending all three would double-notify a terminal that understands more than
;; one.
;;
;; Everything here is pure and headless-testable except detect! (the probe) and
;; the two emitters (copy!/notify!), which no-op when stdout is not a tty.
(ns glimmer-tui.terminal
  (:require [clojure.string :as str]
            [glimmer-tui.ffi :as c]))

;; --- raw emission ------------------------------------------------------------
;; ESC / BEL / ST as characters, so the sequences below carry no reader escapes.
(def ^:private esc (char 27))          ; ESC
(def ^:private bel (char 7))           ; BEL — the OSC terminator kitty/iTerm2 expect
(def ^:private st  (str esc (char 92))) ; ST (ESC + backslash) — xterm's OSC 52 terminator

(def sync-on  (str esc "[?2026h"))     ; begin synchronized output
(def sync-off (str esc "[?2026l"))     ; end synchronized output

;; --- UTF-8 + base64 ----------------------------------------------------------
(defn- utf8-bytes
  "String -> seq of unsigned byte ints (RFC 3629). jolt strings are code-point
  sequences, so each character encodes directly, with no UTF-16 surrogates."
  [s]
  (mapcat
   (fn [cp]
     (cond
       (< cp 0x80)    [cp]
       (< cp 0x800)   [(bit-or 0xc0 (bit-shift-right cp 6))
                       (bit-or 0x80 (bit-and cp 0x3f))]
       (< cp 0x10000) [(bit-or 0xe0 (bit-shift-right cp 12))
                       (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3f))
                       (bit-or 0x80 (bit-and cp 0x3f))]
       :else          [(bit-or 0xf0 (bit-shift-right cp 18))
                       (bit-or 0x80 (bit-and (bit-shift-right cp 12) 0x3f))
                       (bit-or 0x80 (bit-and (bit-shift-right cp 6) 0x3f))
                       (bit-or 0x80 (bit-and cp 0x3f))]))
   (map int s)))

(def ^:private b64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn- base64-encode
  "Seq of unsigned byte ints -> base64 string with padding."
  [bs]
  (let [bs (vec bs)
        n (count bs)
        full (quot n 3)
        sb (StringBuilder.)]
    (dotimes [i full]
      (let [j (* i 3)
            b0 (nth bs j) b1 (nth bs (inc j)) b2 (nth bs (+ j 2))
            triple (+ (bit-shift-left b0 16) (bit-shift-left b1 8) b2)]
        (.append sb (nth b64-alphabet (bit-shift-right triple 18)))
        (.append sb (nth b64-alphabet (bit-and 63 (bit-shift-right triple 12))))
        (.append sb (nth b64-alphabet (bit-and 63 (bit-shift-right triple 6))))
        (.append sb (nth b64-alphabet (bit-and 63 triple)))))
    (when (= 1 (rem n 3))
      (let [b0 (last bs)
            pair (bit-shift-left b0 8)]
        (.append sb (nth b64-alphabet (bit-shift-right pair 10)))
        (.append sb (nth b64-alphabet (bit-and 63 (bit-shift-right pair 4))))
        (.append sb "==")))
    (when (= 2 (rem n 3))
      (let [b0 (nth bs (- n 2)) b1 (nth bs (dec n))
            triple (+ (bit-shift-left b0 16) (bit-shift-left b1 8))]
        (.append sb (nth b64-alphabet (bit-shift-right triple 18)))
        (.append sb (nth b64-alphabet (bit-and 63 (bit-shift-right triple 12))))
        (.append sb (nth b64-alphabet (bit-and 63 (bit-shift-right triple 6))))
        (.append sb (char 61))))
    (.toString sb)))

(defn emit!
  "Write `s` straight to fd 1, around ncurses rather than through it, sized by
  its UTF-8 byte length — write(2) counts bytes, not characters."
  [s]
  (c/write-fd 1 s (count (utf8-bytes s)))
  nil)

;; --- capability detection ----------------------------------------------------
(defonce ^:private detected (atom nil))

(defn capabilities-for
  "The capabilities a terminal family is assumed to have. Synchronized output and
  OSC 52 write are safe on any terminal (unknown private modes and OSCs are
  ignored), so they are always on; only the notification sequence is per-family."
  [family]
  {:family family
   :sync-output true
   :clipboard true
   :notify (case family
             :kitty :osc99
             (:iterm2 :wezterm) :osc777
             :osc9)})

(defn capabilities
  "The detected terminal capabilities, or the conservative defaults before (or
  without) a probe: {:family :unknown :sync-output true :clipboard true
  :notify :osc9}."
  []
  (or @detected (capabilities-for :unknown)))

(defn parse-probe
  "Codes of an XTVERSION reply (ESC [ > | id ... ESC + backslash, or just the
  printable id) -> a family keyword. The id between the pipe and the next
  non-printable names the terminal."
  [codes]
  (let [pipe (first (keep-indexed (fn [i c] (when (= 124 c) i)) codes))
        id (when pipe
             (->> (subvec codes (inc pipe))
                  (take-while #(<= 32 % 126))
                  (map char)
                  (apply str)
                  str/lower-case))]
    (cond
      (nil? id) :unknown
      (str/starts-with? id "kitty") :kitty
      (str/starts-with? id "iterm2") :iterm2
      (str/starts-with? id "wezterm") :wezterm
      (str/starts-with? id "xterm") :xterm
      (str/starts-with? id "tmux") :tmux
      (str/starts-with? id "ghostty") :ghostty
      (str/starts-with? id "foot") :foot
      :else :unknown)))

(defn- read-code [win timeout-ms]
  (c/wtimeout win timeout-ms)
  (let [ch (c/wgetch win)]
    (when (not= ch c/ERR) ch)))

(defn detect!
  "Best-effort terminal identification over XTVERSION (CSI > 0 q): write the
  query, read the reply for up to `timeout-ms`, and cache the resulting
  capabilities. Never throws — a terminal that does not answer, or answers
  something unknown, yields the conservative defaults. Called once from
  glimmer-tui.curses/start!, after raw mode is on."
  ([win] (detect! win 200))
  ([win timeout-ms]
   (let [caps (try
                (emit! (str esc "[>0q"))
                (let [codes (loop [acc []]
                              (if-let [k (read-code win timeout-ms)]
                                (let [acc (conj acc k)]
                                  (if (and (>= (count acc) 2)
                                           (= 27 (nth acc (- (count acc) 2)))
                                           (= 92 k))
                                    acc
                                    (recur acc)))
                                acc))]
                  (capabilities-for (parse-probe codes)))
                (catch :default _ (capabilities-for :unknown)))]
     (reset! detected caps)
     caps)))

;; --- OSC 52 clipboard --------------------------------------------------------
(defn copy-sequence
  "The OSC 52 sequence that puts `text` on the terminal clipboard, terminated
  with ST (ESC + backslash). The payload is base64, so arbitrary text — newlines
  included — survives."
  [text]
  (str esc "]52;c;" (base64-encode (utf8-bytes (str text))) st))

(defn copy!
  "Copy `text` to the terminal clipboard. No-op when stdout is not a tty."
  [text]
  (when (pos? (c/isatty 1))
    (emit! (copy-sequence text)))
  nil)

;; --- notifications -----------------------------------------------------------
(defn- strip-controls [s]
  (apply str (remove #(or (< (int %) 32) (= (int %) 127)) s)))

(defn notify-sequence
  "The OSC sequence that raises a notification. `mode` is the :notify value from
  capabilities — :osc99 (kitty, title + body), :osc777 (iTerm2/WezTerm, title +
  body) or :osc9 (title only). Title has control characters and semicolons
  stripped (a semicolon would truncate it at the middle field); body has control
  characters stripped."
  [mode title body]
  (let [title (str/replace (strip-controls title) ";" ",")
        body (strip-controls body)]
    (case mode
      :osc99  (str esc "]99;;" title ";" body bel)
      :osc777 (str esc "]777;notify;" title ";" body bel)
      (str esc "]9;" title bel))))

(defn notify!
  "Raise a desktop notification with `title` and optional `body`, using the
  sequence the detected terminal supports. No-op when stdout is not a tty."
  ([title] (notify! title ""))
  ([title body]
   (when (pos? (c/isatty 1))
     (emit! (notify-sequence (get (capabilities) :notify :osc9) title body)))
   nil))
