(ns glimmer-tui.terminal-test
  "Terminal-protocol surface, headless: OSC 52 clipboard, notification
  sequences, capability detection, synchronized-output markers."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [glimmer-tui.terminal :as term]))

(def esc (char 27))
(def bel (char 7))
(def st  (str esc (char 92)))
(def osc52-header (str esc "]52;c;"))

(deftest copy-sequence-is-osc-52
  (let [s (term/copy-sequence "hi")]
    (is (str/starts-with? s osc52-header))
    (is (= "aGk=" (subs s (count osc52-header) (- (count s) 2))))
    (is (= st (subs s (- (count s) 2)))))
  (testing "non-ascii payloads are UTF-8 encoded before base64"
    (let [s (term/copy-sequence "hé")]
      (is (= "aMOp" (subs s (count osc52-header) (- (count s) 2))))))
  (testing "newlines survive"
    (let [s (term/copy-sequence (str "a" (char 10) "b"))]
      (is (= "YQpi" (subs s (count osc52-header) (- (count s) 2)))))))

(deftest notify-sequence-per-family
  (testing "kitty sends OSC 99 title + body"
    (is (= (str esc "]99;;done;all green" bel)
           (term/notify-sequence :osc99 "done" "all green"))))
  (testing "iTerm2/WezTerm send OSC 777"
    (is (= (str esc "]777;notify;done;all green" bel)
           (term/notify-sequence :osc777 "done" "all green"))))
  (testing "fallback sends OSC 9 title only"
    (is (= (str esc "]9;done" bel)
           (term/notify-sequence :osc9 "done" "all green"))))
  (testing "title control chars and semicolons are sanitized"
    (is (= (str esc "]9;done, ok" bel)
           (term/notify-sequence :osc9 (str "done; ok" (char 10)) "ignored")))))

(deftest parse-probe-identifies-families
  (let [reply (fn [id] (vec (concat [27 91 62 124] (map int id) [27 92])))]
    (is (= :kitty (term/parse-probe (reply "kitty(0.32.0)"))))
    (is (= :iterm2 (term/parse-probe (reply "iTerm2 3.5.0"))))
    (is (= :wezterm (term/parse-probe (reply "WezTerm 20240203-110809"))))
    (is (= :xterm (term/parse-probe (reply "XTerm(390)"))))
    (is (= :tmux (term/parse-probe (reply "tmux 3.4"))))
    (is (= :unknown (term/parse-probe [1 2 3])))))

(deftest capabilities-for-chooses-notification
  (is (= :osc99 (:notify (term/capabilities-for :kitty))))
  (is (= :osc777 (:notify (term/capabilities-for :iterm2))))
  (is (= :osc777 (:notify (term/capabilities-for :wezterm))))
  (is (= :osc9 (:notify (term/capabilities-for :unknown))))
  (is (true? (:sync-output (term/capabilities-for :unknown))))
  (is (true? (:clipboard (term/capabilities-for :unknown)))))

(deftest sync-markers
  (is (= (str esc "[?2026h") term/sync-on))
  (is (= (str esc "[?2026l") term/sync-off)))
