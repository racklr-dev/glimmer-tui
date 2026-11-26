(ns glimmer-tui.widgets.basic
  "The leaf widgets and the plain containers: label, button, checkbutton,
  separator, box and window."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.text :as text]
            [glimmer-tui.widget :as w]))

;; --- label -------------------------------------------------------------------
(defn- label-text [props]
  (str (or (:label props) (:text props) "")))

(defn- label-cache
  "The label text, its lines and its widest line, cached in widget state.
  Keyed by object identity on the text: a label's string is stable across
  repaints while its component is not re-rendering, so scrolling costs no
  re-splitting and no re-measuring of the same label."
  [props state]
  (let [t (label-text props)]
    (if (identical? t (:label state))
      state
      {:label t
       :lines (text/lines t)
       :block-width (text/block-width t)})))

(def label-spec
  {:container :none
   :measure (fn [props state]
              (let [c (label-cache props state)]
                {:w (:block-width c) :h (count (:lines c))}))
   :derive-state (fn [props state _] (label-cache props state))
   :paint   (fn [screen rect props n _]
              (let [st (w/style props)
                    align (:align props :start)
                    lines (:lines (label-cache props (:state n)))
                    vis (scr/visible-rect screen)
                    start (if vis (max 0 (- (:y vis) (:y rect))) 0)
                    end (if vis
                          (min (count lines) (- (+ (:y vis) (:h vis)) (:y rect)))
                          (count lines))]
                (doseq [i (range start end)
                        :let [line (nth lines i)]]
                  (scr/put! screen (:x rect) (+ (:y rect) i)
                            (w/text-align line (:w rect) align) st))))})

;; --- button ------------------------------------------------------------------
;; A button is its label inside brackets, which is the convention every terminal
;; UI uses and needs no colour to read. Focus shows as reverse video.
(defn- button-label [props]
  (let [[l r] (:brackets props ["[ " " ]"])]
    (str l (or (:label props) "") r)))

(def button-spec
  {:container :none
   :measure (fn [props _] {:w (text/width (button-label props)) :h 1})
   :paint   (fn [screen rect props n ctx]
              (scr/put! screen (:x rect) (:y rect)
                        (w/text-align (button-label props) (:w rect) (:align props :start))
                        (w/style props (= (:id n) (:focus-id ctx)))))
   :focusable? (fn [_] true)
   :activate (fn [wid] (when-let [f (:on-click (:props @wid))] (f)))})

;; --- checkbutton -------------------------------------------------------------
(defn- check-mark [props]
  (if (:active props)
    (:checked-mark props "[x]")
    (:unchecked-mark props "[ ]")))

(def checkbutton-spec
  {:container :none
   :measure (fn [props _]
              {:w (+ (text/width (check-mark props)) 1 (text/width (str (:label props)))) :h 1})
   :paint (fn [screen rect props n ctx]
            (let [s (str (check-mark props) " " (or (:label props) ""))]
              (scr/put! screen (:x rect) (:y rect) (text/pad s (:w rect))
                        (w/style props (= (:id n) (:focus-id ctx))))))
   :focusable? (fn [_] true)
   ;; The handler owns the state, exactly as in glimmer-gtk: it flips the cell
   ;; the component reads, the component re-renders, and :active comes back down
   ;; as a prop. The widget never toggles itself.
   :activate (fn [wid] (when-let [f (:on-toggled (:props @wid))] (f)))})

;; --- separator ---------------------------------------------------------------
(def separator-spec
  {:container :none
   :measure (fn [_ _] {:w 1 :h 1})
   :paint (fn [screen rect props _ _]
            (let [st (w/style props)
                  vertical? (= :vertical (:orientation props))
                  ch (:char props (if vertical? "│" "─"))]
              (if vertical?
                (doseq [i (range (:h rect))]
                  (scr/put! screen (:x rect) (+ (:y rect) i) ch st))
                (doseq [i (range (:h rect))]
                  (scr/put! screen (:x rect) (+ (:y rect) i)
                            (apply str (repeat (:w rect) ch)) st)))))})

;; --- box ---------------------------------------------------------------------
;; A container can carry an :on-key handler, which is how an application binds a
;; key of its own: a key the focused widget declines bubbles outwards, so a
;; handler on the root box sees everything nothing else wanted. It returns truthy
;; to say it consumed the key.
(defn- container-key [wid event _ctx]
  (when-let [f (:on-key (:props @wid))]
    (boolean (f event))))

(def box-spec
  {:container :box
   :measure (fn [_ _] {:w 0 :h 0})
   :key container-key
   :paint (fn [screen rect props _ _]
            ;; a box only paints its background, so a nested box does not punch a
            ;; hole in what is underneath it
            (when (:bg props)
              (doseq [i (range (:h rect))]
                (scr/fill! screen (:x rect) (+ (:y rect) i) (:w rect) (w/style props)))))})

;; --- window ------------------------------------------------------------------
(def window-spec
  {:container :window
   :measure (fn [_ _] {:w 0 :h 0})
   :key container-key
   :paint (fn [screen rect props _ _]
            (when (:bg props)
              (doseq [i (range (:h rect))]
                (scr/fill! screen (:x rect) (+ (:y rect) i) (:w rect) (w/style props)))))})

(defn install! []
  (w/register-widget! :window window-spec)
  (w/register-widget! :box box-spec)
  (w/register-widget! :label label-spec)
  (w/register-widget! :button button-spec)
  (w/register-widget! :checkbutton checkbutton-spec)
  (w/register-widget! :separator separator-spec)
  nil)
