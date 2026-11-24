(ns glimmer-tui.render
  "Paint a laid-out widget snapshot onto a screen.

  Parents paint before their children, so a box's background sits under its
  contents, and every node is clipped to its parent's rect. Clipping matters
  more in a terminal than it does in a GUI: a label that wants 40 columns inside
  a frame that has 20 will happily write over the frame's right border, and
  worse, over whatever is beside it. A subtree with no room left is skipped
  entirely.

  A widget is handed its OWN rect and a screen already clipped to the visible
  part of it, rather than the clipped rect itself. That distinction is what makes
  scrolling work: a row whose top half is above the viewport still draws its
  second line at the right place instead of restarting at the clip boundary.

  Overlays are hoisted out of the main pass and painted last, against the whole
  screen — a dialog is not clipped by whatever box it happens to be declared in.

  Pure with respect to the tree — the only thing it mutates is the screen it was
  handed, which in the tests is an in-memory buffer."
  (:require [glimmer-tui.screen :as scr]
            [glimmer-tui.widget :as w]))

(defn intersect
  "The overlap of two rects, or nil when they do not overlap."
  [a b]
  (when (and a b)
    (let [x (max (:x a) (:x b))
          y (max (:y a) (:y b))
          r (min (+ (:x a) (:w a)) (+ (:x b) (:w b)))
          bt (min (+ (:y a) (:h a)) (+ (:y b) (:h b)))]
      (when (and (> r x) (> bt y))
        {:x x :y y :w (- r x) :h (- bt y)}))))

(defn- paint-node! [screen n vis ctx]
  (when-let [spec (w/spec-for (:tag n))]
    (when-let [paint (:paint spec)]
      (paint (scr/clip screen vis) (:rect n) (:props n) n ctx))))

(defn- overlay? [n] (= :overlay (w/container-kind (:tag n))))

(defn- collect-overlays
  "All overlay nodes in `tree`, declaration order (parents before children, so
  an overlay inside an overlay paints later and sits on top)."
  [n]
  (vec (concat (when (overlay? n) [n])
               (mapcat collect-overlays (:children n)))))

(defn- child-clip
  "The rectangle a node's children are confined to. Normally that is the node's
  own visible rect, but a scroll keeps a column for its scrollbar: content wider
  than the viewport would otherwise paint straight over the bar, which is only
  visible when there is enough content to need one."
  [n vis]
  (if-let [vp (:viewport n)]
    (intersect vis {:x (:x (:rect n)) :y (:y (:rect n)) :w (:w vp) :h (:h vp)})
    vis))

(defn- stacks-vertically?
  "Whether this container lays children out top-to-bottom (their rects are
  y-sorted, so the off-screen head/tail can be skipped)."
  [n]
  (case (w/container-kind (:tag n))
    :box (= :vertical (:orientation (:props n)))
    (:scroll :window :frame) true
    false))

(defn- visible-kids
  "Children of `n` that can intersect `clip`: drop the y-sorted head above the
  clip, stop at the first child below it. Small or horizontal containers return
  all children."
  [n clip]
  (let [kids (:children n)]
    (if (or (nil? clip) (< (count kids) 12) (not (stacks-vertically? n)))
      kids
      (let [top (:y clip)
            bottom (+ (:y clip) (:h clip))]
        (->> kids
             (drop-while (fn [c] (< (+ (:y (:rect c)) (:h (:rect c))) top)))
             (take-while (fn [c] (< (:y (:rect c)) bottom))))))))

(defn- paint!
  "Paint `n` and its children, clipped to `clip`. Overlay subtrees are collected
  rather than painted, and returned so the caller can paint them on top. Records
  the focused node's visible rect in `focus-vis` so the caret can be hidden when
  that widget has scrolled out of view."
  [screen n clip ctx focus-vis]
  (if (overlay? n)
    []
    (if-let [vis (intersect (:rect n) clip)]
      (do (paint-node! screen n vis ctx)
          (when (= (:id n) (:focus-id ctx)) (reset! focus-vis vis))
          (let [inner (child-clip n vis)]
            (vec (mapcat #(paint! screen % inner ctx focus-vis)
                         (visible-kids n inner)))))
      ;; not visible: skip the whole subtree — overlays were collected up front
      [])))

(defn- find-node [n id]
  (first (filter #(= id (:id %)) (w/walk n))))

(defn cursor-position
  "Where the terminal cursor belongs: the focused widget decides, via its spec's
  :cursor fn. nil means hide it, which is what everything except a text entry
  wants — a blinking cursor parked on a button reads as a glitch. `[x y false]`
  means hide it there: a field that has let its keys go elsewhere keeps the
  cursor at its caret, out of sight, rather than wherever painting ended."
  [tree focus-id]
  (when focus-id
    (when-let [n (find-node tree focus-id)]
      (when-let [f (:cursor (w/spec-for (:tag n)))]
        (f (:rect n) (:props n) (:state n))))))

(defn render!
  "Clear `screen`, paint the laid-out `tree` onto it, place the cursor and
  present the frame. `ctx` carries {:focus-id id} so widgets can draw themselves
  focused."
  [screen tree ctx]
  (let [[cols rows] (scr/size screen)
        full {:x 0 :y 0 :w cols :h rows}
        focus-vis (atom nil)]
    (scr/clear! screen)
    ;; Main pass paints only what intersects the screen — an off-screen subtree
    ;; is skipped entirely. Overlays (collected up front, declaration order) then
    ;; paint last against the whole screen, so a later one sits on top.
    (paint! screen tree full ctx focus-vis)
    (doseq [o (collect-overlays tree)]
      (when-let [vis (intersect (:rect o) full)]
        (paint-node! screen o vis ctx)
        (when (= (:id o) (:focus-id ctx)) (reset! focus-vis vis))
        (doseq [c (:children o)]
          (paint! screen c vis ctx focus-vis))))
    ;; The caret is placed through the focused node's visible rect, so it hides
    ;; when that widget has scrolled out of view rather than parking on whatever
    ;; row its raw layout coords name.
    (if-let [[x y show?] (and @focus-vis (cursor-position tree (:focus-id ctx)))]
      (scr/cursor! (scr/clip screen @focus-vis) x y (not (false? show?)))
      ;; no place given: hidden, wherever the screen last put it
      (scr/cursor! screen nil nil false))
    (scr/present! screen)
    nil))
