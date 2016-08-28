(ns caveman.core
  (:require [devtools.core :as devtools]

            [infinitelives.pixi.canvas :as c]
            [infinitelives.pixi.events :as e]
            [infinitelives.pixi.resources :as r]
            [infinitelives.pixi.texture :as t]
            [infinitelives.pixi.sprite :as s]
            [infinitelives.pixi.pixelfont :as pf]
            [infinitelives.utils.math :as math]
            [infinitelives.utils.sound :as sound]
            [infinitelives.utils.gamepad :as gp]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.console :refer [log]]
            [cljs.core.match :refer-macros [match]]

            [cljs.core.async :refer [<! chan put! timeout close!]] )
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]
                   [infinitelives.pixi.pixelfont :as pf]
                   ))

(enable-console-print!)
(devtools/install!)

;(def Exception)
;(def backtrack)

(def grass-colour 0x577543)

(defonce canvas
  (c/init {:layers [:bg :level :things :ui]
           :background grass-colour
           :expand true}))

(def spritesheet
  {:fire-place {:pos [48 0] :size [16 16]}
   :caveman {:pos [64 0] :size [16 16]}
   :top-left {:pos [144 16] :size [16 16]}
   :top {:pos [160 16] :size [16 16]}
   :top-right {:pos [176 16] :size [16 16]}
   :left {:pos [144 32] :size [16 16]}
   :center {:pos [160 32] :size [16 16]}
   :right {:pos [176 32] :size [16 16]}
   :bottom-left {:pos [144 48] :size [16 16]}
   :bottom {:pos [160 48] :size [16 16]}
   :bottom-right {:pos [176 48] :size [16 16]}
   })

(def tilesheet
  {:size [16 16]
   :offsets
   {
    :empty [32 0]
    :grass-1-l [0 0]
    :grass-1-r [16 0]
    :grass-2-l [0 16]
    :grass-2-r [16 16]
    :grass-3-l [0 32]
    :grass-3-r [16 32]}})

(defn make-tileset []
  (let [texture (r/get-texture :sprites :nearest)
        {:keys [size offsets]} tilesheet]
    (->> offsets
         (map (fn [[k pos]] [k (t/sub-texture texture pos size)]))
         (into {}))))

(defn make-field []
  (doall
   (vec (for [y (range 50)]
             (vec (for [x (range 50)]
                       (rand-nth [:grass-1-l :grass-1-r :grass-2-l :grass-2-r :grass-3-l])))))))

(defn add-tiles! [batch tileset tilemap]
  ;(log tilemap)
  (doall
   (for [row (range (count tilemap))
         col (range (count (first tilemap)))]
     (let [char (get-in tilemap [row col])]
       ;(log "add" char)
       (.addChild batch
                  (s/make-sprite (tileset char)
                                 :x (* 16 col)
                                 :y (* 16 row)
                                 :xhandle 0
                                 :yhandle 0))))))


(defn make-window [w h & {:keys [handle mousedown] :or {handle :center}}]
  (let [window (js/PIXI.Container.)
        w-1 (dec w)
        h-1 (dec h)
        top-left (t/get-texture :top-left)
        top-right (t/get-texture :top-right)
        bottom-left (t/get-texture :bottom-left)
        bottom-right (t/get-texture :bottom-right)
        left (t/get-texture :left)
        right (t/get-texture :right)
        bottom (t/get-texture :bottom)
        top (t/get-texture :top)
        center (t/get-texture :center)]

    (assert (= (.-width top-left)
               (.-width left)
               (.-width bottom-left)) "left edge tiles are not equal width")
    (assert (= (.-width top)
               (.-width center)
               (.-width bottom)) "center vertical strip of tiles are not equal width")
    (assert (= (.-width top-right)
               (.-width right)
               (.-width bottom-right)) "right edge tiles are not equal width")

    (assert (= (.-height top-left)
               (.-height top)
               (.-height top-right)) "top edge tiles are not equal height")
    (assert (= (.-height left)
               (.-height center)
               (.-height right)) "center horizontal strip of tiles are not equal height")
    (assert (= (.-height bottom-left)
               (.-height bottom)
               (.-height bottom-right)) "bottom edge tiles are not equal height")

    (let [left-width (.-width left)
          center-width (.-width center)
          right-width (.-width right)
          top-height (.-height top)
          center-height (.-height center)
          bottom-height (.-height bottom)
          total-width (+ left-width (* center-width (- w 2)) right-width)
          total-height (+ top-height (* center-height (- h 2)) bottom-height)

          ]

      (doall
       (for [x (range w)
             y (range h)]
         (let [sp (s/make-sprite
                   (match [x y]
                          [0 0] top-left
                          [w-1 0] top-right
                          [0 h-1] bottom-left
                          [w-1 h-1] bottom-right
                          [0 _] left
                          [_ 0] top
                          [w-1 _] right
                          [_ h-1] bottom
                          :else center)
                   :x (match x
                             0 0
                             _ (+ left-width (* center-width (dec x))))

                   :y (match y
                             0 0
                             _ (+ top-height (* center-height (dec y))))
                   :xhandle 0
                   :yhandle 0)]
           (when mousedown
             (set! (.-interactive sp) true)
             (set! (.-mousedown sp) mousedown))
           (.addChild window sp))))
      (when mousedown
        (set! (.-interactiveChildren window) true))
      (s/set-scale! window 4)
      (apply s/set-pivot! window
             (case handle
               :center [(/ total-width 2) (/ total-height 2)]
               :top-right [total-width 0]
               :top-left [0 0]))
      window)))

(defn make-action-window [t & opts]
  (let [window (apply make-window 8 4 opts)
        text (pf/make-text :small "Survive" :scale 4)]
    (.addChild window text)
    window))

(defn survive-or-think []
  (let [c (chan)
        ;; this is a hack to get around a <! inside macro inside go block returning wrong value.
        ;; TODO: investigate this use case
        res (chan)]
    (go
      (m/with-sprite canvas :ui
        [window-left (make-window 8 4 :handle :top-right :mousedown #(put! c :survive))
         window-right (make-window 8 4 :handle :top-left :mousedown #(put! c :think))
         text-left (pf/make-text :small "Survive" :scale 4 :x -250 :y 190 :tint 0x000000)
         text-right (pf/make-text :small "Think" :scale 4 :x 250 :y 190 :tint 0x000000)]
        (>! res (<! c))))
    res))

(defonce main
  (go
    (<! (r/load-resources canvas :ui ["img/sprites.png"
                                      "img/fonts.png"]
                          :full-colour 0xa8c032))

    (pf/pixel-font :small "img/fonts.png" [5 5] [250 60]
                   :chars ["ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           "abcdefghijklmnopqrstuvwxyz"
                           "0123456789!?#`'.,"]
                   :kerning {"fo" -2  "ro" -1 "la" -1 }
                   :space 5)

    (t/load-sprite-sheet! (r/get-texture :sprites :nearest) spritesheet)

    (let [tileset (make-tileset)
          bg (js/PIXI.TilingSprite.
              (:empty tileset)
              2000 2000)
          level (make-field)
          level-batch (js/PIXI.ParticleContainer.)

          start-state {:breeding 0
                       :warmth 0
                       :happiness 0
                       :night-attack 0.1
                       :resiliance 0
                       :speed 0
                       :distance 0
                       :trade 0
                       :population 1
                       :defence 0
                       :health 0

                       :food 1
                       :thoughts 0}
          ]
      (add-tiles! level-batch tileset level)


      (m/with-sprite canvas :bg
        [background bg
         main level-batch
         player (s/make-sprite :caveman :scale 4)
         ]
        (set! (.-interactive main) true)
        (set! (.-mousedown main) )
        (s/set-pos! main -1000 -1000)
        (s/set-scale! main 4)
        (s/set-pos! background -1000 -1000)

        (loop [mode (<! (survive-or-think))
               state start-state]
          (<! (timeout 1000))

          (let [night-attack? (< (rand) (:night-attack state))]
            (when night-attack? (log "you were attacked in the night"))


            (recur
             (<! (survive-or-think))
             state))
          )
        ))))
