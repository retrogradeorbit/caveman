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
                   [caveman.async :refer [go-while continue-while]]
                   ))

(enable-console-print!)
(devtools/install!)

;(def Exception)

;(def backtrack)


(def grass-colour 0x577543)

(defonce canvas
  (c/init {:layers [:bg :level :things :stats :inventions :ui]
           :background grass-colour
           :expand true
           :origins {:stats :top-left
                     :inventions :top-right}}))

(def spritesheet
  {:fire-place {:pos [48 0] :size [16 16]}
   :fire {:pos [48 64] :size [16 16]}
   :caveman {:pos [64 0] :size [16 16]}
   :caveman-2 {:pos [80 0] :size [16 16]}
   :caveman-3 {:pos [96 0] :size [16 16]}
   :caveman-4 {:pos [112 0] :size [16 16]}
   :caveman-5 {:pos [128 0] :size [16 16]}
   :caveman-6 {:pos [144 0] :size [16 16]}
   :shelter {:pos [64 16] :size [16 16]}
   :food {:pos [48 48] :size [16 16]}
   :heart {:pos [96 16] :size [16 16]}
   :wheel {:pos [80 16] :size [16 16]}
   :flame {:pos [80 32] :size [16 16]}
   :club {:pos [48 32] :size [16 16]}
   :pets {:pos [48 16] :size [16 16]}
   :religion {:pos [112 16] :size [16 16]}
   :health {:pos [129 16] :size [16 16]}
   :grave {:pos [80 48] :size [16 16]}

   :top-left {:pos [144 16] :size [16 16]}
   :top {:pos [160 16] :size [16 16]}
   :top-right {:pos [176 16] :size [16 16]}
   :left {:pos [144 32] :size [16 16]}
   :center {:pos [160 32] :size [16 16]}
   :right {:pos [176 32] :size [16 16]}
   :bottom-left {:pos [144 48] :size [16 16]}
   :bottom {:pos [160 48] :size [16 16]}
   :bottom-right {:pos [176 48] :size [16 16]}})

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

(defn night-summary [text]
  (go
    (let [c (chan)]
      (m/with-sprite canvas :ui
        [window (make-window 12 2 :mousedown #(close! c))
         text-sprite (pf/make-text :small text :scale 4 :tint 0x000000 :y -10)]
        (<! c)))))

(defn pop-up [markup]
  (go
    (let [c (chan)]
      (m/with-sprite canvas :ui
        [window (make-window 14 6 :mousedown #(close! c))
                                        ;text-sprite (pf/make-text :small text :scale 4 :tint 0x000000 :y -10)
         ]
        (s/set-pos! window 0 200)
        (m/with-sprite-set canvas :ui
          [lines
           (->
            (let [[name & args] markup]
              (case name
                :lines
                (for [[lnum [lname & largs]] (map vector (range) args)]
                  (case lname
                    :line
                    (for [[cname & [text]] largs]
                      (pf/make-text :small text :scale 4
                                    :tint (case cname
                                            :white 0xffffff
                                            :yellow 0xffff00
                                            :red 0xff0000
                                            :orange 0xff8000
                                            :purple 0xff00ff
                                            :black 0x000000
                                            :brown 0x804000
                                            :green 0x008000
                                            :blue 0x000080)
                                    :x 0 :y (+ 200 (* -1 12 4) (* 12 4 lnum))
                                    )
                      )
                    )
                  )
                ))

            flatten)]
          (<! c))))))


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
        [window-left (make-window 8 2 :handle :top-right :mousedown #(put! c :survive))
         window-right (make-window 8 2 :handle :top-left :mousedown #(put! c :think))
         text-left (pf/make-text :small "Survive" :scale 4 :x -250 :y 152 :tint 0x000000)
         text-right (pf/make-text :small "Think" :scale 4 :x 250 :y 152 :tint 0x000000)]
        (s/set-pos! window-left 0 100)
        (s/set-pos! window-right 0 100)
        (>! res (<! c))))
    res))

(defn add-shelter! [batch x y]
  (let [s (s/make-sprite :shelter :x x :y y :scale 4)]
    (.addChild batch s)
    s))

(defn add-fire! [batch x y]
  (let [s (s/make-sprite :fire-place :x x :y y :scale 4)]
    (.addChild batch s)
    s)
)

(def fragment-shader-glsl

  "
precision mediump float;
varying vec4 vColor;
varying vec2 vTextureCoord;
uniform sampler2D u_texture; //diffuse map
uniform sampler2D u_lightmap;   //light map
uniform vec2 resolution; //resolution of screen
uniform vec4 ambientColor; //ambient RGB, alpha channel is intensity
uniform float alpha;
uniform float fire;
void main() {
    // the lavers base piel colour
    vec4 diffuseColor = texture2D(u_texture, vTextureCoord);

    // the light at this point's colour
    vec2 lighCoord = (gl_FragCoord.xy / resolution.xy);
    vec4 light = texture2D(u_lightmap, vTextureCoord);
    float intensity = light.r * fire;
    vec3 factor=vec3(intensity, intensity, intensity);
    vec3 avec = vec3(alpha, alpha, alpha);
    vec3 inv_avec = vec3(1.0-alpha, 1.0-alpha, 1.0-alpha);

    vec3 finalColor = diffuseColor.rgb * factor * vec3(alpha, alpha, alpha)
                    + diffuseColor.rgb * inv_avec;
    gl_FragColor = vec4(finalColor, diffuseColor.a); // + vec4(finalColor, diffuseColor.a * alpha);
}
"

)

(defn change-text! [batch font-key text]
  (let [font (pf/get-font font-key)]
    (loop [[c & l] (seq text)
           xp 0 yp 0
           last-c nil]
      (let [char ((:font font) c)
            {:keys [texture pos size]} char
            [x y] pos
            [w h] size
            pair (str last-c c)
            koff ((:kerning font) pair)
            ]
        (if (nil? char)
          ;; if character is not present in font map, put a space
          (when (seq l)
            (recur l (+ xp (:space font)) yp c))

          (do
            ;character is present, add the sprite to the container
            (.addChild batch (s/make-sprite texture :x (+ xp koff) :y yp :xhandle 0 :yhandle 0))
            (if (seq l)
              (recur l (+ xp w 1.0 koff) yp c)
              (s/set-pivot! batch (/ (+ xp w koff) 2.0) 0))))))))

(defn lightmap-filter [lightmap ambient resolution alpha fire]
  (js/PIXI.AbstractFilter.
   nil
   ;#js [vertex-glsl]
   #js [fragment-shader-glsl]
   #js {
        "u_lightmap" #js {"type" "sampler2D" "value" lightmap}
        "alpha" #js {"type" "1f" "value" alpha}
        "fire" #js {"type" "1f" "value" fire}
        "resolution" #js {"type" "2f" "value" (js/Float32Array. resolution)}
        "ambientColor" #js {"type" "4f" "value" (js/Float32Array. ambient)}}))

(defn found-food [markup]
  (pop-up
   [:lines
    [:line
     [:white "Food!"]]
    [:line
     [:black "You went searching at night and found"]]
    [:line
     markup]
    ]))

(defn night-attack [markup]
  (pop-up
   [:lines
    [:line
     [:white "Night Attack!"]]
    [:line
     [:black "You were attacked in the night by"]]
    [:line
     markup]
    ]))

(defn night-attack-eaten [markup]
  (pop-up
   [:lines
    [:line
     [:white "Hunted!"]]
    [:line
     [:black "Last night you hunted"]]
    [:line
     markup]
    ]))

(defn invention [markup result]
  (pop-up
   [:lines
    [:line
     [:white "New Invention!"]]
    [:line
     [:black "Last night you invented"]]
    [:line
     markup]
    [:line
     [:black result]]]))

(defn no-invention []
  (let [[title text]
        (rand-nth [
                   ["Nothing!" "I really tried, but I came up with nothing"]
                   ["My Head Hurts!" "Thinking is hard"]
                   ["Fell Asleep" "I tried to think, but I got tired"]])]

    (pop-up
     [:lines
      [:line
       [:white title]]
      [:line
       [:black text]]])))

(defn hungry []
  (let [[title text]
        (rand-nth [
                   ["Hungry!" "My stomach is grumbling"]
                   ["Food!" "As in, we need to find some."]])]

    (pop-up
     [:lines
      [:line
       [:white title]]
      [:line
       [:black text]]])))

(defn set-pos [t u g h]
  (s/set-pos! u -160 (+ (* 5 (Math/sin (/ t 10))) -300))
  (s/set-scale! u 4.2)
  ;(s/set-rotation! u (+ (/ (rand) 10) 0.1))
  (s/set-pos! g 0 (+ (* 5 (Math/sin (+ 1.04 (/ t 10)))) -300))
  (s/set-scale! g 3.4)
  ;(s/set-rotation! g (+ (/ (rand) 10) -0.05))
  (s/set-pos! h 140 (+ (* 5 (Math/sin (+ 2.08 (/ t 10)))) -320))
  (s/set-scale! h 3.6)
  ;(s/set-rotation! h (+ (/ (rand) 10) -0.2))
)

(defn titlescreen []
  (let [clicked? (atom false)
        click-fn (fn [ev] (reset! clicked? true))
        ]
    (go
      (m/with-sprite canvas :ui
        [ug (s/make-sprite (r/get-texture :u :nearest) :scale 4 :x -180 :y -300 ;:mousedown click-fn
                           )
         g (s/make-sprite (r/get-texture :g :nearest) :scale 4 :x 0 :y -300)
         h (s/make-sprite (r/get-texture :h :nearest) :scale 4 :x 200 :y -300)

         ]

        (set! (.-interactive ug) true)
        (set! (.-buttonMode ug) false)
        (set! (.-mousedown ug) click-fn)
        (set! (.-interactive g) true)
        (set! (.-buttonMode g) false)
        (set! (.-mousedown g) click-fn)
        (set! (.-interactive h) true)
        (set! (.-buttonMode h) false)
        (set! (.-mousedown h) click-fn)

        (loop [t 0]
          (set-pos t ug g h)
          (<! (e/next-frame))
          (when (not @clicked?)
            (recur (inc t))))
        )))

  )

(defn do-night-attack [state mode]
  (go
    (if (and (:club @state) (< (rand) 0.5))
      ;; eaten via club
      (let [sub-to-zero (fn [val take]
                          (max 0 (- val (if (= :survive mode)
                                          take
                                          (* 1.2 take)))))
            {:keys [damage]} @state
            [color name func arg]
            (rand-nth
             [
              [:blue "NIGHTWOLF" sub-to-zero (* damage 15)]
              [:blue "BEAR" sub-to-zero (* damage 15)]
              [:red "BLOODBAT" sub-to-zero (* damage 10)]
              [:green "LEACHES" sub-to-zero (* damage 5)]])
            ]
        (<!
         (night-attack-eaten [color name]))
        (swap! state update-in [:food] + 20))

      ;; attacked!
      (let [sub-to-zero (fn [val take]
                          (max 0 (- val (if (= :survive mode)
                                          take
                                          (* 1.2 take)))))
            {:keys [damage]} @state
            [color name func arg]
            (rand-nth
             [
              [:blue "NIGHTWOLF" sub-to-zero (* damage 20)]
              [:blue "BEAR" sub-to-zero (* damage 20)]
              [:red "BLOODBAT" sub-to-zero (* damage 15)]
              [:green "LEACHES" sub-to-zero (* damage 10)]])
            ]
        (<!
         (night-attack [color name]))
        (swap! state update-in [:life] func arg)))))

(defonce main
  (go
    (<! (r/load-resources canvas :ui ["img/sprites.png"
                                      "img/fonts.png"
                                      "img/light.png"
                                      "img/u.png"
                                      "img/g.png"
                                      "img/h.png"
                                      "sfx/blop.ogg"]
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
          level-batch (js/PIXI.Container.) ;; ParticleContainer cant have filters applies

          state (atom {
                       :fire false
                       :night-attack 0.15
                       :population 1
                       :defence 0

                       :food 10
                       :life 100
                       :love 0
                       :thoughts 0})
          ]
      (add-tiles! level-batch tileset level)

      (let [player (s/make-sprite :caveman :scale 4)
            player-batch (js/PIXI.Container.)

            dark-filter-level (lightmap-filter
                               (r/get-texture :light :nearest)
                               #js  [0 0 0 0.5]
                               #js  [1.0 1.0]
                               0.0 0.0)

            dark-filter-sprites (lightmap-filter
                                 (r/get-texture :light :nearest)
                                 #js  [0 0 0 0.5]
                                 #js  [1.0 1.0]
                                 0.0 0.0)

            sunset (fn [fire-state]
                     (go
                       (loop [f 0]
                         (set! (.-uniforms.alpha.value dark-filter-level)
                               (Math/sin (/ f 60)))
                         (set! (.-uniforms.fire.value dark-filter-level)
                               (if fire-state
                                 (Math/sin (/ f 60))
                                 0))
                         (set! (.-uniforms.alpha.value dark-filter-sprites)
                               (if fire-state 0 (Math/sin (/ f 60))))

                         (<! (e/next-frame))
                         (when (< f 60)
                           (recur (inc f))))))

            sunrise (fn [fire-state]
                      (go
                        (loop [f 60]
                          (set! (.-uniforms.alpha.value dark-filter-level)
                                (Math/sin (/ f 60)))
                          (set! (.-uniforms.fire.value dark-filter-level)
                                (if fire-state
                                  (Math/sin (/ f 60))
                                  0))
                          (set! (.-uniforms.alpha.value dark-filter-sprites)
                                (if fire-state 0 (Math/sin (/ f 60))))
                          (<! (e/next-frame))
                          (when (pos? f)
                            (recur (dec f)))))
                      )

            inventions (js/PIXI.Container.)

            ]
        (.addChild player-batch player)

        (m/with-sprite canvas :inventions
          [inventions inventions]

          (m/with-sprite canvas :stats
            [food (s/make-sprite :food :scale 4 :x 50 :y 50)
             food-text (pf/make-text :small (str (:food @state)) :scale 4 :y 35 :x 100)

             life (s/make-sprite :health :scale 4 :x 50 :y 110)
             life-text (pf/make-text :small (str (:life @state)) :scale 4 :y 95 :x 120)

             love (s/make-sprite :heart :scale 4 :x 50 :y 170)
             love-text (pf/make-text :small (str (:love @state)) :scale 4 :y 155 :x 100)
             ]

            (go
              (loop [food-num (:food @state)]
                (<! (e/next-frame))
                (when (not= (:food @state) food-num)
                  ;; change food num
                  (.removeChildren food-text)
                  (change-text! food-text :small (str (:food @state)))
                  )
                (recur (:food @state))))

            (go
              (loop [life-num (:life @state)]
                (<! (e/next-frame))
                (when (not= (:life @state) life-num)
                  ;; change life num
                  (.removeChildren life-text)
                  (change-text! life-text :small (str (:life @state)))
                  )
                (recur (:life @state))))


            (m/with-sprite canvas :bg
              [background bg
               main level-batch
               sprite-batch player-batch
               ]

                                        ;(set! (.-blendMode dark-filter) js/PIXI.BLEND_MODES.ADD)
              (set! (.-filters main) (make-array dark-filter-level))
              (set! (.-filters sprite-batch) (make-array dark-filter-sprites))

              (set! (.-interactive main) true)
              (set! (.-mousedown main) )
              (s/set-pos! main -1000 -1000)
              (s/set-scale! main 4)
              (s/set-pos! background -1000 -1000)

              (loop [mode (<! (survive-or-think))]

                #_ (when (= :think mode)
                     (swap! state update-in [:thoughts] inc))

                (log (clj->js @state))

                (when (:fire @state)
                  (s/set-texture! (:fire-sprite @state) :fire))
                (<! (sunset (:fire @state)))
                (<! (timeout 1000))
                (<! (sunrise (:fire @state)))
                (when (:fire @state)
                  (s/set-texture! (:fire-sprite @state) :fire-place))

                ;; invention?
                (when (= :think mode)
                  (log "1" (:thoughts @state))
                  (when (= 2 (:thoughts @state))
                    (log "2")
                    (sound/play-sound :blop 0.5 false)
                    (add-shelter! sprite-batch -48 0)
                    (<! (pop-up
                         [:lines
                          [:line
                           [:white "New Invention!"]]

                          [:line
                           [:black "Last night you invented"]]
                          [:line
                           [:brown "SHELTER"]]]))
                    (.addChild inventions
                               (s/make-sprite :shelter :scale 4 :x -50 :y 50))
                    )

                  (when (= 4 (:thoughts @state))
                    (sound/play-sound :blop 0.5 false)


                    (swap! state assoc
                           :fire true
                           :fire-sprite (add-fire! sprite-batch 32 0))
                    (<! (pop-up
                         [:lines
                          [:line
                           [:white "New Invention!"]]

                          [:line
                           [:black "Last night you invented"]]
                          [:line
                           [:yellow "FIRE"]]]))
                    (.addChild inventions
                               (s/make-sprite :fire :scale 4 :x -50 :y 140))
                    ))

                (let [night-attack? (< (rand) (:night-attack @state))]
                  (if night-attack?

                    ;; attacked
                    (do (<!
                         (pop-up
                          [:lines
                           [:line
                            [:white "Night Attack!"]]

                           [:line
                            [:black "You were attacked in the night by"]]
                           [:line
                            [:blue "NIGHT WOLVES"]]
                           ]))

                        (swap! state update-in [:life] - 25)
                        )



                    ;; not attacked
                    (case mode
                      :survive
                      (when (< (rand) 0.3)

                        (<!
                         (pop-up
                          [:lines
                           [:line
                            [:white "Food!"]]

                           [:line
                            [:black "You went searching at night and found"]]
                           [:line
                            [:yellow "BANANAS"]]
                           ]))
                        (swap! state update-in [:food] + 10))

                      :think
                      (when (= :think mode)
                        (swap! state update-in [:thoughts] inc)
                        )
                      )
                    )

                  ;; are you dead?
                  (if (zero? (:life @state))
                    (do (log "dead")
                        (s/set-texture! player :grave)
                        (while true (<! (e/next-frame)))
                        )

                    (do

                      ;; go complete. eat food
                      (swap! state update-in [:food] dec)

                      (let [action (<! (survive-or-think))]

                        (recur action)))))))))))))
