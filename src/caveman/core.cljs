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

(s/set-default-scale! 4)

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
   :u {:pos [32 128] :size [32 32]}
   :g {:pos [64 128] :size [32 32]}
   :h {:pos [96 128] :size [32 32]}

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
  (let [texture (r/get-texture :sprites)
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
  (doall
   (for [row (range (count tilemap))
         col (range (count (first tilemap)))]
     (let [char (get-in tilemap [row col])]
       (.addChild batch
                  (s/make-sprite (tileset char)
                                 :x (* 16 col)
                                 :y (* 16 row)
                                 :xhandle 0
                                 :yhandle 0
                                 :scale 1))))))


(defn make-window [w h & {:keys [xhandle yhandle
                                 mousedown x y]
                          :or {xhandle 0.5
                               yhandle 0.6
                               x 0 y 0}}]
  (let [w-1 (dec w)
        h-1 (dec h)
        top-left (t/get-texture :top-left)
        top-right (t/get-texture :top-right)
        bottom-left (t/get-texture :bottom-left)
        bottom-right (t/get-texture :bottom-right)
        left (t/get-texture :left)
        right (t/get-texture :right)
        bottom (t/get-texture :bottom)
        top (t/get-texture :top)
        center (t/get-texture :center)
        ]

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
          children (for [x (range w)
                         y (range h)]
                     (s/make-sprite
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
                      :yhandle 0
                      :mousedown mousedown
                      :scale 1))
          window (s/make-container children :xhandle xhandle :yhandle yhandle
                                   :scale 4
                                   ;:tint 0x000000
                                   :visible true
                                   :rotation 0 :x x :y y)]
      (when mousedown
        (set! (.-interactiveChildren window) true))
      window)))

(defn night-summary [text]
  (go
    (let [c (chan)]
      (m/with-sprite :ui
        [window (make-window 12 2 :mousedown #(close! c))
         text-sprite (pf/make-text :small text :tint 0x000000 :y -10)]
        (<! c)))))

(defn pop-up [markup]
  (go
    (let [c (chan)]
      (m/with-sprite :ui
        [window (make-window 14 6 :mousedown #(close! c)
                             :x 0 :y 50 :yhandle 0
                             )]
        (m/with-sprite-set :ui
          [lines
           (->
            (let [[name & args] markup]
              (case name
                :lines
                (for [[lnum [lname & largs]] (map vector (range) args)]
                  (case lname
                    :line
                    (for [[cname & [text]] largs]
                      (pf/appear-text :small text
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
                                      :x 0 :y (+ 240 (* -1 12 4) (* 12 4 lnum))
                                      :xhandle 0.5 :yhandle 0))))))

            flatten)]
          (<! c))))))


(defn make-action-window [t & opts]
  (let [window (apply make-window 8 4 opts)
        text (pf/make-text :small "Survive")]
    (.addChild window text)
    window))

(defn survive-or-think []
  (let [c (chan)
        ;; this is a hack to get around a <! inside macro inside go block returning wrong value.
        ;; TODO: investigate this use case
        res (chan)]
    (go
      (m/with-sprite :ui
        [window-left (make-window 8 2 :xhandle 0.5 :yhandle 0.5
                                  :x 250 :y 150
                                  :mousedown #(put! c :survive))
         window-right (make-window 8 2 :xhandle 0.5 :yhandle 0.5
                                   :x -250 :y 150
                                   :mousedown #(put! c :think))
         text-left (pf/make-text :small "Survive"
                                 :x -250 :y 150
                                 :xhandle 0.5 :yhandle 0.5
                                 :tint 0x000000)
         text-right (pf/make-text :small "Think"
                                  :x 250 :y 150
                                  :xhandle 0.5 :yhandle 0.5
                                  :tint 0x000000)]
        (>! res (<! c))))
    res))

(defn add-shelter! [batch x y]
  (let [s (s/make-sprite :shelter :x x :y y)]
    (.addChild batch s)
    s))

(defn add-fire! [batch x y]
  (let [s (s/make-sprite :fire-place :x x :y y)]
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
     [:black result]]
    ]))

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
  (s/set-pos! u -125 (+ (* 5 (Math/sin (/ t 10))) -300))
  (s/set-pos! g 0 (+ (* 5 (Math/sin (+ 1.04 (/ t 10)))) -300))
  (s/set-pos! h 140 (+ (* 5 (Math/sin (+ 2.08 (/ t 10)))) -300)))

(defn titlescreen []
  (let [clicked? (atom false)
        click-fn (fn [ev] (reset! clicked? true))
        ]
    (go
      (m/with-sprite :ui
        [ug (s/make-sprite :u :x -180 :y -300 :mousedown click-fn :scale 5)
         g (s/make-sprite :g :x 0 :y -300 :mousedown click-fn :scale 5)
         h (s/make-sprite :h :x 200 :y -300 :mousedown click-fn :scale 5)
         ]

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
              [:green "LEECHES" sub-to-zero (* damage 5)]])
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
              [:green "LEECHES" sub-to-zero (* damage 10)]])
            ]
        (<!
         (night-attack [color name]))
        (swap! state update-in [:life] func arg)))))

(defonce main
  (go
    (<! (r/load-resources canvas :ui ["img/sprites.png"
                                      "img/fonts.png"
                                      "img/light.png"
                                      "sfx/blop.ogg"]
                          :full-colour 0xa8c032
                          :scale 1))

    (pf/pixel-font :small "img/fonts.png" [5 5] [250 60]
                   :chars ["ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                           "abcdefghijklmnopqrstuvwxyz"
                           "0123456789!?#`'.,"]
                   :kerning {"fo" -2  "ro" -1 "la" -1 }
                   :space 5)

    (t/load-sprite-sheet! (r/get-texture :sprites) spritesheet)

    (let [tileset (make-tileset)
          bg (js/PIXI.TilingSprite.
              (:empty tileset)
              2000 2000)
          level (make-field)
          level-batch (js/PIXI.Container.) ;; ParticleContainer cant have filters applies

          state (atom nil)
          ]
      (loop []
        (reset! state {
                       :fire false
                       :night-attack 0.60
                       :population 1
                       :defence 0
                       :damage 1

                       :club false
                       :wheel false
                       :religion false
                       :food 10
                       :life 100
                       :love 0
                       :thoughts 0})
        (add-tiles! level-batch tileset level)

        (let [player (s/make-sprite :grave)
              player-batch (js/PIXI.Container.)

              dark-filter-level (lightmap-filter
                                 (r/get-texture :light)
                                 #js  [0 0 0 0.5]
                                 #js  [1.0 1.0]
                                 0.0 0.0)

              dark-filter-sprites (lightmap-filter
                                   (r/get-texture :light)
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

              inventions (js/PIXI.Container.)]
          (.addChild player-batch player)
          (m/with-sprite :inventions
            [inventions inventions]

            (m/with-sprite :stats
              [food (s/make-sprite :food :x 50 :y 50)
               food-text (pf/make-text :small (str (:food @state)) :y 35 :x 100 :xhandle 0 :yhandle 0)

               life (s/make-sprite :heart :x 50 :y 110)
               life-text (pf/make-text :small (str (:life @state)) :y 95 :x 100 :xhandle 0 :yhandle 0)
               ]

              (go
                (loop [food-num (:food @state)]
                  (<! (e/next-frame))
                  (when (not= (:food @state) food-num)
                    ;; change food num
                    (pf/change-text! food-text :small
                                     (str (max 0 (int (:food @state))))
                                     0xffffff)
                    (s/update-handle! food-text 0 0)
                    )
                  (recur (:food @state))))

              (go
                (loop [life-num (:life @state)]
                  (<! (e/next-frame))
                  (when (not= (:life @state) life-num)
                    ;; change life num
                    (pf/change-text! life-text :small
                                     (str (max 0 (int (:life @state))))
                                     0xffffff)
                    (s/update-handle! life-text 0 0))
                  (recur (:life @state))))


              (m/with-sprite :bg
                [background bg
                 main level-batch
                 sprite-batch player-batch
                 ]

                                        ;(set! (.-blendMode dark-filter) js/PIXI.BLEND_MODES.ADD)
                (set! (.-filters main) (make-array dark-filter-level))
                (set! (.-filters sprite-batch) (make-array dark-filter-sprites))

                (set! (.-interactive main) true)

                (s/set-pos! main -1000 -1000)
                (s/set-scale! main 4)
                (s/set-pos! background -1000 -1000)

                (<! (titlescreen))

                (s/set-texture! player
                                (rand-nth
                                 [:caveman :caveman-2 :caveman-3
                                  :caveman-4 :caveman-5 :caveman-6]))

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

                  (let [night-attack? (< (rand) (:night-attack @state))]
                    (if night-attack?

                      ;; attacked
                      (<! (do-night-attack state mode))

                      ;; not attacked
                      (case mode
                        :survive
                        (if (< (rand) 0.5)
                          (let [[color name func arg]
                                (rand-nth
                                 [
                                  [:yellow "BANANAS" + 10]
                                  [:brown "BULBS" + 7]
                                  [:brown "POTATOES" + 7]
                                  [:brown "ROOTS" + 5]
                                  [:green "SHOOTS" + 5]
                                  [:green "LEAVES" + 5]
                                  [:red "STRAWBERRYS" + 7]
                                  [:orange "CITRUS" + 7]
                                  ])]
                            (log color name func arg)
                            (<! (found-food [color name]))
                            (swap! state update-in [:food] func arg))

                          ;; eat food and get healthier
                          (when (zero? (:food @state))
                            (<! (hungry))
                            )
                          )

                        :think
                        (when (= :think mode)
                          (swap! state
                                 #(-> %
                                      (update-in [:thoughts] inc)
                                        ;(update-in [:food] dec)
                                      ))

                          (case (:thoughts @state)
                            2
                            (do
                              (sound/play-sound :blop 0.5 false)
                              (add-shelter! sprite-batch -48 0)
                              (<! (invention [:brown "SHELTER"] "damage is halved"))
                              (.addChild inventions
                                         (s/make-sprite :shelter :x -50 :y 50))
                              (swap! state assoc :damage 0.5))

                            4
                            (do
                              (sound/play-sound :blop 0.5 false)


                              (swap! state assoc
                                     :fire true
                                     :fire-sprite (add-fire! sprite-batch 32 0))
                              (<! (invention [:yellow "FIRE"] "night attacks are rarer"))
                              (.addChild inventions
                                         (s/make-sprite :fire :x -50 :y 140))
                              )

                            6
                            (do
                              (sound/play-sound :blop 0.5 false)


                              (swap! state assoc :club true)
                              (<! (invention [:brown "CLUB"] "night attacks may be hunted"))
                              (.addChild inventions
                                         (s/make-sprite :club :x -50 :y 230))
                              )

                            8
                            (do
                              (sound/play-sound :blop 0.5 false)


                              (swap! state assoc :wheel true)
                              (<! (invention [:green "WHEEL"] "trade routes open"))
                              (.addChild inventions
                                         (s/make-sprite :wheel :x -50 :y 320))
                              )

                            10
                            (do
                              (sound/play-sound :blop 0.5 false)


                              (swap! state assoc :religion true)
                              (<! (invention [:purple "RELIGION"] "fear of death decreases"))
                              (.addChild inventions
                                         (s/make-sprite :religion :x -50 :y 410))
                              )

                            (if (zero? (:life @state))
                              (<! (hungry))
                              (<! (no-invention)))
                            )


                          ))


                      )


                    )
                  ;; are you dead?
                  (if (zero? (int (:life @state)))
                    (do (log "dead")
                        (s/set-texture! player :grave)
                        (<! (pop-up
                             [:lines
                              [:line
                               [:white "You Dead!"]]
                              [:line
                               [:black "There goes the species"]]])))

                    (do

                      ;; go complete. eat food
                      (swap! state
                             (fn [{:keys [food] :as s}]
                               (if (pos? food)
                                 (-> s
                                     (update-in [:food] dec)
                                     (update-in [:life] + 5)
                                     (update-in [:life] min 100))

                                 ;; go hungry
                                 (-> s
                                     (update-in [:life] - 10))
                                 )))

                      (let [action (<! (survive-or-think))]

                        (recur action)))))))))
        (recur)))

    ))


(defn make-text [font-key text x y right line-spacing charset]
  (let [font (pf/get-font font-key)
        height (:height font)
        word-extents (vec (pf/word-beginnings-ends text))
        num (pf/how-many-words-fit font-key text (map second word-extents) right)
        start (-> word-extents first first)
        end (-> num dec word-extents second)
        section (subs text start (- end start))
        width (pf/string-width font-key section)
        diff (- right width)
        num-spaces (count (re-seq #" " section))
        more? (< num (count word-extents))
        padding (if more? (/ diff num-spaces) 0)
        line (pf/make-char-sprite-set font-key section :tint 0xffffff :x 0 :y y :space-padding padding)]
    (if more?
      (make-text font-key (subs text (-> num word-extents first))
                 x (+ y (* line-spacing height))
                 right line-spacing (into charset line))
      (into charset line))))


(defonce run-later
  (go
    (<! (timeout 2000))
    (m/with-sprite :ui
      [example (s/make-container
                (make-text :small "This is some sample text that goes on and on and needs to wrap like the quick brown fox, who allegedly, jumped over the lazy dog!" 0 0 200 1.2 [])
                :scale 2)]
      (<! (timeout 10000))
      )
    ))
