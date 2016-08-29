(ns caveman.async
  (:require [cljs.core.async.macros :as m]
            [clojure.walk :refer [macroexpand-all]]))

;; macros for more convenient game async
(defmacro <!* [test & body]
  `(let [result# (~'<! ~@body)]
     (if ~test
       result#
       (throw (js/Error "go-while exit")))))

(defmacro >!* [test & body]
  `(let [result# (~'>! ~@body)]
     (if ~test
       result#
       (throw (js/Error "go-while exit")))))

(comment
  (macroexpand-1 '(>!* @kill-atom (e/next-frame foo)))
  (macroexpand-1 '(<!* @kill-atom (e/next-frame foo))))

(defn process-body [test [head & tail]]
  (let [
        [processed-head processed-tail]
        (cond
          (identical? '() head) ['() tail]
          (identical? [] head) [[] tail]
          (list? head) [(process-body test head) tail]
          (vector? head) [(vec (process-body test head)) tail]
          (map? head) [(into {} (process-body test (seq head))) tail]
          (= '<! head) ['caveman.async/<!* (cons test tail)]
          (= '>! head) ['caveman.async/>!* (cons test tail)]
          :default [head tail])]
    (if processed-tail
      (cons processed-head (process-body test processed-tail))
      (list processed-head))))

(comment
  (process-body 'test2 '(do (foo) (bar) (>! (nf))
                            (foo [])
                            ()
                            {}
                            (loop [t (<! (nf))
                                   p {:a (<! b)
                                      :b :b
                                      (<! c) :c}] (bing) (<! (nf))
                                      (when bong (recur (inc t))))))

  (process-body 'test '(foo [] ())))

(defmacro go-while [test & body]
  `(m/go ~@(process-body test body)))

(defmacro continue-while [test & body]
  (first (process-body test body)))

(defmacro ^:private assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro get-layer [canvas layer]
  `(or
    (-> ~canvas :layer ~layer)
    (throw (js/Error (str "canvas layer " ~layer " not found.")))
    ))

(defmacro with-sprite [canvas layer bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (even? (count bindings)) "an even number of forms in binding vector")

  (if (pos? (count bindings))
    (let [symb (first bindings) val (second bindings)]
      `(let [~symb ~val]
         (try (.addChild (get-layer ~canvas ~layer) ~symb)
              (with-sprite ~canvas ~layer ~(subvec bindings 2) ~@body)
              (finally (.removeChild (get-layer ~canvas ~layer) ~symb)))))
    `(do ~@body)))


(macroexpand-all '(continue-while (not @clicked?)
                                (with-sprite canvas :ui
        [ug (s/make-sprite (r/get-texture :u :nearest) :scale 4 :x -180 :y -300 ;:mousedown click-fn
                           )
         g (s/make-sprite (r/get-texture :g :nearest) :scale 4 :x 0 :y -300)
         h (s/make-sprite (r/get-texture :h :nearest) :scale 4 :x 200 :y -300)

         ]
        (do
          (log "U" ug)
          (set! (.-interactive ug) true)
                                        ;(set! (.-buttonMode ug) true)
                                        ;(set! (.-mousedown u) click-fn)

          (loop [t 0]
            (set-pos t ug g h)
            (<! (e/next-frame))
            (recur (inc t))))
        )

                 ))

(comment
  (macroexpand-1
   '(go-while (= a b) (do (foo) (bar) (<! (nf))
                          (loop [] (bing) (<! (nf)) (when bong (recur)))
                          last)
              very-last
              )))
