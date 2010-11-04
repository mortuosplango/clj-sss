(ns clj-sss.core
  (:use
   [rosado.processing]
   [rosado.processing.applet]
   [overtone.live :exclude (atan sqrt scale line abs atan2 round
                                 triangle pow sin asin acos exp
                                 cos ceil)]))

(def mouse-position (atom [0 0]))
(def ffont nil)

(def *padding* 30)
(def *top-padding* 20)
(def *width* 800)
(def *height* 300)
(def *num-rows* 8)
(def *samples* '())
(def *datadata* (make-array Boolean/TYPE *num-rows* 16))
(def *datasynths* (make-array Integer/TYPE 8))
(def *beat* (atom 0))
(def synths '('pong 'pang))


(defsynth pong [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (sin-osc freq)))))

(defsynth pang [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (pulse freq)))))


(defn draw
  []
  (let [beat (mod (round (* 15 (/ (millis) 4000.0))) 16)]
    (if (not (= beat (deref *beat*)))
      (do
        (reset! *beat* beat)
        (dotimes [rownum *num-rows*]
          (if (aget *datadata* rownum beat)
              (if (= (nth *datasynths* rownum) 1)
                (pong (* 110 (- *num-rows* rownum)))
                (pang (* 110 (- *num-rows* rownum))))))))
    (background-float 0 0 0)
    (text-font ffont)
    (dotimes [rownum *num-rows*]
      (let [y-pos (+ (* rownum *padding*) *top-padding*)]
        (string->text (str "synth " (aget *datasynths* rownum))
                      (+ (* 17 *padding*) 20) (+ y-pos 10))
        (dotimes [colnum 16]
          (let [col (aget *datadata* rownum colnum)]
            (if (= beat colnum)
              (stroke-float 125 255 125)
              (stroke-float (if (= (mod rownum 2) 1) 55 125)))
            (stroke-weight  (if (= col true) 25 15))
            (point (+ (*  colnum *padding*) 20)
                   y-pos)))))))


(defn setup []
  (def ffont (load-font "ArialNarrow-32.vlw"))
  (smooth)
  (no-stroke))

(defn mouse-pressed [evt]
  (let [x (.getX evt)
        y (.getY evt)]
    (let [ index (int (/ (float x) *padding*))
          rownum (int (/ (float y) *padding*))]
      (reset! mouse-position [index rownum])
      (if (< rownum *num-rows*)
        (if (< index 16)
          (aset *datadata* rownum index
                (not (aget *datadata* rownum index)))
          (aset *datasynths* rownum
                (if (= 1 (aget *datasynths* rownum)) 0 1 )))))))


(defapplet clj-sss :title "clj-sss"
  :size [*width* *height*]
  :setup setup :draw draw
  :mouse-pressed mouse-pressed
  )

(run clj-sss)

;; (stop clj-sss)  )
