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
(def *datadata* (make-array Boolean/TYPE *num-rows* 16))
(def *datasynths* (make-array Integer/TYPE 8))
(def *beat* (atom 0))
(def *synths* [d-sine d-saw d-tsch d-hit])

(defsynth d-sine [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.2) 1 1 0 1 :free)
                (sin-osc freq)))))

(defsynth d-saw [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (pulse freq)))))

(defsynth d-tsch [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.05) 1 1 0 1 :free)
                (gray-noise)))))

(defsynth d-hit [freq 440 amp 0.5]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (sin-osc (* freq (env-gen (perc 0.01 0.1) 1 1 0 1 :free)))))))

(defn draw
  []
  (let [beat (mod (round (* 15 (/ (millis) 4000.0))) 16)]
    (if (not (= beat (deref *beat*)))
      (do
        (reset! *beat* beat)
        (dotimes [rownum *num-rows*]
          (if (aget *datadata* rownum beat)
            ((nth *synths* (aget *datasynths* rownum))
             (* 110 (- *num-rows* rownum)))))))
    (background-float 10 20 10)
    (text-font ffont)
    (fill-float 60 125 60)
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
                (mod (+ 1 (aget *datasynths* rownum)) 4)))))))


(defapplet clj-sss :title "clj-sss"
  :size [*width* *height*]
  :setup setup :draw draw
  :mouse-pressed mouse-pressed
  )

(run clj-sss)

;; (stop clj-sss)  )
