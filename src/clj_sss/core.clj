(ns clj-sss.core
  (:use
   [rosado.processing]
   [rosado.processing.applet]
   [overtone.live :exclude (atan sqrt scale line abs atan2 round
                                 triangle pow sin asin acos exp
                                 cos ceil)]))

(def *ffont* nil)

(def *beat-spacing* 30)
(def *window-padding* 30)

(def *num-rows* 8)
(def *num-beats* 16)

(def *datadata* (make-array Boolean/TYPE *num-rows* *num-beats*))
(def *datasynths* (make-array Integer/TYPE *num-rows*))

(defsynth d-sine [freq 440 amp 0.1]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.2) 1 1 0 1 :free)
                (sin-osc freq)))))

(defsynth d-saw [freq 440 amp 0.1]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (lpf (pulse [freq (* freq 1.01)]) (* freq 8))))))

(defsynth d-tsch [freq 440 amp 0.1]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.05) 1 1 0 1 :free)
                (rhpf (gray-noise) freq 0.5)))))

(defsynth d-hit [freq 440 amp 0.1]
  (out 0
       (pan2 (* amp
                (env-gen (perc 0.01 0.1) 1 1 0 1 :free)
                (sin-osc (* freq (env-gen (perc 0.01 0.1) 1 1 0 1 :free)))))))

(def metro (metronome 260))
(def *synths* [d-sine d-saw d-tsch d-hit])

(defn player [beat]
  (at (metro beat)
      (dotimes [rownum *num-rows*]
        (if (aget *datadata*  rownum (mod beat *num-beats*))
          ((nth *synths* (aget *datasynths* rownum))
           (* 110 (- *num-rows* rownum))))))
  (apply-at #'player (metro (inc beat)) (inc beat)))

(defn draw
  []
  (let [beat (mod (- (metro) 1) *num-beats*)]
    (background-float 20)
    (text-font *ffont*)
    (fill-float 80 125 80)
    (dotimes [rownum *num-rows*]
      (let [y-pos (+ (* rownum *beat-spacing*) *window-padding*)]
        (string->text (str "synth " (aget *datasynths* rownum))
                      (+ (* 17 *beat-spacing*) *window-padding*) (+ y-pos 10))
        (dotimes [colnum *num-beats*]
          (if (= beat colnum)
            (stroke-float 125 255 125)
            (stroke-float (if (= (mod rownum 2) 1) 55 125)))
          (stroke-weight  (if (aget *datadata* rownum colnum) 25 15))
          (point (+ (*  colnum *beat-spacing*) *window-padding*)
                 y-pos))))))

(defn setup []
  (def *ffont* (load-font "ArialNarrow-32.vlw"))
  (player (metro) )
  (smooth)
  (no-stroke))

(defn mouse-pressed [evt]
  (let [ index (int
                (/ (- (+ (float (.getX evt))
                         (/ *beat-spacing* 2.0))
                      *window-padding*) *beat-spacing*))
        rownum (int
                (/ (- (+ (float (.getY evt))
                         (/ *beat-spacing* 2.0))
                      *window-padding*) *beat-spacing*))]
    (if (< rownum *num-rows*)
      (if (< index *num-beats*)
        (aset *datadata* rownum index
              (not (aget *datadata* rownum index)))
        (aset *datasynths* rownum
              (mod (+ 1 (aget *datasynths* rownum)) (count *synths*)))))))


(defapplet clj-sss :title "clj-sss"
  :size [(+ (* 2 *window-padding*) (* *beat-spacing* (+ *num-beats* 4)))
         (+ (* *beat-spacing* *num-rows*) (* 2 *window-padding*))]
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed)

(run clj-sss)

;; (stop clj-sss) 


