(ns clj-sss.core
  (:use
   [rosado.processing]
   [rosado.processing.applet]
   [overtone.live :exclude (atan sqrt scale line abs atan2 round
                                 triangle pow sin asin acos exp
                                 cos ceil)])
  (:gen-class))


;;; size of beats and padding to window border in px
(def *beat-spacing* 30) 
(def *window-padding* 30)

;;; Parameters of the sequencer
(def *num-tracks* 8)
(def *num-beats* 16)                    ; beats per track
(def *metro* (metronome 260))           ; beats per minute

(def *ffont* nil)
(def *tracks* (atom []))

(def *synths* [
               (defsynth d-sine [freq 440 amp 0.1 sustain 0.8]
                 (out 0
                      (pan2 (* amp
                               0.5
                               (env-gen (perc 0.01 sustain 1 -16)
                                        1 1 0 1 :free)
                               (f-sin-osc [freq (* freq 1.02)]))
                            (- (* 2 (/ (cpsmidi freq) 127.0)) 1))))

               (defsynth d-sub [freq 440 amp 0.1]
                 (let [env (env-gen (perc 0.01 0.5 1 -4) 1 1 0 1 :free)]
                   (out 0
                        (pan2 (* amp
                                 env
                                 0.5
                                 (rlpf (white-noise)
                                      [freq (* freq 1.01)]
                                      [0.05 0.04]))))))
               
               (defsynth d-tsch [freq 440 amp 0.1]
                 (let [env (env-gen (perc 0.01 0.4 1 -8) 1 1 0 1 :free)]
                   (out 0
                        (pan2 (* amp
                                 env
                                 (rhpf (white-noise)
                                       (* (+ 1 env)
                                          (* freq 2)) 0.4))))))
               
               (defsynth d-hit [freq 440 amp 0.1]
                 (out 0
                      (pan2 (* amp
                               (env-gen (perc 0.01 0.1)
                                        1 1 0 1 :free)
                               (sin-osc (* freq
                                           (env-gen
                                            (perc 0.01 0.1)
                                            1 1 0 1 :free)))))))])


;;; representation of a track
(defprotocol Display
  (display [this]))

(defprotocol Play
  (play [this tick]))

(defrecord Track [index data synth beat]
  Display
  (display [this]
           (let [y-pos (+ (* index *beat-spacing*) *window-padding*)]
             (string->text (str "synth: " (:name (meta (nth *synths* @synth))))
                           (+ (* (inc *num-beats*)
                                 *beat-spacing*) *window-padding*)
                           (+ y-pos 10))
             (doseq [i (range *num-beats*)]
               (if (= @beat i)
                 (stroke-float 125 255 125)
                 (stroke-float (if (= (mod index 2) 1) 55 125)))
               (if (nth @data i)
                 (stroke-weight (* *beat-spacing* 0.8))
                 (stroke-weight (* *beat-spacing* 0.4)))
               (point (+ (* i *beat-spacing*) *window-padding*)
                      y-pos))))
  Play
  (play [this tick]
        (when (nth @data (reset! beat (mod tick (count @data))))
          ((nth *synths* @synth)
           (* 110 (- *num-tracks* index))))))


;;; sequencing loop
(defn player [beat]
  (at (*metro* beat)
      (doseq [track @*tracks*]
        (play track beat)))
  (apply-at #'player (*metro* (inc beat)) (inc beat)))


;;; draw loop
(defn draw
  []
  (let [beat (mod (- (*metro*) 1) *num-beats*)]
    (background-float 20)
    (text-font *ffont*)
    (fill-float 80 125 80)
    (doseq [track @*tracks*]
      (display track)
      )))

(defn setup []
  (def *ffont* (load-font "ArialNarrow-32.vlw"))
  (smooth)
  (no-stroke)  

  (reset! *tracks* [])
  (let [empty-track (apply vector (repeat *num-beats* false))]
    (dotimes [i *num-tracks*]
      (swap! *tracks* conj (Track. i (atom empty-track) (atom 0) (atom 0)))))
  
  (player (*metro*)))


(defn mouse-pressed [evt]
  (let [where? (fn [val] (int
                          (/ (- (+ (float val)
                                   (/ *beat-spacing* 2.0))
                                *window-padding*)
                             *beat-spacing*)))
        index (where? (.getX evt))
        tracknum (where? (.getY evt))]
    (when (< tracknum *num-tracks*)
      (if (< index *num-beats*)
        (swap! (.data (nth @*tracks* tracknum))
               #(assoc %1 index (not (nth %1 index)) ))
        (swap! (.synth (nth @*tracks* tracknum)) 
               #(mod (+ 1 %1) (count *synths*)))))))




(defapplet clj-sss :title "clj-sss"
  :size [(+ (* 2 *window-padding*) (* *beat-spacing* (+ *num-beats* 6)))
         (+ (* *beat-spacing* *num-tracks*) (* 2 *window-padding*))]
  :setup setup
  :draw draw
  :mouse-pressed mouse-pressed)


(defn -main [& args]
  (run clj-sss))

;;(run clj-sss)

;; (stop clj-sss) 
