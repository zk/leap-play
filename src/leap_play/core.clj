(ns leap-play.core
  (:import [com.leapmotion.leap
            Controller
            Listener]))

;; SDK Helpers

(defn make-listener [on-frame]
  (proxy [Listener] []
         (onInit [controller]
           (println "init"))
         (onFrame [controller]
           (on-frame controller))))

(defn make-controller [listener]
  (Controller. listener))

(def state (atom {}))

(def played (atom {}))

(defn hand-obj->palm [hand-obj]
  (let [palm (.palm hand-obj)
        fingers (.fingers hand-obj)
        pos (.getPosition palm)
        x (.getX pos)
        y (.getY pos)
        z (.getZ pos)
        v (.velocity hand-obj)
        vx (.getX v)
        vy (.getY v)
        vz (.getZ v)
        finger (if (> (.size fingers) 0)
                 (.get fingers 0))
        vel (if finger (.velocity finger))
        tip (if finger (.tip finger))
        y (if tip (.getY (.getPosition tip)))
        vy (if vel (.getY vel))]
    #_(println y vy)
    {(.id hand-obj) {:y y :vy vy :x x :z z}}))

(defn bongo? [id old-state new-state]
  (let [old-pos (get old-state id)
        new-pos (get new-state id)
        old-y (:y old-pos)
        old-vy (:vy old-pos)
        old-x (:x old-pos)
        new-y (:y new-pos)
        new-x (:x new-pos)
        new-z (:z new-pos)
        new-vy (:vy new-pos)]
    (and (< new-vy -300)
         (< new-y 350)
         (< new-z 90)
         (> new-y 110))))

(defn with-hands [hands]
  (let [old-hands @state
        new-hands hands
        hand-ids (keys new-hands)]
    (reset! state new-hands)
    (doseq [id hand-ids]
      (let [new-vy (:vy (get new-hands id))]
        (when (and (bongo? id old-hands new-hands)
                   (not (get @played id)))
          (swap! played assoc id true))
        (when (> new-vy 0)
          (swap! played (fn [old]
                          (if (get old id)
                            (println "BONGO!!!")
                            (assoc old id false)))))))))

(defn extract-hands [hands-obj]
  (let [num-hands (.size hands-obj)]
    (loop [hands {} n 0]
      (if (>= n num-hands)
        hands
        (recur (merge hands (hand-obj->palm (.get hands-obj n))) (inc n))))))

(defn on-frame [c]
  (let [frame (.frame c)
        hands-obj (.hands frame)
        hands (extract-hands hands-obj)]
    (when-not (empty? hands)
      (try
        (with-hands hands)
        (catch Exception e)))))

(def l (make-listener on-frame))
(def c (make-controller l))


