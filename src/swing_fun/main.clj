(ns swing-fun.main
  (:gen-class)
  (:require [com.climate.claypoole :as cp])
  (:import (javax.swing JFrame)
           (javax.swing JLabel)
           (javax.swing ImageIcon)
           (java.awt.image BufferedImage)
           (java.awt.event MouseAdapter)))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(def pool (cp/threadpool (cp/ncpus)
                         :daemon true
                         :thread-priority 4
                         :name "my-pool"))


(def initial-state
  {:center-re 0.0
   :center-im 0.0
   :max-iter  4096
   :zoom      (/ 1.0 256.0)})

(defonce state
  (atom initial-state))

(def palette
  [0x00421E0F 0x0019071A 0x0009012F 0x00040449
   0x00000764 0x000C2C8A 0x001852B1 0x00397DD1
   0x0086B5E5 0x00D3ECF8 0x00F1E9BF 0x00F8C95F
   0x00FFAA00 0x00CC8000 0x00995700 0x006A3403])

(defn mouse-released-handler [state ^java.awt.event.MouseEvent event]
  (let [x      (.getX event)
        y      (.getY event)
        button (if-not (zero? (bit-and (.getModifiers event) java.awt.event.InputEvent/BUTTON1_MASK))
                 :left
                 :right)

        {:keys [^double zoom ^double center-re ^double center-im ^long center-x ^long center-y]} state
        new-state
        (case button
          :left (-> state
                    (update :zoom (fn [^double z] (/ z 4.0)))
                    (assoc :center-re (+ center-re (* (- x center-x) zoom)))
                    (assoc :center-im (- center-im (* (- y center-y) zoom))))

          :right (update state :zoom (fn [^double z] (* 4.0 z)))
          state)]
    (prn (dissoc new-state :image :frame))
    new-state))


(defn clear [state color]
  (let [frame ^JFrame (:frame state)
        image ^BufferedImage (:image state)]
    (doseq [x     (range (.getWidth image))
            y     (range (.getHeight image))]
      (.setRGB image x y color))
    (.repaint frame)))


(defn draw-pattern [state]
  (let [image  ^BufferedImage  (:image state)
        frame  ^JFrame (:frame state)
        width  (:width state)
        height (:height state)]
    (doseq [x    ^long (range width)
            y    ^long (range height)
            :let [color (bit-and ^long x ^long y) #_(if (zero? (bit-and ^long x ^long y)) 0xffffff 0)]]
      (.setRGB image x y color))
    (.repaint frame)))

(deftype Complex [^double re ^double im])

(defn mandel-for-complex ^long [^Complex z ^long max-iter]
  (let [max-dist 4.0]
    (loop [z_re ^double (.re z)
           z_im ^double (.im z)
           i    0]
      (let [re_sqr (* z_re z_re)
            im_sqr (* z_im z_im)]
        (if (> (+ re_sqr im_sqr) max-dist)
          i  ;; outside The Mandelbrot Set
          (if (>= i max-iter)
            0  ;; inside The Mandelbrot Set
            (recur (+ (- re_sqr im_sqr) ^double (.re z))
                   (+ (* 2.0 z_re z_im) ^double (.im z))
                   (inc i))))))))

(defn draw-mandel-line [state ^long y]
  (let [{:keys [frame image ^double zoom ^long max-iter width ^long center-x ^long center-y ^double center-re ^double center-im]}
        state

        im     (- center-im (* (- y center-y) zoom))]
    (dotimes [x width]
      (let [re     (+ center-re (* (- x center-x) zoom))
            mandel (mandel-for-complex (->Complex re im) max-iter)
            color  (if (zero? mandel)
                     0
                     (nth palette (mod mandel (count palette))))]
        (.setRGB ^BufferedImage image x y color)))
    (.repaint ^JFrame frame)))

(defn draw-mandel [{:keys [height] :as state }]
  #_(dorun (map (partial draw-mandel-line state) (range height)))
  #_(dorun (pmap (partial draw-mandel-line state) (range height)))
  (dorun (cp/upmap pool (partial draw-mandel-line state) (range height))))

(add-watch state :zoom-changed (fn [_ _ old-state new-state]
                                 (when-not (= (:zoom old-state) (:zoom new-state))
                                   (println "Redraw!")
                                   (future
                                     (time (draw-mandel new-state))))))


(defn -main []
  (let [frame  ^JFrame (JFrame.)
        _      (doto frame
                 (.setExtendedState JFrame/MAXIMIZED_BOTH)
                 (.setUndecorated true)
                 (.setVisible true))
        _      (Thread/sleep 100)
        size   (-> frame .getContentPane .getSize)
        width  (.getWidth size)
        height (.getHeight size)
        image  ^BufferedImage (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]

    (doto frame
      (.add (JLabel. ^ImageIcon (ImageIcon. image)))
      (.addMouseListener
       (proxy [MouseAdapter] []
         (mouseReleased [event] (swap! state (fn [s] (mouse-released-handler s event))))))
      (.pack))

    (swap! state assoc
           :width     width
           :height    height
           :center-x  (quot width 2)
           :center-y  (quot height 2)
           :frame     frame
           :image     image))

  (time (draw-mandel @state)))


(defn zoom-to! [new-state]
  (swap! state (fn [s] (merge s new-state))))

(comment
  (-main)

  (remove-watch state :zoom-changed)
  (swap! state (fn [s] (merge s initial-state)))

  (do
    (Thread/sleep 1000)
    (time (draw-mandel @state)))

  (time (clear @state 0x000000))

  (time (draw-pattern @state))

  (zoom-to! {:zoom 9.765625E-4            :center-y 432.0 :center-im 0.267578125         :center-x 768.0 :center-re -0.7861328125})
  (zoom-to! {:zoom 2.2737367544323206E-13 :center-y 432.0 :center-im -0.4327114795678427 :center-x 768.0 :center-re -1.2847130488365373})
  (zoom-to! {:max-iter 4096, :zoom 5.6843418860808015E-14, :center-y 432.0 :center-im -0.43271147956693323, :center-x 768.0, :center-re -1.284713048835173})

 )
