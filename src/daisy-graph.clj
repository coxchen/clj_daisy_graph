(use '(incanter core processing))

(def BKG-COLOR (color 200 200 200))

(def ORIG {:x 150 :y 150})

(defn daisy-slice [sktch orig r thick start-degree stop-degree]
  (let [outter-r (* 2 (+ r thick))
	inner-r (* 2 r)
        x (:x orig)
        y (:y orig)
        start (radians start-degree)
        stop (radians stop-degree)]
    (doto sktch
      (stroke-weight 1)
      (stroke 255 0 0)
      (fill 255 255 0)
      (arc x y outter-r outter-r start stop)
      (fill BKG-COLOR)
      (arc x y inner-r inner-r start stop)
      (rotate start)
      (line (+ x r) y (+ x r thick) y)
      (rotate (- stop start))
      (line (+ x r) y (+ x r thick) y)
      (rotate (* (- 0 1) stop)))))

(defn daisy-level [sktch daisy-data slice-level start-degree total-size]
  (loop [data (:elems daisy-data),
         level slice-level,
         start start-degree]
    (let [slice (first data)
          stop (+ start
                  (* 360.0
                     (/ (:size slice) total-size)))]
      (println "\n   size:" (:size slice))
      (println "degrees:" start stop)
      (println "  elems:" (:elems slice))
      (if (seq (:elems slice))
	(daisy-level sktch slice (+ 1 level) start total-size))
      (daisy-slice sktch {:x 0 :y 0} (* level 50) 45 start stop)
      (if (seq (rest data))
        (recur (rest data) level stop)))))
  

(defn daisy-graph [sktch daisy-data]
  (let [total-size (:size daisy-data)]
    (daisy-level sktch daisy-data 1 0 total-size)))

(def Gallery13
     {:label "Gallery13"
      :size 51
      :elems [{:label "XSS"
               :size 3
               :elems [{:label "severity 1"
                        :size 2}
                       {:label "severity 2"
                        :size 1}]}
              {:label "INFO_LEAK"
               :size 1}
              {:label "WEAK_CRYPT"
               :size 47
               :elems [{:label "severity 1"
                        :size 38}
                       {:label "severity 2"
                        :size 9}]}]})

(let [sktch (sketch
	     (setup []
		    (doto this
		      (size 400 400)
		      (background BKG-COLOR)
                      ;(translate (:x ORIG) (:y ORIG))
		      smooth))
	     (draw []
                   (doto this
                     (background BKG-COLOR)
                     (translate (:x ORIG) (:y ORIG))
                     (daisy-graph Gallery13))
                   ))]
  (view sktch :size [400 400]))
