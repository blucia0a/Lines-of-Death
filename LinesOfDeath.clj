(def width 1024)
(def height 768)

(def windspeed (atom [-2 0]))

(def weatherlines (atom []))
(def weather-line-speed (atom 7))
(def weather-line-speed-timer (atom 0))

(def position (atom [0 (/ width 2)]))
(def displacement (atom [0 0]))
(def player-speed (atom 6))
(def player-radius (atom 14))

(def enemy-position (atom [(rand width) (rand height)]))
(def enemy-displacement (atom [0 0]))
(def enemy-speed (atom 2))

(def covered-extents (atom [0 0]))

(def score (atom 0))
(def death-count (atom 0))

(defn initialize-game[]
  (reset! weatherlines [])
  (dotimes [x 50]
    (reset! weatherlines 
      (cons 
        (vector (+ 30 (rand (- width 30)))
          (rand height) 
          (rand (/ width 4)) 
        ) 
        @weatherlines
      )
    )
  )
  (reset! position [0 (/ width 2)])
  (reset! weather-line-speed 4)
  (reset! weather-line-speed-timer 0)
  (let [py (second @position)]
    (reset! covered-extents [py (+ 1 py)])
  )
)

(def gamelooper (agent true))
(def weathermanager (agent true))
(def playermanager (agent true))
(def enemymanager (agent true))

(defn drawCharacter [g2d x y]
  (. g2d setColor java.awt.Color/white)
  (. g2d fillOval (- x 1) (- y 1) 27 27)
  (. g2d setColor java.awt.Color/green)
  (. g2d fillOval x y 25 25)
  (. g2d setColor java.awt.Color/black)
  (. g2d fillOval (+ x 8) (+ y 3) 7 7)
  (. g2d fillOval (+ x 21) (+ y 3) 7 7)
  (. g2d setColor java.awt.Color/white)
  (. g2d fillOval (+ x 9) (+ y 4) 5 5)
  (. g2d fillOval (+ x 22) (+ y 4) 5 5)
  (. g2d setColor java.awt.Color/black)
  (. g2d fillOval (+ x 14) (+ y 14) 10 5)
  (. g2d setColor java.awt.Color/white)
  (. g2d fillOval (+ x 15) (+ y 15) 8 3)
)

(defn drawEnemy[g2d x y]
  (. g2d setColor java.awt.Color/white)
  (. g2d fillOval (- x 1) (- y 1) 27 27)
  (. g2d setColor java.awt.Color/red)
  (. g2d fillOval x y 25 25)
)

(defn drawWeather [g2d]
  (dorun 
    (map 
      #(. g2d drawLine (first %) (second %) (+ (first %) (nth % 2)) (second %) )
      @weatherlines
    )
  )
)

(defn draw [g2d]
  (. g2d setColor java.awt.Color/black)
  (. g2d fillRect 0 0 width height)

  (let [[miny maxy] @covered-extents]
    (if (and (< miny 25) 
             (> maxy (- height 30))) 
      (. g2d setColor java.awt.Color/green)
      (. g2d setColor java.awt.Color/red))
    (. g2d fillRect (- width (/ width 20)) miny 
                    (/ width 20) (- (+ 30 maxy) miny))
    (if (< miny 25)  
      (. g2d setColor java.awt.Color/green)
      (. g2d setColor java.awt.Color/red))
    (. g2d fillRect (- width (/ width 20)) 15 (/ width 20) 10)

    (if (> maxy (- height 30)) 
      (. g2d setColor java.awt.Color/green)
      (. g2d setColor java.awt.Color/red))
    (. g2d fillRect (- width (/ width 20)) (- height 30) (/ width 20) 10)
  )

  (. g2d setColor java.awt.Color/blue)
  (drawWeather g2d)
  (. g2d setColor java.awt.Color/white)
  (. g2d drawString (str "Score: " (int @score)) 3 10)
  (. g2d drawString (str "Speed: " @weather-line-speed) 300 10)
  (. g2d drawString (str "Speed Timer: " @weather-line-speed-timer) 300 20)
  (. g2d drawString (str "Deaths: " @death-count) (- width 70) 10) 
  (let [ [x y] @position ]
    (drawCharacter g2d x y))
  (let [ [x y] @enemy-position ]
    (drawEnemy g2d x y))
)

(def frame (new javax.swing.JFrame))
(def display 
  (proxy [javax.swing.JPanel] [] 
    (paintComponent [g2d] (draw g2d))))

(defn gameloop[running];This function is the game loop
  (if running 
    (send-off *agent* #'gameloop)  ;keep sending self msgs to spin
    (let [_ (println "Goodbye!")]   ;Can the program if we're not running
      (System/exit 0)))

  (.repaint display ) 
  (. Thread sleep 30 )
  running 
)

(defn intersect-player-weather[hit nxt]
  (dosync 
  (if 
    (= hit false)
    (and 
      (and 
        (> (first @position) (first nxt)) 
        (< (first @position) (+ (first nxt) (nth nxt 2))) 
      )
      (and 
        (> (second nxt) (second @position)) 
        (< (second nxt) (+ (second @position) 28))
      )
    )
    true
  )
  )
)

(defn intersect-circular[uppl1 uppl2 r]
  (let [ [x1 y1] (map #(+ % r) uppl1)]
    (let [ [x2 y2] (map #(+ % r) uppl2)]
      (let [x-intersect (and (> x1 (- x2 r)) (< x1 (+ x2 r)))]
        (let [y-intersect (and (> y1 (- y2 r)) (< y1 (+ y2 r)))]
          (and x-intersect y-intersect)))))
)

(defn player[running] 
  (when running 
    (send-off *agent* #'player))

  (dosync
    (let [ [x y] @position]
      (let [ [deltax deltay] @displacement]
        (let [ [newx newy] [(+ deltax x) (+ deltay y)] ]
          (let [xwrap (> newx width)]
            (let [ywrap (> newy (- height 30))]
              (let [yunwrap (< newy 20)]
                (let [ywrapnewval (if ywrap y newy)]
                  (let [newpos (vector (if xwrap 0 newx) (if yunwrap y ywrapnewval))]
                    (reset! position newpos)
                  )
                )
              )
              (let [[miny maxy] @covered-extents]
                (let [new-min (if (< newy miny) newy miny)]
                  (let [new-max (if (> newy maxy) newy maxy)]
                    (println (str "max: " new-max "min: " new-min))
                    (reset! covered-extents (vector new-min new-max))
                    (when (and xwrap 
                               (and (< new-min 25) (> new-max (- height 31))))
                      (reset! score (+ @weather-line-speed @score))
                      (reset! covered-extents [newy newy])
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (when 
    (or
      (< (first @position) -30)
      (reduce intersect-player-weather false @weatherlines)
    )
    (reset! death-count (+ 1 @death-count))
    (initialize-game)
  )
  

  (. Thread sleep 30)
  running 
)

(defn enemy[running] 
  (when running 
    (send-off *agent* #'enemy))

  (let [ [x y]  @enemy-position]
    (let [ [px py] @position]
      (let [ esp @enemy-speed ]
        (let [newx (if (<= x px) (+ x esp) (- x esp))]
          (let [newy (if (<= y py) (+ y esp) (- y esp))]
            (reset! enemy-position (vector (if (> newx width) (- width 1) newx)
                                           (if (> newy height)(- height 1) newy))
            )
          )
        )
      )
    )
  )

  (when 
    (intersect-circular @position @enemy-position @player-radius)
    (println (str "score: " @score))
    (let [newscore (- @score 0.05)]
      (reset! score newscore))
  )


  (. Thread sleep 30)
  running 
)

(defn update-weather-line[s] 
  (let [oneless (- (first s) @weather-line-speed)] 
    (let [wrap (< oneless 0)]
      (vector 
        (if wrap width oneless)  
        (if wrap (rand height) (second s) ) 
        (if wrap (rand (/ width 4)) (nth s 2) ) 
      )
    )
  )
)

(defn weather[running] ;This function is the game loop
  (when running 
    (send-off *agent* #'weather))
  
  (reset! weatherlines 
          (map (fn [s] (update-weather-line s)) 
               @weatherlines
          )
  )
  (reset! weather-line-speed-timer (inc @weather-line-speed-timer))
  (when (> @weather-line-speed-timer 500)
    (reset! weather-line-speed (inc @weather-line-speed))
    (reset! weather-line-speed-timer 0)
  )
    
 
  (dosync
    (let [ [x y] @position]
      (let [ [windx windy] @windspeed]
        (reset! position [(if (> (+ windx x) width) 0 (+ windx x)) 
                          (if (> (+ windy y) width) 0 (+ windy y))]
        )
      )
    )
  )

  (. Thread sleep 50)
  running 
)

(defn stop-gameloop []
  (send gamelooper (fn [x] false)))

(defn handle-keypress [evt]
  (let [cha (str (.getKeyChar evt))]
    (let [xinc (condp = cha 
                 "l" @player-speed 
                 "j" (* -1 @player-speed)
                 (first @displacement))]
      (let [yinc (condp = cha
                 "i" (* -1 @player-speed)
                 "k" @player-speed 
                 (second @displacement))]
        (reset! displacement [xinc yinc])))
    (when (= cha "q") (stop-gameloop))
  )
)

(defn handle-keyrelease [evt]
  (let [cha (str (.getKeyChar evt))]
    (let [xstop (condp = cha 
                 "l" 0 
                 "j" 0
                 (first @displacement))]
      (let [ystop (condp = cha
                   "i" 0 
                   "k" 0
                   (second @displacement)) ]
        (reset! displacement [xstop ystop])))
  )
)


(.setDefaultCloseOperation frame
  javax.swing.WindowConstants/EXIT_ON_CLOSE)
(.setContentPane frame display)
(.addKeyListener frame (proxy [java.awt.event.KeyAdapter] []
                         (keyPressed [e] (handle-keypress e))))
(.addKeyListener frame (proxy [java.awt.event.KeyAdapter] []
                         (keyReleased[e] (handle-keyrelease e))))
(.pack frame)
(.setSize frame width height)
(.setTitle frame (str "Lines Of Death"))
(.show frame)

  
(initialize-game)
(send gamelooper (fn [x] true))
(send-off gamelooper gameloop)

(send weathermanager (fn [x] true))
(send-off weathermanager weather )

(send playermanager (fn [x] true))
(send-off playermanager player)

(send enemymanager (fn [x] true))
(send-off enemymanager enemy)
