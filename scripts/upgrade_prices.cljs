;;; Requires
;;;  ug: #s.sys.upgrades
;;;  dd: #s.dd.lib
;;;  sl: #s.scripts.lib

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Library functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def browse ('browse ($scr 'dd))) ; Wrapper for market.browse
(def gc-str ('to_gc_str ($scr 'sl)))

(defn ->gc-str [n]
  (if (zero? n)
    "N/A"
    (gc-str n)))

;;;;;;;;;;;;;;;;;;
;;; Statistics ;;;
;;;;;;;;;;;;;;;;;;

(defn mean [coll]
  (if (empty? coll) 0 (/ (reduce + 0 coll) (count coll))))

(defn trimmed-mean [prop coll]
  (let [dis (* prop (count coll))
        ret (* (- 1 (* 2 prop)) (count coll))]
    (mean (take ret (drop dis coll)))))

;;;;;;;;;;;;;;;;;;;;
;;; Main program ;;;
;;;;;;;;;;;;;;;;;;;;

(def columns '[i rarity name cost loaded])

(let [ugs (->> ($scr 'ug {'full 1})
               (map (fn [x] (update x 'rarity rarity->number)))
               (map
                 (fn [x]
                   {'i      (str ('i x))
                    'rarity (str ('rarity x))
                    'name   (color ('rarity x) ('name x))
                    'cost   (->> (browse {'name ('name x) 'rarity ('rarity x)})
                                 (map 'cost)
                                 (trimmed-mean 0.05)
                                 floor
                                 ->gc-str)
                    'loaded (if ('loaded x) "LOAD" "")}))
               (map (apply juxt columns)))
      col-max (->> (map (fn [x] (map count x)) ugs)
                   (reduce (fn [a v] (map max a v)) (repeat (count columns) 0)))]
  (->> ugs
       (map (fn [x] (join " " (map pad-start x col-max))))
       (map println)))

(println "\nFinished in" (- ($new $Date) $ST) "milliseconds")
