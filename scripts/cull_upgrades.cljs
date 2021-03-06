;;; Requires
;;;  ug:  #s.sys.upgrades
;;;  cul: #s.sys.cull
;;;  bl:  #s.dd.lib

(def browse ('browse ($scr 'bl)))

(defn important? [x]
  (or (contains? x 'k3y) (x 'loaded)))

(defn market-data [x]
  (browse {'name   ('name x)
           'rarity (rarity->number ('rarity x))}))

(defn mean [xs]
  (/ (reduce + 0 xs) (count xs)))

(defn average-price [x]
  (->> (browse {'name ('name x)
                'rarity (rarity->number ('rarity x))})
       (take 10)
       (map 'cost)
       mean))

(->> ($scr 'ug {'full true})
     (remove important?)
     (map (fn [x] (merge (select-keys x '(name rarity i))
                         {'cost (average-price x)})))
     (filter (fn [x] (< ('cost x) ('min $args))))
     (map 'i)
     (hash-map 'i)
     (merge {'confirm true})
     ($scr 'cul))
