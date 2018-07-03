function(c,a){return #fs.hg.lisp().save('core', `

// SYSTEM //

(defmacro defn [n b & fs] ^(def ~n (fn ~b ~@fs)))

(defn swap! [a f & xs] (reset! a (apply f @a xs)))

(defn boolean [a] (if a true false))

(defn nil? [a] (if ($exact a false) false ($eq a nil)))
(defn seq? [coll] ($call $Array :isArray coll))
(defn string? [a] (and a ($exact $String ($get a :constructor))))
(defn number? [a] (and a ($exact $Number ($get a :constructor))))
(defn boolean? [a] (and (not (nil? a)) ($exact $Boolean ($get a :constructor))))
(defn true? [a] ($exact a true))
(defn false? [a] ($exact a false))
(defn map? [a] (and a ($exact $Object ($get a :constructor))))
(defn function? [a] (and a ($exact $Function ($get a :constructor))))
(defn regex? [a] (and a ($exact $RegExp ($get a :constructor))))

(def gensym
  (let [cnt (atom 0)]
    (fn [a]
      (swap! cnt inc)
      (symbol (str (if a a "G__") @cnt)))))

(defn == [a b]
  ($exact ($valueOf a) ($valueOf b)))

(defn = [a b]
  (cond
    (string? a)   (if (string? b)
                    (== a b))
    (nil? a)      (nil? b)
    (list? a)     (and
                    (list? b)
                    ($exact (count a) (count b))
                    (every? (fn [x] (= (nth x a) (nth x b))) (range 0 (count a))))
    (hash-map? a) (and
                    (= (sort (keys a)) (sort (keys b)))
                    (every? (fn [x] (= (get a x) (get b x))) (keys a)))
    true          ($exact a b)))



// SEQUENCES //

// Higher Order Functions
(defn map [f coll] ($call coll 'map (fn [x] (f x))))
(defn filter [p coll] ($call coll 'filter (fn [x] (p x))))
(defn reduce [f i coll] ($call coll 'reduce (fn [a v] (f a v)) i))
(defn reduce-right [f i coll] ($call coll 'reduceRight (fn [a v] (f a v)) i))
(defn every? [p coll] ($call coll 'every (fn [x] (p x))))
(defn some? [p coll] ($call coll 'some (fn [x] (p x))))

// Elements
(defn nth [n coll] ($get coll n))
(defn first [coll] (nth 0 coll))
(defn second [coll] (nth 1 coll))
(defn last [coll] (nth (dec (count coll)) coll))
(defn take [n coll] ($call coll 'slice 0 n))
(defn drop [n coll] ($call coll 'slice n))
(defn rest [coll] (drop 1 coll))

// Other
(defn list [& xs] xs)
(defn empty? [coll] ($exact 0 (count coll)))
(defn sort [coll] ($call ($call coll 'slice) 'sort))
(defn reverse [coll] ($call ($call coll 'slice) 'reverse))

(defn count [c]
  (if (seq? c)
    ($get c 'length)
    (count (keys c))))

(defn seq [c]
  (if (> (count c) 0)
    (cond
      (seq? c) c
      (map? c) (map (fn [k] [k (get c k)]) (keys c))
      (string? c) ($call c :split ""))))

(defn partition [n coll]
  (if (empty? coll)
    ()  
    (cons (take n coll) (partition n (drop n coll)))))



// MATH //

(defn + [& ns] (reduce $add 0 ns))
(defn - [n & ns] (if (empty? ns) ($sub 0 n) (reduce $sub n ns)))
(defn * [& ns] (reduce $mul 1 ns))
(defn / [n & ns] (if (empty? ns) ($div 1 n) (reduce $div n ns)))

(def max ($get $Math 'max))
(def min ($get $Math 'min))
(def ceil ($get $Math 'ceil))
(def floor ($get $Math 'floor))
(def round ($get $Math 'round))

(defn even? [n] (== 0 (mod n 2)))
(defn odd? [n] (== 1 (mod n 2)))
(defn pos? [n] (> n 0))
(defn neg? [n] (< n 0))
(defn zero? [n] (== 0 n))
(defn inc [n] (+ n 1))
(defn dec [n] (- n 1))

(defn str [& s] (if s ($call s 'join "") ""))

(def write-json ($get $JSON 'stringify))
(def read-json ($get $JSON 'parse))



// HASH-MAPS //

(defn keys [m] ($call $Object 'keys m))
(defn vals [m] ($call $Object 'values m))
(defn get [m k d] (or ($get m k) d))
(defn contains? [m k] ($in m k))

(defn assoc [m k v & kvs]
  (let [m ($call $Object 'assign {} m)]
    ($set m k v)
    (if (empty? kvs)
      m
      (apply assoc m (first kvs) (second kvs) (drop 2 kvs)))))

(defn dissoc [m k v & kvs]
  (let [m ($call $Object 'assign {} m)]
    ($delete m k)
    (if (empty? kvs)
      m
      (apply dissoc m (first kvs) (second kvs) (drop 2 kvs)))))

(defn update [m k f & xs]
  (assoc m k (apply f (get m k) xs)))
        


// LOGIC //

(defmacro or [& xs]
  (if (empty? xs)
    nil
    (if ($exact 1 (count xs))
      (first xs)
      (let [cvar (gensym)]
        ^(let [~cvar ~(first xs)]
           (if ~cvar ~cvar (or ~@(rest xs))))))))

(defmacro and [& xs]
  (if (empty? xs)
    nil
    (if ($exact 1 (count xs))
      (first xs)
      (let [cvar (gensym)]
        ^(let [~cvar ~(first xs)]
           (if ~cvar (and ~@(rest xs)) ~cvar))))))

(def > $gt)
(def < $lt)
(defn >= [a b] (or (> a b) (= a b)))
(defn <= [a b] (or (< a b) (= a b)))



// MACROS //

(defmacro if-let [b t f] ^(let ~b (if ~(first b) ~t ~f)))
(defmacro when [p & fs] ^(if ~p (do ~@fs)))
(defmacro when-not [p & fs] ^(if ~p nil (do ~@fs)))
(defmacro when-let [b & fs] ^(let ~b (if ~(first b) (do ~@fs))))

(defmacro cond [& ps]
  (if (empty? ps)
    nil
    ^(if ~(first ps)
       ~(second ps)
       (cond ~@(drop 2 ps)))))

(defmacro time [exp]
  ^(let [start ($new $Date)
         res ~exp
         end ($new $Date)]
     (prn (str "Time: " (- end start) " msecs"))
     res))

(defmacro -> [v & fs]
  (if (empty? fs)
    v
    (let [f (first fs)
          f (if (seq? f) f (list f))]
      ^(-> (~(first f) ~v ~@(rest f)) ~@(rest fs)))))

(defmacro ->> [v & fs]
  (if (empty? fs)
    v
    (let [f (first fs)
          f (if (seq? f) f (list f))]
      ^(->> (~@f ~v) ~@(rest fs)))))

(defmacro as-> [v n & fs]
  ^(let ~(reduce (fn [a f] (conj a n f)) [n v] fs) ~n))



// Invocables

//(typefn $Object invoke [m k d] (get m k d))
//(typefn $String invoke [k m d] (get m k d))



// MISC

(defn identity [a] a)

(defn comp [& fs]
  (fn [& xs]
    (reduce-right (fn [r x] (apply x r)) xs fs)))

(defn partial [f & xs]
  (fn [& ys]
    (apply f xs ys)))

`)}
