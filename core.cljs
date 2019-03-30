(defmacro defn [n b & fs] ^(def ~n (fn ~b ~@fs)))

(defn str [& s] (if s ($call s 'join "") ""))

(defn deref [a] ('__a a))
(defn reset! [a v] ($set a '__a v))
(defn swap! [a f & xs] ($set a '__a (apply f (cons @a xs))))

(defn identity [a] a)
(defn complement [p] (fn [& args] (not (apply p args))))
(defn partial [f & xs] (fn [& ys] (apply f (concat xs ys))))

(defn count [c]
  (if (nil? c) 0
    (if (map? c) (count (keys c))
      ($get c 'length))))

(defn empty? [coll] ($exact 0 (count coll)))

(defn boolean [a] (if a true false))
(defn true? [a] ($exact a true))
(defn false? [a] ($exact a false))
(defn nil? [a] (if ($exact a false) false ($eq a nil)))
(def some? (complement nil?))

(defn even? [n] ($exact 0 (mod n 2)))
(defn odd? [n] ($exact 1 (mod n 2)))
(defn pos? [n] (> n 0))
(defn neg? [n] (< n 0))
(defn zero? [n] (== 0 n))
(defn inc [n] (+ n 1))
(defn dec [n] (- n 1))
(defn negate [n] (- 0 n))

(defn type [a] (if a ('constructor a) nil))
(defn has-constructor? [con a] (if a ($exact con ('constructor a)) false))
(def string? (partial has-constructor? $String))
(def number? (partial has-constructor? $Number))
(def seq? ('isArray $Array))
(def map? (partial has-constructor? $Object))
(def set? (partial has-constructor? $Set))
(def regex? (partial has-constructor? $RegExp))
(defn boolean? [a] (if (not (nil? a)) ($exact $Boolean ('constructor a)) false))
(def function? (partial has-constructor? $Function))
(defn macro? [a] (if a ('__M a) false))
(defn atom? [a] (if a ('__A a) false))

(defn nth [n coll] (if (some? coll) ($get coll n)))
(defn first [coll] (nth 0 coll))
(defn ffirst [coll] (first (first coll)))
(defn second [coll] (nth 1 coll))
(defn last [coll] (nth (dec (count coll)) coll))
(defn butlast [coll] (if (some? coll) ($call coll 'slice 0 (dec (count coll)))))
(defn take [n coll] (if (some? coll) ($call coll 'slice 0 n)))
(defn drop [n coll] (if (some? coll) ($call coll 'slice n)))
(def rest (partial drop 1))

(defn drop-while [f coll]
  (if (some? coll)
    (let [cf (complement f)
          index ($call coll 'findIndex cf)]
      (if (neg? index) () ($call coll 'slice index)))))

(def gensym
  (let [cnt (atom 0)]
    (fn [a]
      (swap! cnt inc)
      (str "__" (if a a "g") "_" @cnt))))

(defmacro or [& xs]
  (if (empty? xs)
    nil
    (if ($exact 1 (count xs))
      (first xs)
      ^(let [cvar# ~(first xs)]
         (if cvar# cvar# (or ~@(rest xs)))))))

(defmacro and [& xs]
  (if (empty? xs)
    nil
    (if ($exact 1 (count xs))
      (first xs)
      ^(let [cvar# ~(first xs)]
         (if cvar# (and ~@(rest xs)) cvar#)))))
           
(defmacro if-let [b t f] ^(let ~b (if ~(first b) ~t ~f)))
(defmacro unless [b f t] ^(if ~b ~t ~f))
(defmacro when [p & fs] ^(if ~p (do ~@fs)))
(defmacro when-not [p & fs] ^(if ~p nil (do ~@fs)))
(defmacro when-let [b & fs] ^(let ~b (if ~(first b) (do ~@fs))))

(defmacro case [exp & cs]
  (let [defl (if (odd? (count cs)) (last cs))
        cs (partition 2 (if defl (butlast cs) cs))]
    ^(let [exp# ~exp]
       ~(reduce-right
          (fn [a [tconst res]]
            (conj ^(if (= exp# ~tconst) ~res) a))
          defl
          cs))))

(defmacro cond [& ps]
  (if (empty? ps)
    nil
    ^(if ~(first ps) ~(second ps) (cond ~@(drop 2 ps)))))

(defmacro condp [p exp & ts]
  (let [init (if (odd? (count ts)) (last ts))
        ts (if init (butlast ts) ts)
        ts (partition 2 ts)]
    ^(let [pred# ~p expr# ~exp]
       ~(reduce-right
          (fn [a [texp res]]
            (conj ^(if (pred# ~texp expr#) ~res) a))
          init
          ts))))

(defmacro defa [n & fs]
  (let [atab (list)
        vari (list)]
    (run! (fn [fspec]
            (let [args (first fspec)
                  fun  (cons 'fn fspec)
                  variadic (= '& (last (butlast args)))
                  arity    (- (count args) (if variadic 1 0))]
              (if variadic
                (if (empty? vari)
                  (push! vari arity fun)
                  (throw (str "Multiple variadics (" n ")")))
                (if ($in arity atab)
                  (throw (str "Conflicting arities (" n ")"))
                  ($set atab arity fun)))))
          fs)
    (if (and (not-empty vari) (> (count atab) (first vari)))
      (throw (str "Arity conflicts with variadic (" n ")")))
      ^(let [atab# ~(cons 'list atab) ~@(if (empty? vari) () (list ^vari# (second vari)))]
         (def ~n (fn [& args]
                   (if-let [fun# (atab# (count args))]
                     (apply fun# args)
                     ~(if (empty? vari)
                        ^(throw (str "No matching arity (" '~n ")"))
                        ^(if (>= (inc (count args)) ~(first vari))
                           (apply vari# args)
                           (throw (str "No matching arity (" '~n ")"))))))))))

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

(defn == [a b]
  ($exact ($val a) ($val b)))

(defn = [a b]
  (cond
    (string? a) (if (string? b)
                    (== a b))
    (nil? a)    (nil? b)
    (seq? a)    (and
                  (seq? b)
                  ($exact (count a) (count b))
                  (every? (fn [x] (= (nth x a) (nth x b))) (range 0 (count a))))
    (map? a)    (and
                  (= (sort (keys a)) (sort (keys b)))
                  (every? (fn [x] (= (get a x) (get b x))) (keys a)))
    true        ($exact a b)))

(def not= (complement =))
(def not-empty (complement empty?))

(def > $gt)
(def < $lt)
(defn >= [a b] (or (> a b) (= a b)))
(defn <= [a b] (or (< a b) (= a b)))

(defn map [f coll] ($call coll 'map (fn [x] (f x))))
(defn map-indexed [f coll] ($call coll 'map (fn [x i] (f i x))))
(defn run! [f coll] ($call coll 'forEach (fn [x] (f x))))
(defn filter [p coll] ($call coll 'filter (fn [x] (p x))))
(defn remove [p coll] (filter (complement p) coll))
(defn every? [p coll] ($call coll 'every (fn [x] (p x))))
(defn some [p coll] ($call coll 'some (fn [x] (p x))))
(defn find [p coll] (if (some? coll) ($call coll 'find (fn [x] (p x)))))
(defn split-at [n coll] (list (take n coll) (drop n coll)))
(defn reverse [coll] (if (some? coll) ($call ($call coll 'slice) 'reverse)))

(defa range
  ([lo up] ($call $Array 'from {'length (- up lo)} (fn [x i] (+ lo i))))
  ([up] (range 0 up)))

(defn split [s re] ($call s 'split re))
(defa join ([coll] (join "" coll)) ([sep coll] ($call coll 'join sep)))

(defn comparator [op]
  (fn [a b]
    (let [left (op a b)]
      (cond
        (number? left) left
        (boolean? left) (cond left -1 (op b a) 1 true 0)
        true (throw "Invalid compare function")))))

(defa sort
  ([coll] ($call ($call coll 'slice) 'sort))
  ([cmp coll] ($call ($call coll 'slice) 'sort (comparator cmp))))  

(defa partition
  ([n step coll]
    (loop [acc () coll coll]
      (if (empty? coll)
          acc
          (do (push! acc ($call coll 'slice 0 n))
              (recur acc (drop step coll))))))
  ([n coll] (partition n n coll)))

(defn reduce [f i coll] ($call coll 'reduce (fn [a v] (f a v)) i))
(defn reduce-right [f i coll] ($call coll 'reduceRight (fn [a v] (f a v)) i))

(defn juxt [& fs]
  (fn [& xs] (map (fn [f] (apply f xs)) fs)))

(def max ('max $Math))
(def min ('min $Math))
(def ceil ('ceil $Math))
(def floor ('floor $Math))
(def round ('round $Math))

(def ->json ($get $JSON 'stringify))
(def <-json ($get $JSON 'parse))

(def keys ('keys $Object))
(def vals ('values $Object))
(defn get [m k d] (or ($get m k) d))
(defn contains? [m k] ($in k m))
(defn merge [m n] ($call $Object 'assign {} m n))

(defn assoc [m & kvs]
  (loop [m ($call $Object 'assign {} m)
         kvs kvs]
    (if (not-empty kvs)
        (do 
          ($set m (first kvs) (second kvs))
          (recur m (drop 2 kvs)))
        m)))

(defn dissoc [m & ks]
  (loop [m ($call $Object 'assign {} m)
         ks ks]
    (if (not-empty ks)
        (do 
          ($delete m (first ks))
          (recur m (rest ks)))
        m)))

(defn select-keys [m ks]
  (->> (keys m)
       (remove (fn [x] ($call ks 'includes x)))
       (cons m)
       (apply dissoc)))

(defn update [m k f & xs]
  (assoc m k (apply f (cons (get m k) xs))))

(defn seq [c]
  (if (> (count c) 0)
    (cond
      (seq? c)    c
      (map? c)    (('entries $Object) c)
      (string? c) ($call c 'split ""))))

(defn conj [a & bs]
  (cond
    (nil? a) bs
    (seq? a) (concat a bs)
    (map? a) (loop [a (merge {} a)
                    n (first bs)
                    bs (rest bs)]
               (cond
                 (map? n) ($call $Object 'assign a n)
                 (seq? n) ($set a (first n) (second n)))
               (if (empty? bs) a (recur a (first bs) (rest bs))))))

(defn comp [& fs]
  (fn [& xs]
    (reduce-right (fn [r x] (x r)) xs fs)))

(defn re-groups [reg s]
  (if (not ($get reg 'global))
      ($call reg 'exec s)
      (loop [acc (list)]
        (if-let [m ($call reg 'exec s)]
          (recur (push! acc m)))
          acc)))
  
(defn re-find [reg s]
  (if ($get reg 'global)
    (map first (re-groups reg s))
    (first (re-groups reg s))))

(defn color [c s]
  (str "\x60" c s "\x60"))

(defn uncolor [s]
  ($call s 'replace rg"\x60[0-9A-Za-z](?!:.?\x60)([^\x60\n]+)\x60" '$1))
  
(defn rarity->number [s]
  (number (second s)))

(defn $scr [n & args] (apply ('call ($args n)) args))

;;; Database stuff

(defn list-files []
  (map 'name ($call $db 'f {'type 'lisp_scr})))
