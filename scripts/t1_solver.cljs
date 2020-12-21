;;;;;;;;;;;;;;;;;
;;; Lock data ;;;
;;;;;;;;;;;;;;;;;

;; Prime sieve
(defn primes-to [limit]
  (let [lim-rt ($call $Math 'sqrt limit)]
    (loop [primes ()
           [head & more] (range 2 limit)]
      (if (or (nil? head) (> head lim-rt))
        (concat primes head more)
        (recur
          (conj primes head)
          (remove (fn [x] (zero? (mod x head))) more))))))

(def ez-commands '[open release unlock])
(def ez-primes (primes-to 100))
(def k3ys '[6hh8xw cmppiq sa23uw tvfkyq uphlaw vc2c7q xwz7ja])

(def colors
  (let [base '[red purple blue cyan green lime yellow orange red]
        num (count base)
        comple (->> (range num)
                    (map (fn [x] (mod (+ x 4) num)))
                    (map base))
        triad1 (->> (range num)
                    (map (fn [x] (mod (+ x 3) num)))
                    (map base))
        triad2 (->> (range num)
                    (map (fn [x] (mod (+ x 5) num)))
                    (map base))]
    (map list base comple triad1 triad2)))

(def data-check
  {"\\"did you know\\" is a communication pattern common to user ++++++"
   'fran_lee
   "a ++++++ is a household cleaning device with a rudimentary networked sentience"
   'robovac
   "according to trust, ++++++ is more than just following directives"
   'sentience
   "communications issued by user ++++++ demonstrate structural patterns associated with humor"
   'sans_comedy
   "in trust's vLAN, you became one of angie's ++++++"
   'angels
   "in trust's vLAN, you became one of mallory's ++++++"
   'minions
   "in trust's vLAN, you discovered that mallory and che are ++++++"
   'sisters
   "in trust's vLAN, you encountered the will of ++++++, the prover"
   'petra
   "in trust's vLAN, you visited faythe's ++++++"
   'fountain
   "in trust's vLAN, you were required to hack halperyon.++++++"
   'helpdesk
   "pet, pest, plague and meme are accurate descriptors of the ++++++"
   'bunnybat
   "safety depends on the use of scripts.++++++"
   'get_level
   "service ++++++ provides atmospheric updates via the port epoch environment"
   'weathernet
   "this fact checking process is a function of ++++++, the monitor"
   'eve
   "trust's vLAN emphasized the importance of the transfer and capture of ++++++"
   'resource
   "trust's vLAN presented a version of angie who had lost a friend called ++++++"
   'bo
   "user 'on_th3_1ntern3ts' has ++++++ many things"
   'heard
   "user ++++++ provides instruction via script"
   'teach
   "user ++++++ uses the port epoch environment to request gc"
   'outta_juice
   "users gather in channel CAFE to share ++++++"
   'poetry})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lock state transition functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn find-next [prev stack]
  (first
    (drop-while
      (fn [x] (<= x prev))
      stack)))

(defn find-color [prev]
  (if (nil? prev)
    (first colors)
    (second
      (drop-while
        (fn [[x _ _ _]] (not= x prev))
        colors))))

(defn next-command [lockname args]
  {lockname (find-next (lockname args) ez-commands)})

(defn next-digit [args]
  {'digit (find-next ('digit args) (range 10))})

(defn next-prime [args]
  {'ez_prime (find-next ('ez_prime args) ez-primes)})

(defn next-k3y [args]
  {'l0cket (find-next ('l0cket args) k3ys)})

(defn next-c001 [args]
  (let [color (first (find-color ('c001 args)))
        digit (count color)]
    {'c001 color, 'color_digit digit}))

(defn next-c002 [args]
  (let [color (find-color ('c002 args))]
    (zipmap '[c002 c002_complement] (take 2 color))))

(defn next-c003 [args]
  (let [color (find-color ('c003 args))]
    (zipmap '[c003 c003_triad_1 c003_triad_2] (map color '[0 2 3]))))

(defn next-c00x [lockname args]
  ((case lockname
      'c001 next-c001
      'c002 next-c002
      'c003 next-c003)
   args))

(defn solve-data-check [resp]
  {'DATA_CHECK (join (map data-check (split resp "\n")))})

;;;;;;;;;;;;;;;;;;;;
;;; Main program ;;;
;;;;;;;;;;;;;;;;;;;;

(defn format-params [p]
  (str "{" (join ", " (map (fn [[k v]] (str k ":" (prn-str v))) (seq p))) "}"))

(defn crack [scr]
  (loop [params {'DATA_CHECK ""}
         prev-lock 'DATA_CHECK]
    (let [response ($call scr 'call params)
          match (re-find
                  r"EZ_..|command|digit|prime|c00.|color|l0cket|k3y|\\+\\+\\+\\+\\+\\+"
                  (last (split response "\n")))]
      (condp (fn [c e] (c e)) match
        '#{EZ_21 EZ_35 EZ_40} (recur
                                (merge params (next-command match params))
                                match)
        '#{command} (recur
                      (merge params (next-command prev-lock params))
                      prev-lock)
        '#{digit} (recur
                    (merge params (next-digit params))
                    match)
        '#{prime} (recur
                    (merge params (next-prime params))
                    match)
        '#{c001 c002 c003} (recur
                             (merge params (next-c00x match params))
                             match)
        '#{color} (recur
                    (merge params (next-c00x prev-lock params))
                    prev-lock)
        '#{l0cket k3y} (recur
                         (merge params (next-k3y params))
                         match)
        '#{++++++} (recur
                     (merge params (solve-data-check response))
                     match)
        (do
          (println (format-params params))
          (println "Completed in" (- ($new $Date) $ST) "milliseconds")
          (println)
          (print response))))))
