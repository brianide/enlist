;;; Prime sieve
(defn primes-to [limit]
  (let [lim-rt ($call $Math 'sqrt limit)]
    (loop [primes ()
           [head & more] (range 2 limit)]
      (if (or (nil? head) (> head lim-rt))
        (concat primes head more)
        (recur
          (conj primes head)
          (remove (fn [x] (zero? (mod x head))) more))))))

;;; Lock data

(def ez-commands '[open release unlock])
(def ez-primes (primes-to 100))

;;; Functions to determine the next param state from the previous one 

(defn find-next [prev stack]
  (first
    (drop-while
      (fn [x] (<= x prev))
      stack)))

(defn next-command [lockname args]
  {lockname (find-next (lockname args) ez-commands)})

(defn next-digit [args]
  {'digit (find-next ('digit args) (range 10))})

(defn next-prime [args]
  {'ez_prime (find-next ('ez_prime args) ez-primes)})

;;; Main program

(defn crack [scr]
  (loop [params {'EZ_21 'open}
         prev-lock nil]
    (let [response (last (split ($scr scr params) "\n"))
          match (re-find r"EZ_..|command|digit|prime|terminated" response)]
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
        '#{terminated} (do
                         (prn params)
                         (print "All locks completed"))
        (do
          (prn params)
          (print "Encountered unknown lock" match))))))
