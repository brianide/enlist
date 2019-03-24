#!/usr/bin/clojure
(ns hackmud.uploader
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.shell :refer [sh]]))

;;; Utils

(defn wait [s]
  (Thread/sleep (* s 1000)))

(defn find-window [clazz]
  (string/trim
    (:out (sh "xdotool" "search" "--class" clazz))))

(defn send-key! [window k]
  (sh "xdotool" "key" "--window" window k))

(defn send-line! [window text]
  (sh "xdotool" "type" "--window" window text)
  (send-key! window "Return"))

;;; File stuff

(def boilerplate
  "function(c,a){
    let f = {type:'{{type}}', name:'{{name}}'},
        b = {{append}} ? #db.f(f).first().body : '';

    b += `{{body}}`;
    #db.us(f, {$set:{body:b}});
  }")

(defn non-whitespace-count [s]
  (->> s seq (remove #(Character/isWhitespace %)) count))
  
(defn apply-template [s reps]
  (string/replace s #"\{\{([^}]+)}}" #(reps (keyword (% 1)))))

(defn splits [limit s]
  "Produces a sequence of start/end index pairs that divide s into substrings
  containing no more than limit non-whitespace characters."
  (let [sp (->> s
                (map-indexed vector)
                (remove #(Character/isWhitespace (% 1)))
                (partition-all limit)
                (map ffirst)
                (partition 2 1)
                vec)]
    (if (empty? sp)
        [(list 0 (count s))]
        (conj sp (list (last (last sp)) (count s))))))

(defn segments [limit s]
  (map (fn [[b e]] (subs s b e)) (splits limit s)))

(defn random-name []
  (format "hm_%04x" (rand-int 0xFFFF)))

;;; Main program

(def window (find-window "hackmud"))

(defn send-upload-commands! [file]
  (send-line! window (str "#up " file))
  (wait 3)
  (send-line! window file)
  (wait 3)
  (send-line! window (str "#up " file " delete"))
  (wait 2))

(defn main [source remote entry-type char-limit]
  (let [content (slurp source)
        script-name (random-name)
        file-name (str script-name ".js")
        bp-size (non-whitespace-count
                  (apply-template boilerplate {:type entry-type
                                               :name remote
                                               :body ""
                                               :append "false"}))
        limit (- (Integer/parseInt char-limit) bp-size)
        segs (map-indexed vector (segments limit content))
        msg (format "Uploading %s to %s in %d segments" source remote (count segs))]
    (println msg)
    (send-line! window (str "# " msg))
    (run! (fn [[i s]]
            (spit file-name
                  (apply-template boilerplate {:type entry-type
                                               :name remote
                                               :body s
                                               :append (str (> i 0))}))
            (send-upload-commands! script-name))
          segs)
    (io/delete-file file-name))
    (shutdown-agents))

(apply main *command-line-args*)
