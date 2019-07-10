(def ^:const qparam-expr "(?:\\w+\\-*\\w+|\\w+)")

(defn new-pattern [params-string]
  (let [vars-count-in-path (atom 0)]
    (letfn [(%update-counter [num]
              (swap! vars-count-in-path (fn [n] (+ n num))))
            (%count-vars [string]
              (count (re-seq #"\?[^\)/]+" string)))
            (%replace-vars [string & [do-counting]] 
              (let [vars-in-str (%count-vars string)]
                (if (> vars-in-str 0)
                  (do (when do-counting
                          (%update-counter vars-in-str))
                      (clojure.string/replace string #"\?[^\)/]+" "((?:\\\\w+\\\\-*\\\\w+)+|\\\\w+)"))
                  string)))]
      (let [host (second (first (re-seq #"host\(([^\)]+)" params-string)))
            path (second (first (re-seq #"path\(([^\)]+)" params-string)))
            qparams (map second (re-seq #"queryparam\(([^\)]+)" params-string))
            params (mapv second (re-seq  #"\?([^\)/]+)" params-string))
            pathpart (str "(?:https?://)?(?:www\\.)?" (%replace-vars host true) "/" (%replace-vars path true))
            qparams-part (if (empty? qparams)
                             (str "(?:(?:\\?" qparam-expr "=" qparam-expr ")(?:&" qparam-expr "=" qparam-expr ")*)?") 
                             (str "(?:(?:\\?(?:" (clojure.string/join "|" (map %replace-vars qparams)) "|" qparam-expr "=" qparam-expr "))(?:&(?:" (clojure.string/join "|" (map %replace-vars qparams)) "|" qparam-expr "=" qparam-expr "))*)?"))]
[ (re-pattern (str pathpart qparams-part)) params (- (count params) @vars-count-in-path)]))))

(defn recognize [pattern-struct url-string]
  (let [ [pattern params qparams-count] pattern-struct
          matched-exprs (re-matches pattern url-string) ]
    (when (and matched-exprs (<= (count params) (dec (count matched-exprs))))
      (let [ params-count (- (count params) qparams-count)
             path-part (subvec matched-exprs 1 (inc params-count))
             qparams-part (remove nil? (subvec matched-exprs (inc params-count)))]
            (when (= (count qparams-part) qparams-count) 
              (mapv vector (mapv keyword params) (remove nil? (rest matched-exprs))))))))


(def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))
(def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
(def dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))

(defn test [pattern url expected]
  (let [generated (recognize pattern url)]
    (if (= generated expected)
        (println (str "PASSED  " generated))
        (println (str "FAILED, expected: " expected " but got " generated)))))

(test twitter "http://twitter.com/bradfitz/status/562360748727611392" [[:user "bradfitz"] [:id "562360748727611392"] ])
(test dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" [[:id "1905065-Travel-Icons-pack"] [:offset "1"]])
(test dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" nil)
(test dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users" nil)
(test dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?offset=1&list=users" [[:id "1905065-Travel-Icons-pack"] [:offset "1"] [:type "users"] ])
;;(test dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1" [[:id "1905065-Travel-Icons-pack"] [:type "users"] [:offset "1"]])
;; should arguments order matter?
(test dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?offset=1&some=thing&list=users" [[:id "1905065-Travel-Icons-pack"] [:offset "1"] [:type "users"] ])

