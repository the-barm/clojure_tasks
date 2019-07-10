;; Average rating for similar_books is bugged: it shows the same number for all books so I've chosen another strategy: taking up to 20 most rated 'read' books and getting similar from them
;; Also there is an assumption that currently-reading shelf contains no more than 20 books (I would not let this go to prod in this way ofc)
;; Limit is 1req/sec so i don't want to iterate over all pages of 'currently-reading' (it's enough that it takes 20 sec to iterate over 20 books in 'read')
;; Also bad idea to store key in source code like this
;; Collaborative filtering for recommendations is problematic since we don't know how the user rated the books (at least from shelf API)
(ns goodreads.core
  (:gen-class)
  (:require [clojure.tools.cli :as cli]
            [manifold.deferred :as d])
  (:use
   clj-xpath.core))

(def ^:const api-key "7wGDWh6qvpR6UIGo5iJldw")

(defn get-book-info-url [book-id]
  (str "https://www.goodreads.com/book/show.xml?key=" api-key "&id=" book-id))

(defn get-read-shelf-url [user-id]
  (str "https://www.goodreads.com/review/list?key=" api-key "&id=" user-id "&shelf=read&sort=avg_rating&order=d"))

(defn get-curr-reading-shelf-url [user-id]
  (str "https://www.goodreads.com/review/list?key=" api-key "&id=" user-id "&shelf=currently-reading"))

(def make-api-call (memoize slurp))

(def xmldoc
     (memoize (fn [url] (xml->doc (make-api-call url)))))

(defn build-recommendations [config]
  (d/success-deferred (let [currently-reading-shelf (future (xmldoc (get-curr-reading-shelf-url (:token config))))
        number-of-books (:number-books config)
        currently-reading-names ($x:text* "//books/book/title" @currently-reading-shelf)
        read-books-xml (atom [])
        similar-books (atom [])
        iter (atom 0)]
    (Thread/sleep 1000)
    (let [read-shelf (future (xmldoc (get-read-shelf-url (:token config))))]
      (doseq [book ($x:text* "//books/book/id" @read-shelf)]
        (Thread/sleep 1000)
        ;; For safety we could've added there a future, but since we are waiting 1sec anyways that would be an overkill
        (swap! read-books-xml (fn [n] (conj n (xmldoc (get-book-info-url book))))))
      (while (and (< @iter (count @read-books-xml))
                  (< (count @similar-books) number-of-books))
        (do (swap! similar-books
                   (fn [n]
                     (vec
                      (remove (fn [item] (some #(= item %) currently-reading-names))
                              (distinct
                               (into n (mapv
                                        (fn [title authors link] {:title title :authors authors :link link})
                                        ($x:text* "//similar_books/book/title" (nth @read-books-xml @iter))
                                        (mapv (fn [item] (vec ($x:text* "./author/name" item))) ($x:node* "//similar_books/book/authors"
                                                                                                          (nth @read-books-xml @iter)))
                                        ($x:text* "//similar_books/book/link" (nth @read-books-xml @iter))))))))) 
            (swap! iter inc)))
      (swap! similar-books (fn [n] (shuffle n)))
      (if (<= (count @similar-books) number-of-books)
        @similar-books
        (subvec @similar-books 0 number-of-books))))))

(def cli-options [["-t"
                   "--timeout-ms MS"
                   "Wait before finished"
                   :default 30000
                   :parse-fn #(Integer/parseInt %)]
                  ["-n"
                   "--number-books NUMBER"
                   "How many books do you want to recommend"
                   :default 10
                   :parse-fn #(Integer/parseInt %)]
                  ["-h" "--help"]])

(defn book->str [{:keys [title link authors]}]
  (format "\"%s\" by %s\nMore: %s"
          title
          (->> authors
               (map :name)
               (clojure.string/join ", "))
          link))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary))
      (some? errors) (do (println errors))
      (empty? arguments) (do (println "Please, specify user's token"))      
      :else (let [config {:token (first arguments)
                          :number-books (:number-books options)}
                  books (-> (build-recommendations config)
                            (d/timeout! (:timeout-ms options) ::timeout)
                            deref)]
              (cond
                (= ::timeout books) (println "Not enough time :(")                
                (empty? books) (println "Nothing found, leave me alone :(")
                :else (doseq [[i book] (map-indexed vector books)]
                        (println (str "#" (inc i)))
                        (println (book->str book))
                        (println)))))))
