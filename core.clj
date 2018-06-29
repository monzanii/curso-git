(ns full.core
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.repl :refer :all]
    #_[clojure.repl :refer [doc]]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(foo "fdlkfj")

(s/conform even? 1000)

(s/valid? even? 1001)

(s/valid? nil? nil)


(s/valid? #(> % 5) 10)

(s/valid? #(> % 5) 6)



(import java.util.Date)
(s/valid? inst? (Date.))

(s/valid? #{:club :diamond :heart :spade} :club)

(s/valid? #{:club :diamond :heart :spade} 42)

(s/valid? #{42} 42)
;; true

(s/def ::date inst?)

(s/valid? ::date (Date.))

(s/def ::suit #{:club :diamond :heart :spade})
(s/conform ::suit 40)

(comment
  (doc reduce)

  (s/def ::big-even (s/and int? even? #(> % 1000)))
  (s/valid? ::big-even :foo)                                ;; false
  (s/valid? ::big-even 10)                                  ;; false
  (s/valid? ::big-even 100000)                              ;; true

  (s/def ::name-or-id (s/or :name string?
                            :id int?))
  (s/valid? ::name-or-id "abc")                             ;; true
  (s/valid? ::name-or-id 100)                               ;; true
  (s/valid? ::name-or-id :foo)                              ;; false

  (s/conform ::name-or-id "abc")
  ;;=> [:name "abc"]
  (s/conform ::name-or-id 100)
  ;;=> [:id 100]
  (s/conform ::name-or-id :foo)

  (s/explain ::suit #_40 :foo)

  (s/explain-str ::big-even 10)

  (s/explain ::name-or-id "abc")

  (def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
  (s/def ::email-type (s/and string? #(re-matches email-regex %)))

  (s/def ::acctid int?)
  (s/def ::first-name string?)
  (s/def ::last-name string?)
  (s/def ::email ::email-type)

  (s/def ::person (s/keys :req [::first-name ::last-name ::email]
                          :opt [::phone]))


  (s/valid? ::person
            {::first-name "Elon"
             ::last-name  "Musk"
             ::email      "elon@example.com"})
  ;;=> true

  ;; Fails required key check
  (s/explain ::person
             {::first-name "Elon"})


  (doc s/merge)
  )

(comment
  (s/def :animal/kind string?)
  (s/def :animal/says string?)
  (s/def :animal/common (s/keys :req [:animal/kind :animal/says]))
  (s/def :dog/tail? boolean?)
  (s/def :dog/breed string?)
  (s/def :animal/dog (s/merge :animal/common
                              (s/keys :req [:animal/common :dog/tail? :dog/breed])))
  (s/valid? :animal/dog
            {:animal/kind "dog"
             :animal/says "woof"
             :dog/tail?   true
             :dog/breed   "retriever"})


  (s/def ::ingredient (s/cat :quantity number? :unit keyword?))
  (s/conform ::ingredient [2 :mirian])
  (o que o :teaspoon está esperando para ser validado)


  (s/explain ::ingredient [11 :w])


  (s/def ::seq-of-keywords (s/* keyword?))


  (s/conform ::seq-of-keywords [:a :b :c])

  (s/explain ::seq-of-keywords [22 10 23])


  (s/def ::odds-then-maybe-even (s/cat :odds (s/+ odd?)
                                       :even (s/? even?)))

  (s/valid? ::odds-then-maybe-even [1 3 7 10])

  (s/def ::opts (s/cat :opt keyword?
                       :val boolean?))

  (s/explain ::opts [:mirian? true])

  (s/conform ::opts [:silent? false #_#_:verbose true])



  (s/def ::config (s/*
                    (s/cat :prop string?
                           :val (s/alt :s string? :b boolean?))))

  (s/explain ::config ["dskfkf" true "sdkdjf" "essestring"])
  (s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])

  (s/def ::even-strings (s/&
                          (s/* string?) #(even? (count %))))

  (s/explain ::even-strings ["ad" "sldkf" "dsdkfp" "23"])

  (defn person-name
    [person]
    {:pre  [(s/valid? ::person person)]
     :post [(s/valid? string? %)]}
    (str (::first-name person) " " (::last-name person) " " (::email person) " "
         (::qq-coisa person)))

  (person-name 42)
  ;;=> java.lang.AssertionError: Assert failed: (s/valid? :my.domain/person person)

  (person-name {::first-name "Elon" ::last-name "Musk" ::email "elon@example.com"})
  ;; Elon Musk


  (defn person-name [person]
    {:pre  [(s/valid? ::person person)]
     :post [(s/valid? string? %)]}
    (str (::first-name person) " " (::last-name person)))

  (person-name 42)

  (person-name {::first-name "Data" ::last-name "Mast" ::email "elon@example.com"
                ::qq-coisa   "dskfj"})

  (person-name "Data")

  (person-name {::first-name "Elon" ::last-name "Musk" ::email "elon@example.com"})


  (defn adder [x] #(+ x %))

  (s/fdef adder
          :args (s/cat :x number?)
          :ret (s/fspec :args (s/cat :y number?) :ret number?)
          :fn #(= (-> % :args :x) ((:ret %) 0)))
  )


(def suit? #{:club :diamond :heart :spade})
(def rank? (into #{:jack :queen :king :ace} (range 2 11)))
(def deck (for [suit suit? rank rank?] [rank suit]))

(s/def ::card (s/tuple rank? suit?))
(s/def ::hand (s/* ::card))

(s/def ::name string?)
(s/def ::score int?)
(s/def ::player (s/keys :req [::name ::score ::hand]))

(s/def ::players (s/* ::player))
(s/def ::deck (s/* ::card))
(s/def ::game (s/keys :req [::players ::deck]))



(def kenny
  {::name  "Kenny Rogers"
   ::score 100
   ::hand  []})

(s/valid? ::player kenny)


(s/explain ::game
           {::deck    deck
            ::players [{::name  "Kenny Rogers"
                        ::score 100
                        ::hand  [[2 :banana]]}]})


(defn total-cards [{:keys [::deck ::players] :as game}]
  (apply + (count deck)
         (map #(-> % ::hand count) players)))



(macroexpand-1 '(-> 12 (inc) (-3) (-2)))

(+ 1 1)

(re-seq #"\w+" "funciona para numeros?")


(defn rev-char [x] (reverse x))
(defn sep-char [x] (re-seq #"\w+" x))
(defn ap-str [x] (apply str x))
(ap-str "mirian")
(re-seq #"\w+" "funciona para numeros?")
(re-seq "mirian")

(re-seq #"\w+" "funciona para numeros?")

(sep-char "funcion para ")


(str/join " " (map ap-str (map rev-char (re-seq #"\w+" "Oi Mirian \n"))))

(map println (map exec-1 '("oi Mirian \n" "\n mais um argumento")))
(defn exec-1 [%] (str/join " " (map ap-str (map rev-char (sep-char %)))))


(defn greeting [x] (map exec-1 [x]))


(map sep-char (greeting msg))
(map sep-char (map (greeting msg) "\n"))

(def msg "oi Mirian\n tudo bem?\n")



(into [] (take 5 (iterate dec 5)))


(def numb-str "1234")

(sep-char numb)
(ap-str numb)
(rev-char numb)

(map inc '(1 2 3))

(sep-char numb)

(map str (str/join " " (reverse numb-str)))
(map str (str/join numb-str))

(str/join numb-str)



;;parse int funcionando
(map (fn [x] (. Integer parseInt x)) (map str (str/join numb-str)))

(defn parse [x] (. Integer parseInt x))
(defn list-numb [x] (map parse (map str (str/join x))))
(defn list-even [x] (keep-indexed #(if (odd? %1) %2) (list-numb x)))
(defn redu [x] (reduce x))


;;(filter (keep-indexed (even?  (list-numb "1234"))))
;;(keep-indexed #(if (even? %1) %2) [(list-numb "1234")])
;;(map (filter odd? (keep-indexed #(if (even? %1) %2) [(list-numb "1234")])))

;;(map (fn [x] filter odd? ) (keep-indexed #(if (even? %1) %2) [(list-numb "1234")]))
;;(keep-indexed #(if (even? %1) %2) (list-numb "123456"))

(reduce + (map (fn [x] (* x 2)) (list-even "1234567")))
(map (fn [x] (* x 2)) (list-even "12345678"))
(- 10 (rem (reduce + (map (fn [x] (* x 2)) (list-even "12345678"))) 10))
;;fazer funcao do if
(- 10 (rem (reduce + (map #(if (> (fn [x] (reduce + (list-even x)))) x)
                          (map (fn [x] (* x 2))
                               (list-even "1234567"))) 10)))


(- 10 (rem 24 10))

(keep-indexed #(if (odd? %1) %2) [:a :b :c :d :e])

(keep-indexed #(if (even? %1) %2) [:a :b :c :d :e])
(keep-indexed #(if (even? %1) %2) [2 3 4 5])

(keep-indexed (odd? [1 2 3]))

(filter (keep-indexed (odd? [1 2 3])))
(filter odd? [1 2 3])
(keep-indexed #(map odd? [1 2 3]))

(filter #(> (count %) 2) (str/split ​ "A fine day it is" ​ #​"\W+" ​))


(doc index)


(map sep-char [numb])

(def foo (shuffle (range 10)))
(def foo '(1 2 3 4 5 6))
(position)

(pos c [a b c d e f g])

(​defn​ indexed [coll] (map-indexed vector coll))

(indexed ​ "abcde" ​)

(​defn​ index-filter [pred coll]
        ​ (when pred
            ​ (​for​ [[idx elt] (indexed coll) :when (pred elt)] idx)))

(​defn​ new-progress [word]
        (repeat (count word) ​ \_​))

(defn x-ind [word] (repeat (count word) \_))

(+ 2 2)

([:a :b :c] 1)
(assoc [0 1 2 3 4 5] 2 :three)

(subvec [0 1 2 3] 3)

(keys {:sundance "spaniel", :darwin "beagle"})
(vals {:sundance "spaniel", :darwin "beagle"})
(:darwin {:sundance "spaniel", :darwin "beagle"})

(completa-campo "mirian")
(defn completa-campo [c] (str/join (concat c (repeat 5 "*"))))
;;(map #(if (= % (key :sundance) ) (completa-campo % ) (completa-campo %)) {:sundance "spaniel", :darwin "beagle"})

(map #(when (= (keys %) :sundance)
        (completa-campo %)) {:sundance "spaniel", :darwin "beagle"})

(def map1 {:sundance "spaniel", :darwin "beagle"})
(def map2 {{:sundance "spaniel"} {:darwin "beagle"}})
(def map3 [:sundance "spaniel" :darwin "beagle"] )

(map #(if (= (vals %) "spaniel")
        (completa-campo %) (completa-campo (rest %)))
     map1)

(map #(completa-campo (rest %))
     map1)

(defn val-map [m]
  (map #(rest %)
     map1))

  (map #(rest %)
     map1)

(defn ver-val [v]
  (if (= v "spaniel") (completa-campo v) v))

(defn ver-val1 [v]
  (if (= (vals v) "spaniel") (completa-campo v) v))

(defn map-vals [m f]
  (loop [acc {} xs (seq m)]
    (if (empty? xs)
      acc
      (let [x (first xs)
            k (first x)
            v (second x)]

        (recur (assoc acc k (f v)) (rest xs))
        ;;  (recur (assoc acc k v) (rest xs))
        ))))

(map-vals map1 ver-val )

(defn map-vals [m f]
  (map f m ))


(map-vals map3 ver-val)
(apply hash-map (map-vals map3 ver-val))
(str/join (vals (apply hash-map (map-vals map3 ver-val))))




;;(reduce + (map (fn [x] (* x 2)) (list-even "1234567")))






;;(str/join (concat (repeat 10 "1")  (repeat 5 "*")))


(:darwin [:sundance "spaniel", :darwin "beagle"])

(def score {:stu nil :joey 100})

(:stu score)
(:joey score)

(contains? score :stu)

(​def​ compositions
       ​ #{{:name ​ "The Art of the Fugue" ​ :composer ​ "J. S. Bach" ​}
           ​ {:name ​ "Musical Offering" ​ :composer ​ "J. S. Bach" ​}
           ​ {:name ​ "Requiem" ​ :composer ​ "Giuseppe Verdi" ​}
           ​ {:name ​ "Requiem" ​ :composer ​ "W. A. Mozart" ​}})
​ (​def​ composers
         ​ #{{:composer ​ "J. S. Bach" ​ :country ​ "Germany" ​}
             ​ {:composer ​ "W. A. Mozart" ​ :country ​ "Austria" ​}
             ​ {:composer ​ "Giuseppe Verdi" ​ :country ​ "Italy" ​}})
​ (​def​ nations
         ​ #{{:nation ​ "Germany" ​ :language ​ "German" ​}
             ​ {:nation ​ "Austria" ​ :language ​ "German" ​}
             ​ {:nation ​ "Italy" ​ :language ​ "Italian" ​}})

(conj [3 4 5] 1)

(+ 1 2)


(def strip "32401977010912345678")
(defn parseChar [c] (- (int c) (int \0)))
(def strip-int (map parseChar strip))
(def strip-calc2 (map * strip-int (cycle [1 2])))

(map * strip-int (cycle [1 2]))

(Math/abs (- (rem (reduce + strip-calc2) 10) 10))

;;;;;;(map strip-dec (map * strip-int (cycle [1 2])))
(defn strip-dec [x] (if (> 9) (reduce + (map parseChar (str x))) x))
(def strip-calc3 (map strip-dec (map * strip-int (cycle [1 2]))))
(Math/abs (- (rem (reduce + strip-calc3) 10) 10))

;; está completamente doido

(Math/exp 2)

(Math/sqrt 64)
(Math/pow 8 2)
(min 5 3)
(list1 2)

(def list1 [10 -1 9])
(def list2 [20 1 1])
(def list3 [30 7 7])
(def list4 [40 4 4])

(defn subX-SubY [l1 l2 x] (- (l1 x) (l2 x)))
(defn quad [l-dest x] (Math/pow (subX-SubY list1 l-dest x) 2))
(defn DM [l-dest] (Math/sqrt (+ (quad l-dest 1) (quad l-dest 2))))
(defn DMS [l] (DM l))
(sort (conj [(DMS list2) (DMS list3) (DMS list4) (DMS list1)]))

;;(min (DM list2) (DM list3))
;;(println (DM list2) (DM list3))
;;(DMS list2)



(defn d [[x1 y1] [x2 y2]] (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                                        (Math/pow (- y1 y2) 2))))

(d [1 1] [4 5])

(def users [[:a -1 9]
            [:b 1 1]
            [:c 7 7]
            [:d 4 4]])

(defn calc-distance [users pt]
  (map #(vector (first %) (d (rest %) pt)) users))

(sort-by second (calc-distance users [0 -3]))





















