(ns func-lab-2.core)

; 1 задача
(defprotocol relations-protocol
  (find-parents [this child-name])
  (find-children [this name])
  (has-children? [this name])
  (parents-to-children [this]))

(defrecord ParentRelation [name child])
(defrecord db-prs [relations]
  relations-protocol
  (find-parents [_ child-name]
    (let [filtered (filter #(= child-name (-> % :child)) relations)]
      (reduce #(conj %1 (-> %2 :name)) [] filtered)))
  (find-children [_ name]
    (let [filtered (filter #(= name (-> % :name)) relations)]
      (reduce #(conj %1 (-> %2 :child)) [] filtered)))
  (has-children? [_ name]
    (->> relations
         (some #(= name (-> % :name)))
         (boolean)))
  (parents-to-children [this]
    (let [parents (distinct (reduce #(conj %1 (-> %2 :name)) [] relations))]
      (map #(hash-map :children (find-children this %1) :name %1) parents))))

(def prs (db-prs. [(ParentRelation. "Полина" "Борис")
                   (ParentRelation. "Анатолий" "Борис")
                   (ParentRelation. "Анатолий" "Лиза")
                   (ParentRelation. "Борис" "Катя")
                   (ParentRelation. "Борис" "Валентина")
                   (ParentRelation. "Полина" "Евгений")]))

(find-parents prs "Катя")
(has-children? prs "Лиза")
(find-children prs "Борис")
(parents-to-children prs)

; 8 задача
(defprotocol shifts-protocol
  (find-by-name [this name])
  (knows? [this name1 name2])
  (knows [this name]))

(defrecord ShiftRelation [name shift])
(defrecord db-shs [shifts]
  shifts-protocol
  (find-by-name [_ name]
    (some #(and (= name (-> % :name)) %) shifts))
  (knows? [this name1 name2]
    (let [first (find-by-name this name1)
          second (find-by-name this name2)]
      (= (-> first :shift) (-> second :shift))))
  (knows [this name]
    (let [person (find-by-name this name)
          person-shift (-> person :shift)]
      (filter #(= person-shift (-> % :shift)) shifts))))

(def shs (db-shs. [(ShiftRelation. "Мария" "day")
                   (ShiftRelation. "Сергей" "night")
                   (ShiftRelation. "Борис" "night")
                   (ShiftRelation. "Валентина" "night")]))

(knows? shs "Сергей" "Борис")
(knows shs "Валентина")
(knows shs "Мария")

; Вычислить n-ый член последовательности Фибоначчи.
(defn fib
  ([]
   (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(defn fib-n [n]
  (last (take n (fib))))

(fib-n 10)

; Вычислить N!+(N-1)!+...+2!+1!.
(defn fac [n] 
  (if (= n 1) 
    1
    (* n (fac (- n 1)))))

(defn sum-fac [N]
  (if (= N 1)
    1
    (+ (fac N) (sum-fac (- N 1)))))

(sum-fac 5)

; Перевести число из десятичной системы счисления в систему с основанием N, где N<10, N>1.
(defn convert-to [num N]
  (if (= num 0)
    "0"
    (str (convert-to (quot num N) N) (rem num N))))

(defn convert [num N]
  (Integer/parseInt (convert-to num N)))

(convert 14 2)