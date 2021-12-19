(ns day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; t4 packet
;; a packet with type ID 4 is a number
;; VVVTTTAAAAABBBBBCCCCC
;; 110100101111111000101000


;; t6 packet
;; a packet with a type ID that is NOT 4 is an operator packet
;; VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
;; 00111000000000000110111101000101001010010001001000000000
;; I is the length type ID
;; 0 means that the next 15 bits indicate the number of bits of data
;; here the numbers is 27, so we take 16 bits chunk starting from the end, i.e. we need to pad at the front

;; 11101110000000001101010000001100100000100011000001100000
;; VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
;; 1 means that the next 11 bits encode the number of 11 bit packets that follow

;; unnötig komplizierte Aufgabe...

;; represents an operator packet (version 4) 
;; which contains an operator packet (version 1) 
;; which contains an operator packet (version 5) 
;; which contains a literal value (version 6); 
;; this packet has a version sum of 16.
(def example "8A004A801A8002F478")

;; represents an operator packet (version 3) 
;; which contains two sub-packets;
;; each sub-packet is an operator packet that contains two literal values. 
;; This packet has a version sum of 12.
(def example2 "620080001611562C8802118E34")

(def hex->bin {\0 "0000"
               \1 "0001"
               \2 "0010"
               \3 "0011"
               \4 "0100"
               \5 "0101"
               \6 "0110"
               \7 "0111"
               \8 "1000"
               \9 "1001"
               \A "1010"
               \B "1011"
               \C "1100"
               \D "1101"
               \E "1110"
               \F "1111"})

(def real-input (slurp (io/resource "day16/input.txt")))

(def bin->hex (clojure.set/map-invert hex->bin))

(defn bin->dec
  [s]
  (Integer/parseInt s 2))

(defn reverse-s
  [s]
  (. (. (StringBuffer. s) reverse) toString))

(defn partition-s
  [n ^String s]
  (loop [s      s
         result []]
    (if (< (count s) n)
      (if (empty? s)
        result
        (conj result s))
      (recur (. s substring n (count s)) (conj result (. s substring 0 n))))))

(defn parse-chunks
  [s]
  (loop [chunks (partition-s 5 s)
         hex-s  ""]
    (let [next (first chunks)]
      (if (str/starts-with? next "0")
        (str hex-s (bin->hex (. next substring 1 5)))
        (recur (rest chunks) (str hex-s (bin->hex (. next substring 1 5))))))))

(def type->op {0 +
               1 *
               2 min
               3 max
               5 (fn [a b] (if (> a b) 1 0))
               6 (fn [a b] (if (< a b) 1 0))
               7 (fn [a b] (if (= a b) 1 0))})

(defn parse-packet
  [^String bin]
  (loop [bin bin
         res []]
    ;; any packet is at least 11 bits long
    (if (< (count bin) 11)
      res
      (let [version     (bin->dec (. bin substring 0 3))
            type        (bin->dec (. bin substring 3 6))]
        (if (= type 4)
          (let [parsed-chunks (parse-chunks (. bin substring 6 (count bin)))
                rest          (. bin substring (+ 6 (* 5 (count parsed-chunks))) (count bin))]
            (recur rest (conj res {:version version :type type :val (Long/parseLong (apply str (map hex->bin parsed-chunks)) 2)})))
          (let [length-type (bin->dec (. bin substring 6 7))]
            (if (= length-type 0)
              (let [packet-length (bin->dec (. bin substring 7 22))
                    packets-s     (. bin substring 22 (+ 22 packet-length))
                    rest          (. bin substring (+ 22 packet-length) (count bin))]
                (recur rest (conj res {:version version :type type :op (type->op type) :subpackets (parse-packet packets-s)})))
              (let [num-subpackets (bin->dec (. bin substring 7 18))]
                (recur (. bin substring 18 (count bin)) (conj res {:version version :type type :op (type->op type) :num-subpackets num-subpackets}))))))))))

(defn version-sum
  [packet]
  (if (not (contains? packet :subpackets))
    (packet :version)
    (+ (packet :version) (reduce + (map version-sum (packet :subpackets))))))

(let [parsed (parse-packet (apply str (map hex->bin example)))]
  parsed
  (reduce + (map version-sum parsed)))
(reduce + (map version-sum (parse-packet (apply str (map hex->bin example2)))))
(reduce + (map version-sum (parse-packet (apply str (map hex->bin "C0015000016115A2E0802F182340")))))
;; part 1
(reduce + (map version-sum (parse-packet (apply str (map hex->bin "A0016C880162017C3686B18A3D4780")))))

(reduce + (map version-sum (parse-packet (apply str (map hex->bin real-input)))))

;; treeify muss immer ne liste zurückgeben
(defn treeify4
  ([packets]
   (treeify4 packets -1))
  ([packets num]
   (loop [queue packets
          num   num
          target []]
     (if (or (= 0 num) (empty? queue))
       [target queue]
       (let [next (first queue)
             rest (rest queue)]
         (cond
           (contains? next :num-subpackets) (let [[subpackets rest] (treeify4 rest (next :num-subpackets))]
                                              (recur rest (dec num) (conj target (assoc next :subpackets subpackets))))
           (contains? next :subpackets)     (let [[subpackets _] (treeify4 (next :subpackets))]
                                              (recur rest (dec num) (conj target (assoc next :subpackets subpackets))))
           :else                            (recur rest (dec num) (conj target next))))))))

(let [packets [{:subpackets [] :num-subpackets 3}
               {:subpackets [] :num-subpackets 2} {:id 3} {:id 4}
               {:id 1}
               {:id 2 :subpackets [{:subpackets [] :num-subpackets 2} {:id 5} {:id 6}]}]]
  (-> (treeify4 packets)
      first
      clojure.pprint/pprint))

(let [packets [{:id 2 :subpackets [{:subpackets [] :num-subpackets 2} {:id 3} {:id 4}]}]]
  (-> (treeify4 packets)
      first
      clojure.pprint/pprint))

(let [packets [{:id 2 :subpackets [{:id 3} {:id 4}]}]]
  (-> (treeify4 packets)
      first
      clojure.pprint/pprint))

(clojure.pprint/pprint (parse-packet (apply str (map hex->bin "C0015000016115A2E0802F182340"))))
(clojure.pprint/pprint (first (treeify4 (parse-packet (apply str (map hex->bin "C0015000016115A2E0802F182340"))))))

(clojure.pprint/pprint (parse-packet (apply str (map hex->bin "A0016C880162017C3686B18A3D4780"))))
(clojure.pprint/pprint (treeify4 (parse-packet (apply str (map hex->bin "A0016C880162017C3686B18A3D4780")))))

(clojure.pprint/pprint (parse-packet (apply str (map hex->bin real-input))))
(clojure.pprint/pprint (treeify4 (parse-packet (apply str (map hex->bin real-input)))))

(defn calc
  [node]
  (if (not (contains? node :op))
    (node :val)
    (apply (node :op) (map calc (node :subpackets)))))

(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "C0015000016115A2E0802F182340")))))]
  (calc (first tree)))

(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "C200B40A82")))))]
  (calc (first tree)))

(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "04005AC33890")))))]
  (calc (first tree)))

(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "9C0141080250320F1802104A08")))))]
  (calc (first tree)))
(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "9C005AC2F8F0")))))]
  (calc (first tree)))
(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "F600BC2D8F")))))]
  (calc (first tree)))
(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "CE00C43D881120")))))]
  (calc (first tree)))
(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "880086C3E88112")))))]
  (calc (first tree)))

(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin "D8005AC2A8F0")))))]
  (calc (first tree)))

;; part 2
(let [tree (first (treeify4 (parse-packet (apply str (map hex->bin real-input)))))]
  (calc (first tree))) ;; 19348959966392

