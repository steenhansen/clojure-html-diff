
; HOW TO USE
; (ns name-space
;   (:require [text-diff :refer [is-html-eq]])
;  )
;
; (is-html-eq expected-test-value actual-test-value)
;
; NB functions are ignored as no way to tell if they are the same

(ns text-diff)

(comment
  (let [html-1 "<div>123<div>"
        html-2 "<div>123<div>"
        [text-diff-1 text-diff-2] (is-html-eq html-1 html-2)]
    (is (= text-diff-1 text-diff-2)))
; true
  )

(comment
  (let [html-1 "<same>DIFFERENT</same>"
        html-2 "<same>different</same>"
        [diff-1 diff-2] (is-html-eq html-1 html-2)]
    (is (= diff-1 diff-2)))
; |START|<same>
; |DIFF1|e>DIFFERENT</
; |DIFF2|e>different</
; |  END|</same>
; FAIL in () (form-init6197705762988680828.clj:202)
; expected (= diff-1 diff-2)
; actual (not (= "DIFFERENT" "different"))
; false
  )

(comment
  (let [html-1 "abcdefghijklmnopqrstuvwxyzDIFFERENTabcdefghijklmnopqrstuvwxyz"
        html-2 "abcdefghijklmnopqrstuvwxyzdifferentabcdefghijklmnopqrstuvwxyz"
        [diff-1 diff-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]
    (is (= diff-1 diff-2)))
; |START|abc ... xyz
; |DIFF1|zDIF ... ENTa
; |DIFF2|zdif ... enta
; |  END|abc ... xyz
; FAIL in () (form-init1629240743054083851.clj:363)
; expected (= diff-1 diff-2)
; actual (not (= "DIF ... ENT" "dif ... ent"))
; false
  )

(comment  ; letter L versus number 1
  (let [html-1 "abcdefghijklmnopqrstuvwxyz_1_abcdefghijklmnopqrstuvwxyz"
        html-2 "abcdefghijklmnopqrstuvwxyz_l_abcdefghijklmnopqrstuvwxyz"
        [diff-1 diff-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]
    (is (= diff-1 diff-2)))
; |START|abc ... yz_
; |DIFF1|_1_
; |DIFF2|_l_
; |  END|_ab ... xyz
; FAIL in () (form-init1629240743054083851.clj:363)
; expected (= text-diff-1 text-diff-2)
; actual (not (= "1" "l"))
; false
  )

(comment  ; tab versus spaces
  (let [html-1 "abc	xyz"
        html-2 "abc  xyz"
        [diff-1 diff-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]
    (is (= diff-1 diff-2)))
; |START|abc
; |DIFF1|c\tx
; |DIFF2|c x
; |  END|xyz
; FAIL in () (form-init1629240743054083851.clj:363)
; expected (= diff-1 diff-2)
; actual (not (= "\t" " "))
; false
  )

(comment
  (let [html-1 "qwe\r\n_asd"
        html-2 "qwe\n-asd"
        [diff-1 diff-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]
    (is (= diff-1 diff-2)))
; |START|qwe
; |DIFF1|e\r\n_
; |DIFF2|e\n-a
; |  END|asd
; FAIL in () (form-init1629240743054083851.clj:363)
; expected (= diff-1 diff-2)
; actual (not (= "\r\n_" "\n-"))
; false			
  )

(def SIZE-PARTITION-SHORT 30)
(def DEFAULT-SANDWICH-SIZE 2)
(def ANSI-BLACK "\u001b[30m")
(def ANSI-RED "\u001b[31m")
(def ANSI-GREEN "\u001b[32m")
(def ANSI-YELLOW "\u001b[33m")
(def ANSI-BLUE "\u001b[34m")
(def ANSI-MAGENTA "\u001b[35m")
(def ANSI-CYAN "\u001b[36m")
(def ANSI-WHITE "\u001b[37m")
(def ANSI-RESET "\u001b[0m")
(def DEFAULT-DIFF-COLORS (hash-map :ERROR-COLOR ANSI-RED
                                   :LEGEND-COLOR ANSI-BLUE
                                   :SAME-COLOR ANSI-GREEN
                                   :WHITESPACE-COLOR ANSI-MAGENTA
                                   :RESET-COLOR ANSI-RESET))
(def TEST-DIFF-COLORS {})
(def TEST-DIFF-PARTITION 30)
(def TEST-DIFF-SANDWICH 2)

(def FUNCTION-REPRESENTATION "A_Function")
(def NIL-REPRESENTATION "A_Nil")

(comment
  ; ["" 42 "" ""]
  (get-colors {:RESET-COLOR 42}))
(defn get-colors [char-colors]
  (let [error-col (get char-colors :ERROR-COLOR "")
        reset-col (get char-colors :RESET-COLOR "")
        legend-col (get char-colors :LEGEND-COLOR "")
        same-col (get char-colors :SAME-COLOR "")
        the-colors [error-col reset-col legend-col same-col]]
    the-colors))

(comment
  ; "1\n2\n3"
  (trim-to-n " 1\r\n2\r3 "))
(defn trim-to-n [padded-result]
  (let [trimmed-result (clojure.string/trim padded-result)
        rn-result (clojure.string/replace trimmed-result #"\r\n" "\n")
        n-result (clojure.string/replace rn-result  #"\r" "\n")]
    n-result))

(comment
  ; "_1"
  (build-until-diff "_" ["1" "1"])

  ; "_"
  (build-until-diff "_" ["1" "2"]))
(defn build-until-diff [accum char-tuple]
  (let [[char-1 char-2] char-tuple]
    (if (= char-1 char-2)
      (str accum char-1)
      (reduced accum))))

(comment
  ; "12"
  (start-str "123" "129"))
(defn start-str [str-1 str-2]
  (let [chars-1 (to-array str-1)
        chars-2 (to-array str-2)
        char-tuples (map vector chars-1 chars-2)]
    (reduce build-until-diff "" char-tuples)))

(comment
  ; "23"
  (end-str "123" "423"))
(defn end-str [str-1 str-2]
  (let [chars-1 (reverse str-1)
        chars-2 (reverse str-2)
        char-tuples (map vector chars-1 chars-2)
        reversed-end (reduce build-until-diff "" char-tuples)
        ordered-end (reverse reversed-end)
        ordered-str (apply str ordered-end)]
    ordered-str))

(comment
  ;"45"
  (get-middle "1234567890" 3 5))
(defn get-middle [str-text start-length end-pos]
  (if (= end-pos 0)
    ""
    (subs str-text start-length end-pos)))

(comment
  ; ["123" "987"]
  (middle-diffs "abcd123edfg" "abcd987edfg" "abcd" "edfg"))
(defn middle-diffs [str-1 str-2 start-same end-same]
  (let [start-length (.length start-same)
        end-length (.length end-same)
        length-1 (.length str-1)
        end-pos-1 (- length-1 end-length)
        length-2 (.length str-2)
        end-pos-2 (- length-2 end-length)
        middle-1 (get-middle str-1 start-length end-pos-1)
        middle-2 (get-middle str-2 start-length end-pos-2)
        middle-tuple (vector middle-1 middle-2)]
    middle-tuple))

(comment
  ; "bc"
  (sandwich-start "abc" 2))
(defn sandwich-start [front-same num-chars]
  (let [front-length (.length front-same)]
    (if (> num-chars front-length)
      front-same
      (subs front-same (- front-length num-chars)))))

(comment
  ; "ab"
  (sandwich-end "abc" 2))
(defn sandwich-end [back-same num-chars]
  (let [back-length (.length back-same)]
    (if (> num-chars back-length)
      back-same
      (subs back-same 0 num-chars))))

(comment
  ; "abcde ... vwxyz"
  (shrink-middle "abcdefghijklmnopqrstuvwxyz" 5)

  ; "abcd"
  (shrink-middle "abcd" 2))
(defn shrink-middle [long-html size-partition]
  (let [length-html (.length long-html)
        start-length size-partition
        end-length size-partition
        middle-length (- length-html start-length end-length)]
    (if (< middle-length 1)
      long-html
      (let [start-html (subs long-html 0 size-partition)
            middle-html " ... "
            end-html (subs long-html (- length-html size-partition) length-html)
            short-html (str start-html middle-html end-html)]
        short-html))))

(comment
  ; ["123 ... 890" "abc ... jkl" "zyx ... pon" "098 ... 321"]
  (text-matches "1234567890abcdefghijkl0987654321" "1234567890zyxwvutsrqpon0987654321" 3)

  ; ["1234" "abcd" "zyxw" "0987"]
  (text-matches "1234abcd0987" "1234zyxw0987" 2))
(defn text-matches [str-1 str-2 partition-size]
  (let [start-same (start-str str-1 str-2)
        end-same (end-str str-1 str-2)
        [middle-1 middle-2] (middle-diffs str-1 str-2 start-same end-same)
        start-short (shrink-middle start-same partition-size)
        middle-1-short (shrink-middle middle-1 partition-size)
        middle-2-short (shrink-middle middle-2 partition-size)
        end-short (shrink-middle end-same  partition-size)
        start-middle2-end (vector start-short middle-1-short middle-2-short end-short)]
    start-middle2-end))

(comment
  ; "aa\\tbb\\r\\ncc\\rdd\\nee"
  (show-chars "aa\tbb\r\ncc\rdd\nee" "" ""))
(defn show-chars [whitespace-str current-color whitespace-col]
  (let [tab-show (str whitespace-col "\\\\t" current-color)
        rn-show (str whitespace-col  "\\\\r\\\\n" current-color)
        r-show (str whitespace-col  "\\\\r" current-color)
        n-show (str whitespace-col  "\\\\n" current-color)
        no-tabs (clojure.string/replace whitespace-str #"\t" tab-show)
        no-rns (clojure.string/replace no-tabs #"\r\n" rn-show)
        no-rs (clojure.string/replace no-rns #"\r" r-show)
        no-ns (clojure.string/replace no-rs #"\n" n-show)]
    no-ns))

(comment
  ; ["aaa" "aXb" "aYb" "bbb" "X" "Y"]
  (char-difference "aaaXbbb" "aaaYbbb" 1 2 {}))
(defn char-difference  [html-1 html-2 sandwich-size partition-size char-colors]
  (let [[start-same middle-1-plain middle-2-plain end-same] (text-matches html-1 html-2 partition-size)
        front-sandwich (sandwich-start start-same sandwich-size)
        error-col (get char-colors :ERROR-COLOR "")
        same-col (get char-colors :SAME-COLOR "")
        whitespace-col (get char-colors :WHITESPACE-COLOR "")
        front-sand-show (show-chars front-sandwich same-col whitespace-col)
        end-sandwich (sandwich-end end-same sandwich-size)
        end-sand-show (show-chars end-sandwich same-col whitespace-col)
        middle-1-show (show-chars middle-1-plain error-col whitespace-col)
        middle-2-show (show-chars middle-2-plain error-col whitespace-col)
        extra-middle-1  (str same-col front-sand-show error-col middle-1-show same-col end-sand-show)
        extra-middle-2  (str same-col front-sand-show error-col middle-2-show same-col end-sand-show)
        start-show (show-chars start-same same-col whitespace-col)
        end-show (show-chars end-same same-col whitespace-col)
        char-all (vector start-show extra-middle-1 extra-middle-2 end-show middle-1-plain middle-2-plain)]
    char-all))

(comment
  ; ["|START|abc|DIFF1|bcXde|DIFF2|bcYde|  END|def\n" "X" "Y"]
  (colored-differences "abc" "def" "|START|abc" "|DIFF|bcXde" "|DIFF|bcYde" "|  END|def" "X" "Y"))
(defn colored-differences [start-show end-show front-line middle-1-line middle-2-line back-line middle-1-plain middle-2-plain]
  (let  [front-empty (= 0 (.length start-show))
         back-empty (= 0 (.length end-show))]
    (if (and front-empty back-empty)
      [(str middle-1-line middle-2-line) middle-1-plain middle-2-plain]
      (if (and front-empty (not back-empty))
        [(str  middle-1-line middle-2-line back-line) middle-1-plain middle-2-plain]
        (if (and front-empty (not back-empty))
          [(str front-line middle-1-line middle-2-line) middle-1-plain middle-2-plain]
          [(str front-line middle-1-line middle-2-line back-line) middle-1-plain middle-2-plain])))))

(declare variable-to-str)

(comment
  ; "A_Function"
  (let [my-func (defn a-func [para] (+ para 1))]
    (function-to-str my-func)))
(defn function-to-str [a-function]
  (str FUNCTION-REPRESENTATION))

(comment
  ; " [1  \"two\"  { :three  '(4 [5]) }  ]"
  (vector-to-str [1 "two" {:three '(4 [5])}]))
(defn vector-to-str [a-vector]
  (let [vector-members (reduce variable-to-str "" a-vector)]
    (str " [" vector-members " ]")))

(comment
  ;"  { :a 1  :b  { :two  '(4 [5]) }  :c \"three\" } "
  (map-to-str {:a 1 :c "three" :b {:two '(4 [5])}}))
(defn map-to-str [a-map]
  (let [map-sorted (into (sorted-map) (sort-by first (seq a-map)))
        map-members (reduce variable-to-str "" map-sorted)]
    (str " {" map-members "} ")))

(comment
  ; " '(1 \"two\" (quote (3 [4]))) "
  (list-to-str '(1  "two" '(3 [4]))))
(defn list-to-str [a-list]
  (str " '" a-list " "))

(comment
  ; " :a \"aa\" "
  (entry-to-str (first {:a "aa"})))
(defn entry-to-str [an-entry]
  (if (string? (val an-entry))
    (str " " (key an-entry) " \"" (val an-entry) "\" ")
    (variable-to-str (str " " (key an-entry) " ") (val an-entry))))

(comment
  ; "Steel's"
  (string-to-str "Steel's"))
(defn string-to-str [a-string]
  a-string)

(comment
  ; "17 "
  (other-to-str 17))
(defn other-to-str [a-variable]
  (str a-variable " "))

(comment
  ; "A_Nil "
  (nil-to-str nil))
(defn nil-to-str [a-nil]
  (str NIL-REPRESENTATION " "))

(comment
  ; " [ { :a 1  :b \"2\"  :c  [ \"a\" 12346  \"c\"  ] :d  '(123 \"x\") } :f  { :a 1  :b 2 }  [ \"a\" 12346  \"c\"  ] '(123 \"x\")  ]"
  (variable-to-str "" [{:a 1
                        :b "2"
                        :c ["a" 12346 "c"]
                        :d '(123 "x")}
                       :f {:a 1 :b 2}
                       ["a" 12346 "c"]
                       '(123 "x")]))

(defn is-a-function? [maybe-func]
  (if (fn? maybe-func)
    true
    (try
      (if (-> maybe-func symbol resolve deref ifn?)
        true
        false)
      (catch Exception e false))))

(defn variable-to-str [accum-str the-variable]
  (if (map-entry? the-variable)
    (str accum-str (entry-to-str the-variable))
    (if (is-a-function? the-variable)
      (str accum-str (function-to-str the-variable))
      (if (vector? the-variable)
        (str accum-str (vector-to-str the-variable))
        (if (map? the-variable)
          (str accum-str (map-to-str the-variable))
          (if (list? the-variable)
            (str accum-str (list-to-str the-variable))
            (if (string? the-variable)
              (str accum-str (string-to-str the-variable))
              (if (nil? the-variable)
                (str accum-str (nil-to-str the-variable))
                (str accum-str (other-to-str the-variable))))))))))

(defn var-to-str [the-variable]
  (let [str-var (variable-to-str "" the-variable)]
    str-var))

(comment
  ; ["[34m\n|DIFF1|[31m[32m[31ma[32m[0m[34m\n|DIFF2|[31m[32m[31mb[32m[0m" "a" "b"]
  (show-diff "a" "b")

  ; ["" "" ""]
  (show-diff "" "")

  ; ["|START|abc\n|DIFF1|bcXde\n|DIFF2|bcYde\n|  END|def\n" "X" "Y"]
  (show-diff "abcXdef" "abcYdef" {})

  ; ["|START|abc\n|DIFF1|cXd\n|DIFF2|cYd\n|  END|def\n" "X" "Y"]
  (show-diff "abcXdef" "abcYdef" {} 1)

  ; ["|START|abc ... hij\n|DIFF1|jXk\n|DIFF2|jYk\n|  END|klm ... rst\n" "X" "Y"]
  (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 1 3)

  ; ["|START|abc ... hij\n|DIFF1|ijXkl\n|DIFF2|ijYkl\n|  END|klm ... rst\n" "X" "Y"]
  (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 2 3)

  ; [ "|START|ab ... ij\n|DIFF1|jXk\n|DIFF2|jYk\n|  END|kl ... st\n" "X" "Y"]
  (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 1 2)

 ;["|START|1\n|DIFF1|123 \n|DIFF2|183 \n|  END|3 \n" "2" "8"]
  (show-dif 123 183 {})

 ; ["" "" ""]
  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]
        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]]
    (show-diff vec-1 vec-2 {}))

 ; ["|START| [ { :a 1  :b \"2\"  :c  [ \"a\" 1 ...  } :f  { :a 1  :b 2 }  [ \"a\" 1\n|DIFF1| 1234\n|DIFF2| 1934\n|  END|346  \"c\"  ] '(123 \"x\")  ]\n" "2" "9"]
  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]
        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 19346 "c"] '(123 "x")]]
    (show-diff vec-1 vec-2 {})))

(defn show-diff
  ([html-1 html-2] (show-diff html-1 html-2 DEFAULT-DIFF-COLORS DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors] (show-diff html-1 html-2 char-colors DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size] (show-diff html-1 html-2 char-colors sandwich-size SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size partition-size]
   (let  [variable-1 (var-to-str html-1)
          variable-2 (var-to-str html-2)]
     (if (= variable-1 variable-2)
       ["" "" ""]
       (let [[start-show middle-1-show middle-2-show end-show middle-1-plain middle-2-plain] (char-difference variable-1 variable-2 sandwich-size partition-size char-colors)
             [error-col reset-col legend-col same-col] (get-colors char-colors)
             middle-1-color (str error-col middle-1-show reset-col)
             middle-2-color (str error-col middle-2-show reset-col)
             front-line      (str legend-col "|START|" same-col start-show reset-col)
             middle-1-line (str legend-col "\n|DIFF1|"  middle-1-color)
             middle-2-line (str legend-col "\n|DIFF2|"  middle-2-color)
             back-line     (str legend-col "\n|  END|" same-col end-show "\n" reset-col)
             colored-output (colored-differences start-show end-show front-line middle-1-line middle-2-line back-line middle-1-plain middle-2-plain)]
         colored-output)))))

(comment
  ; |START|a
  ; |DIFF1|a1b
  ; |DIFF2|a2b
  ; |  END|b
  ; ["1" "2"]
  (is-html-eq "a1b" "a2b")

  ; |START| [ { :a 1  :b "2"  :c  [ "a" 1 ...  } :f  { :a 1  :b 2 }  [ "a" 1
  ; |DIFF1| 1234
  ; |DIFF2| 1934
  ; |  END|346  "c"  ] '(123 "x")  ]
  ; ["2" "9"]
  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]
        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 19346 "c"] '(123 "x")]]
    (is-html-eq vec-1 vec-2))

  ; |START| [ { :a "a" } 1  '("2" [1 2 3 {:z 123, :arr [1 "2" 3 {:er 1/
  ; |DIFF1|1/3}]
  ; |DIFF2|1/4}]
  ; |  END|}]}])  ]
  ; ["3" "4"]
  (is-html-eq [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/3}]}])]
              [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/4}]}])]
              {})

  ; ["" ""]
  (let [afunc  (defn a-func [a] (+ a a))
        bfunc (defn b-func [b] (+ b b))]
    (is-html-eq afunc bfunc))

 ; ["" ""]
  (let [afunc  (fn [a] (+ a a))
        bfunc (fn [b] (+ b b))]
    (is-html-eq afunc bfunc)))
(defn is-html-eq
  ([html-1 html-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors] (is-html-eq html-1 html-2 char-colors DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size] (is-html-eq html-1 html-2 char-colors sandwich-size SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size partition-size]
   (let [[color-diff plain-1-diff plain-2-diff] (show-diff html-1 html-2 char-colors sandwich-size partition-size)]
     (if (not (= plain-1-diff plain-2-diff))
       (println color-diff))
     [plain-1-diff plain-2-diff])))

