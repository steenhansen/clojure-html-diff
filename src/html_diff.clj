
(ns html-diff)

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
  ; ["123" "987"]
  (middle-diffs "abcd123edfg" "abcd987edfg" "abcd" "edfg"))
(defn middle-diffs [str-1 str-2 start-same end-same]
  (let [start-length (.length start-same)
        end-length (.length end-same)
        length-1 (.length str-1)
        end-pos-1 (- length-1 end-length)
        length-2 (.length str-2)
        end-pos-2 (- length-2 end-length)
        middle-1 (subs str-1 start-length end-pos-1)
        middle-2 (subs str-2 start-length end-pos-2)
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
  (text-diff "1234567890abcdefghijkl0987654321" "1234567890zyxwvutsrqpon0987654321" 3)

  ; ["1234" "abcd" "zyxw" "0987"]
  (text-diff "1234abcd0987" "1234zyxw0987" 2))
(defn text-diff [str-1 str-2 partition-size]
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
  (let [[start-same middle-1-plain middle-2-plain end-same] (text-diff html-1 html-2 partition-size)
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
  (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 1 2))
(defn show-diff
  ([html-1 html-2] (show-diff html-1 html-2 DEFAULT-DIFF-COLORS DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors] (show-diff html-1 html-2 char-colors DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size] (show-diff html-1 html-2 char-colors sandwich-size SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size partition-size]
   (if (= html-1 html-2)
     ["" "" ""]
     (let [[start-show middle-1-show middle-2-show end-show middle-1-plain middle-2-plain] (char-difference html-1 html-2 sandwich-size partition-size char-colors)
           [error-col reset-col legend-col same-col] (get-colors char-colors)
           middle-1-color (str error-col middle-1-show reset-col)
           middle-2-color (str error-col middle-2-show reset-col)
           front-line      (str legend-col "|START|" same-col start-show reset-col)
           middle-1-line (str legend-col "\n|DIFF1|"  middle-1-color)
           middle-2-line (str legend-col "\n|DIFF2|"  middle-2-color)
           back-line     (str legend-col "\n|  END|" same-col end-show "\n" reset-col)
           colored-output (colored-differences start-show end-show front-line middle-1-line middle-2-line back-line middle-1-plain middle-2-plain)]
       colored-output))))

(comment
  ; |START|a
  ; |DIFF1|a1b
  ; |DIFF2|a2b
  ; |  END|b
  ; ["1" "2"]
  (is-html-eq "a1b" "a2b"))
(defn is-html-eq
  ([html-1 html-2] (is-html-eq html-1 html-2 DEFAULT-DIFF-COLORS DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors] (is-html-eq html-1 html-2 char-colors DEFAULT-SANDWICH-SIZE SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size] (is-html-eq html-1 html-2 char-colors sandwich-size SIZE-PARTITION-SHORT))
  ([html-1 html-2 char-colors sandwich-size partition-size]
   (let [[color-diff plain-1-diff plain-2-diff] (show-diff html-1 html-2 char-colors sandwich-size partition-size)]
     (if (not (= html-1 html-2))
       (println color-diff))
     [plain-1-diff plain-2-diff])))

