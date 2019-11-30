
; HOW TO USE
; (ns name-space
;   (:require [text-diff :refer [are-vars-eq]])
;  )

; NB functions are ignored as no way to tell if they are the same

; HOW TO TEST
; test-text-diff> (do-tests)

(ns text-diff
  (:require [clojure.test :refer :all])
  (:require [clojure.string :refer :all])
)

(comment
  (let [value-1 "<div>123<div>"
        value-2 "<div>123<div>"
        [text-diff-1 text-diff-2] (are-vars-eq value-1 value-2)]
    (is (= text-diff-1 text-diff-2)))
; true

  (let [value-1 "<same>DIFFERENT</same>"
        value-2 "<same>different</same>"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
; |START|"<same>"
; |DIFF1|"e>DIFFERENT</"
; |DIFF2|"e>different</"
; |  END|"</same>"
; expected (= diff-1 diff-2)
; actual (not (= "DIFFERENT" "different"))
; false

  (let [value-1 "abcdefghijklmnopqrstuvwxyzDIFFERENTabcdefghijklmnopqrstuvwxyz"
        value-2 "abcdefghijklmnopqrstuvwxyzdifferentabcdefghijklmnopqrstuvwxyz"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ;|START|"abcdefghijklmnopqrstuvwxyz"
  ;|DIFF1|"yzDIFFERENTab"
  ;|DIFF2|"yzdifferentab"
  ;|  END|"abcdefghijklmnopqrstuvwxyz"
  ;actual: (not (= "DIFFERENT" "different"))

  (let [value-1 "abcdefghijklmnopqrstuvwxyz_1_abcdefghijklmnopqrstuvwxyz"     ; letter L versus number 1
        value-2 "abcdefghijklmnopqrstuvwxyz_l_abcdefghijklmnopqrstuvwxyz"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ;|START|"abcdefghijklmnopqrstuvwxyz_"
  ;|DIFF1|"z_1_a"
  ;|DIFF2|"z_l_a"
  ;|  END|"_abcdefghijklmnopqrstuvwxyz"
  ;actual (not (= "1" "l"))

  (let [value-1 "abc	xyz"             ; tab versus spaces
        value-2 "abc xyz"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; |START|"abc"
  ; |DIFF1|"bc\txy"
  ; |DIFF2|"bc xy"
  ; |  END|"xyz"
  ; actual (not (= "\t" " "))

  (let [value-1 "qwe\r\n_asd"    ;unix versus Windows eol
        value-2 "qwe\n-asd"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; |START|"qwe"
  ; |DIFF1|"we\r\n_as"
  ; |DIFF2|"we\n-as"
  ; |  END|"asd"
  ; actual (not (= "\r\n_" "\n-"))

  (let [value-1 "123456789"
        value-2  123406789
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ;|diff1|"123456789" String
  ;|diff2| 123406789  Long
  ; actual: (not (= "123456789" 123406789))

  (let [value-1 {:a "a" :b "b" :c "c"}
        value-2 {:b "b" :c "c" :a "a"}
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; true

  (let [value-1 [123 "456"]
        value-2 ["123" 456]
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ;|START|[
  ;|DIFF1|[ 123 "456" ]
  ;|DIFF2|[ "123" 456 ]
  ;|  END|]
  ; actual: (not (= "123 \"456\"" "\"123\" 456"))
  )

(def DEFAULT-PRE-POST-SIZE 2)
(def DEFAULT-ELLIPSIS-SIZE 30)

(def TEXT-N-EOL  "\\\\n")
(def REAL-ANSI-COLOR  #"\u001b")
(def TEXT-ANSI-COLOR  "\\\\u001b")
(def MATCH-ANSI-COLOR #"\\u001b\[\d+m")

(def START-OR-END-QUOTES  #"^\"|\"$")
(def END-QUOTE  #"\"$")
(def START-QUOTE  #"^\"")

(def END-DOT-TYPE  #"\.[^\.]*$")

(def T-NO-ERR ["" "" "" "" ""])

(def REPL-ERROR-PRINT "REPL-ERROR-PRINT")
(def NO-REPL-PRINT "NO-REPL-PRINT")

(def ANSI-BLACK "\u001b[30m")
(def ANSI-RED "\u001b[31m")
(def ANSI-GREEN "\u001b[32m")
(def ANSI-YELLOW "\u001b[33m")
(def ANSI-BLUE "\u001b[34m")
(def ANSI-MAGENTA "\u001b[35m")
(def ANSI-CYAN "\u001b[36m")
(def ANSI-WHITE "\u001b[37m")
(def ANSI-RESET "\u001b[0m")

(def DEFAULT-CHAR-COLORS
  (hash-map :ERROR-COLOR ANSI-RED
            :LEGEND-COLOR ANSI-BLUE
            :RESET-COLOR ANSI-RESET
            :SAME-COLOR ANSI-GREEN
            :TYPE-COLOR ANSI-YELLOW
            :WHITESPACE-COLOR ANSI-MAGENTA))

(def FUNCTION-REPRESENTATION "A_Function")
(def NIL-REPRESENTATION "A_Nil")
(def STRING-DELIM "\"")

(def NIL-TYPE-STR "Nil")

(def VARIETY-RATIOS-INTEGERS   "VARIETY-RATIOS-INTEGERS")
(def VARIETY-FLOATS  "VARIETY-FLOATS")

(def VARIETY-BIGDEC  "VARIETY-BIGDEC")
(def VARIETY-STRING "VARIETY-STRING")

(def VARIETY-MAP "VARIETY-MAP")
(def VARIETY-SET "VARIETY-SET")
(def VARIETY-VECTOR "VARIETY-VECTOR")
(def VARIETY-LIST "VARIETY-LIST")
(def VARIETY-BOOLEAN "VARIETY-BOOLEAN")

(def VARIETY-FUNCTION "VARIETY-FUNCTION")
(def VARIETY-NIL "VARIETY-NIL")
(def VARIETY-OTHER "VARIETY-OTHER")

(comment
  (get-colors {:RESET-COLOR 42})
  ; ["" 42 "" ""]
  )
(defn get-colors [char-colors]
  (let [error-col (get char-colors :ERROR-COLOR "")
        legend-col (get char-colors :LEGEND-COLOR "")
        reset-col (get char-colors :RESET-COLOR "")
        same-col (get char-colors :SAME-COLOR "")
        type-col (get char-colors :TYPE-COLOR "")

        whitespace-col (get char-colors :WHITESPACE-COLOR "")
        the-colors [error-col legend-col reset-col same-col type-col whitespace-col]]
    the-colors))

(comment
  (n-eoln " 1\r\n2\r3 ")
  ; "1\n2\n3"
  )
(defn n-eoln [padded-result]
  (let [trimmed-result (trim padded-result)
        rn-result (replace trimmed-result #"\r\n" "\n")
        n-result (replace rn-result  #"\r" "\n")]
    n-result))

(comment
  (find-start "_" ["1" "1"])
  ; "_1"

  (find-start "_" ["1" "2"])
  ; "_"
  )
(defn find-start [accum char-tuple]
  (let [[char-1 char-2] char-tuple]
    (if (= char-1 char-2)
      (str accum char-1)
      (reduced accum))))

(comment
  (common-start "123" "129")
  ; "12"
  )
(defn common-start [str-1 str-2]
  (let [chars-1 (to-array str-1)
        chars-2 (to-array str-2)
        char-tuples (map vector chars-1 chars-2)
        my-start (reduce find-start "" char-tuples)]
    (if (= my-start STRING-DELIM)
      ""
      my-start)))

(comment
  (common-end "123" "423")
  ; "23"
  )
(defn common-end [str-1 str-2]
  (let [chars-1 (reverse str-1)
        chars-2 (reverse str-2)
        char-tuples (map vector chars-1 chars-2)
        reversed-end (reduce find-start "" char-tuples)
        ordered-end (reverse reversed-end)
        ordered-str (apply str ordered-end)]
    (if (= ordered-str STRING-DELIM)
      ""
      ordered-str)))

(comment
  (get-middle "1234567890" 3 5)
  ;"45"
  )
(defn get-middle [str-text start-length end-pos]
  (if (= end-pos 0)
    ""
    (subs str-text start-length end-pos)))

(comment
  (plain-difference "abcd123edfg" "abcd987edfg" "abcd" "edfg")
  ; ["123" "987"]
  )
(defn plain-difference [str-1 str-2 start-same end-same]
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
  (plain-prefix "abc" 2)
  ; "bc"
  )

(defn plain-prefix [front-untrimmed num-chars]
  (let [front-same (trim front-untrimmed)
        front-quote-length (.length front-same)]
    (if (> num-chars front-quote-length)
      (let [my-start-quoted (subs front-same 0 front-quote-length)]
        my-start-quoted)
      (let [start-sub (- front-quote-length num-chars)
            end-sub  front-quote-length
            my-start-quoted (subs front-same start-sub end-sub)]
        my-start-quoted))))

(comment
  (plain-postfix "abc" 2)
  ; "ab"
  )
(defn plain-postfix [back-same num-chars]
  (let [back-trimmed (trim back-same)
        front-quote-length (.length back-trimmed)]
    (if (>  num-chars front-quote-length)
      (let [my-start-quoted (subs back-trimmed 0 front-quote-length)]
        my-start-quoted)
      (let [my-start-quoted (subs back-trimmed 0  num-chars)]   ;;ok
        my-start-quoted))))

(comment
  (shrink-middle "abcdefghijklmnopqrstuvwxyz" 5)
  ; "abcde ... vwxyz"

  (shrink-middle "abcd" 2)
  ; "abcd"
  )
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
  (text-matches "1\t7" "1\"7" 2)
  ; ["1" "\t" "\"" "7"]

  (text-matches "1234567890abcdefghijkl0987654321" "1234567890zyxwvutsrqpon0987654321" 3)
  ; ["123 ... 890" "abc ... jkl" "zyx ... pon" "098 ... 321"]

  (text-matches "1234abcd0987" "1234zyxw0987" 2)
  ; ["1234" "abcd" "zyxw" "0987"]
  )
(defn text-matches [str-value-1 str-value-2 ellipsis-size]
  (let [no-quotes-1    (replace str-value-1 START-OR-END-QUOTES "")
        no-quotes-2    (replace str-value-2 START-OR-END-QUOTES "")
        start-same (common-start no-quotes-1 no-quotes-2)
        end-same (common-end no-quotes-1 no-quotes-2)
        [middle-1 middle-2] (plain-difference no-quotes-1 no-quotes-2 start-same end-same)
        start-short (shrink-middle start-same ellipsis-size)
        middle-1-short (shrink-middle middle-1 ellipsis-size)
        middle-2-short (shrink-middle middle-2 ellipsis-size)
        end-short (shrink-middle end-same  ellipsis-size)
        start-middle2-end (vector start-short middle-1-short middle-2-short end-short)]
    start-middle2-end))

(comment
  (display-rnt-slashes "aa\tbb\r\ncc\rdd\nee" "" "")
  ; "aa\\tbb\\r\\ncc\\rdd\\nee"
  )
(defn display-rnt-slashes [whitespace-str current-color whitespace-col]
  (let [tab-show (str whitespace-col "\\\\t" current-color)
        rn-show (str whitespace-col  "\\\\r\\\\n" current-color)
        r-show (str whitespace-col  "\\\\r" current-color)
        n-show (str whitespace-col  "\\\\n" current-color)
        no-tabs (replace whitespace-str #"\t" tab-show)
        no-rns (replace no-tabs #"\r\n" rn-show)
        no-rs (replace no-rns #"\r" r-show)
        no-ns (replace no-rs #"\n" n-show)]
    no-ns))

(comment

  (println (colored-prefix "ab\tc" 3 {:SAME-COLOR ANSI-MAGENTA  :WHITESPACE-COLOR ANSI-YELLOW}))
;  b\tc       "b" & "c" in magenta and yellow for tab
  )
(defn colored-prefix [start-same pre-post-size char-colors]
  (let [[_error-col _legend-col _reset-coll same-col whitespace-col] (get-colors char-colors)
        front-sandwich (plain-prefix start-same pre-post-size)
        front-sand-show (display-rnt-slashes front-sandwich same-col whitespace-col)]
    front-sand-show))

(comment
  (println (colored-postfix "a\tbc" 3 {:SAME-COLOR ANSI-RED  :WHITESPACE-COLOR ANSI-CYAN}))
  ; a\tb    "a" & "b" in red and cyan for tab
  )

(defn colored-postfix [end-same pre-post-size char-colors]
  (let [[_error-col _legend-col _reset-coll same-col whitespace-col] (get-colors char-colors)
        end-sandwich (plain-postfix end-same pre-post-size)
        end-sand-show (display-rnt-slashes end-sandwich same-col whitespace-col)]
    end-sand-show))

(comment
  (strip-quotes "\"  \"  \"")
  ; "  \"  "
  )
(defn strip-quotes [quoted-str]
  (let [trimmed-str (trim quoted-str)
        no-end-quote (replace trimmed-str END-QUOTE "")
        no-start-quote (replace no-end-quote START-QUOTE "")]
    no-start-quote))

(comment
  (build-pre-post "123" "abc" {} " " "\"")
; [" 123 " "\"abc\""]

  (build-pre-post "12" "45" {} "" "M")
  ; ["12M" "45M"]
  )
(defn build-pre-post [start-same end-same char-colors delim-start delim-end]
  (let [start-no-quote (strip-quotes start-same)
        end-no-quote (strip-quotes end-same)
        delim-start-used (if (= "" start-no-quote) "" delim-start)
        delim-end-used (if (= "" end-no-quote) "" delim-end)
        [_error-col _legend-col _reset-coll same-col whitespace-col] (get-colors char-colors)
        start-show (str delim-start-used (display-rnt-slashes start-no-quote same-col whitespace-col) delim-end-used)
        end-show (str delim-start-used (display-rnt-slashes end-no-quote same-col whitespace-col) delim-end-used)]
    [start-show end-show]))

(comment
  (bool-str-int VARIETY-RATIOS-INTEGERS "123" "1234")
  ; 123

  (bool-str-int VARIETY-BOOLEAN "true" "not-true")
  ; "true"


  (bool-str-int VARIETY-STRING "a-string" "[a-string]")
  ; "[a-string]"
  )

(defn bool-str-int [variety str-value middle-plain]
  (if (= variety VARIETY-RATIOS-INTEGERS)
    (read-string str-value)
    (if (= variety VARIETY-BOOLEAN)
      str-value
      middle-plain)))


(defn internal-divider-char  [variety-type]
  (if (or (= variety-type VARIETY-STRING)
          (= variety-type VARIETY-FLOATS)
          (= variety-type VARIETY-BIGDEC)
          (= variety-type VARIETY-RATIOS-INTEGERS))
    ""
    " "))



(defn start-end-delimiter [variety-type]
  (if (= variety-type VARIETY-STRING)
    [STRING-DELIM STRING-DELIM]
    (if (= variety-type VARIETY-BIGDEC)
      ["" "M"]
      ["" ""])))

(comment
  (char-difference "1\t7" "1\"7" 1 2  {:SAME-COLOR ANSI-YELLOW} true true)
  ; ["\"1\"" "\"1\\t7\"" "\"1\"7\"" "\"7\"" "\t" "\""]

  (char-difference "aaaXbbb" "aaaYbbb" 1 2 {} true true)
  ; ["\"aaa\"" "\"aXb\"" "\"aYb\"" "\"bbb\"" "X" "Y"]
  )

(defn char-difference  [str-value-1 str-value-2 pre-post-size ellipsis-size char-colors  vars-variety]
  (let [[start-same middle-1-plain middle-2-plain end-same] (text-matches str-value-1 str-value-2 ellipsis-size)
        [delim-start delim-end] (start-end-delimiter vars-variety)
        internal-space (internal-divider-char vars-variety)
        [error-col _legend-col _reset-coll same-col whitespace-col] (get-colors char-colors)
        front-sand-show (colored-prefix start-same pre-post-size char-colors)
        end-sand-show (colored-postfix end-same pre-post-size char-colors)
        middle-1-show (display-rnt-slashes middle-1-plain error-col whitespace-col)
        middle-2-show (display-rnt-slashes middle-2-plain error-col whitespace-col)
        extra-middle-1  (str same-col delim-start front-sand-show internal-space error-col middle-1-show internal-space same-col end-sand-show  delim-end)
        extra-middle-2  (str same-col delim-start front-sand-show internal-space error-col middle-2-show internal-space same-col end-sand-show  delim-end)
        [start-show str-end-show] (build-pre-post start-same end-same char-colors delim-start delim-end)
        end-show (if (= vars-variety VARIETY-BOOLEAN) "" str-end-show)
        middle-11-plain (bool-str-int vars-variety str-value-1 middle-1-plain)
        middle-22-plain (bool-str-int vars-variety str-value-2 middle-2-plain)
        char-all (vector start-show extra-middle-1 extra-middle-2 end-show middle-11-plain middle-22-plain)]
    char-all))

(comment
  (colored-differences "abc" "def" "|START|abc" "|DIFF1|bcXde" "|DIFF2|bcYde" "|  END|def" "X" "Y")
  ; ["|START|abc|DIFF1|bcXde|DIFF2|bcYde|  END|def" "X" "Y"]
  )
(defn colored-differences [start-show end-show front-line middle-1-line middle-2-line back-line middle-1-plain middle-2-plain]
  (let  [front-empty (= 0 (.length start-show))
         back-empty (= 0 (.length end-show))]
    (if (and front-empty back-empty)
      [middle-1-plain middle-2-plain (str middle-1-line middle-2-line)]
      (if (and front-empty (not back-empty))
        [middle-1-plain middle-2-plain (str  middle-1-line middle-2-line back-line)]
        (if (and (not front-empty) back-empty)
          [middle-1-plain middle-2-plain (str front-line middle-1-line middle-2-line)]
          [middle-1-plain middle-2-plain (str front-line middle-1-line middle-2-line back-line)])))))


(comment
    (string-to-str "string")
  ; "\"string\" "
  )
(defn string-to-str [a-string]
  (str STRING-DELIM a-string STRING-DELIM " "))




















(declare variable-to-str)

(comment
  (function-to-str (defn a-func [para] (+ para 1)))
  ; "A_Function"
  )
(defn function-to-str [a-function]
  (str FUNCTION-REPRESENTATION))

(comment
  (vector-to-str [1 "two" {:three '(4 ["5"])}])
  ; " [1  \"two\"  { :three  '(4 [\"5\"]) }  ]"
  )
(defn vector-to-str [a-vector]
  (let [vector-members (reduce variable-to-str "" a-vector)]
    (str " [" vector-members " ]")))

(comment
  (map-to-str {:c "three" :b {:two '(4 ["5"])} :a 1})
  ;"  { :a 1  :b  { :two  '(4 [\"5\"]) }  :c \"three\" } "
  )
(defn map-to-str [a-map]
  (let [map-sorted (into (sorted-map) (sort-by first (seq a-map)))
        map-members (reduce variable-to-str "" map-sorted)]
    (str " {" map-members "} ")))

(comment

  (println (str "xxx" #{7 9} "yyy"))

  (set-to-str #{1 7 3})
  ;"  { :a 1  :b  { :two  '(4 [\"5\"]) }  :c \"three\" } "
  )
(defn set-to-str [a-set]
  (let [set-sorted (sort a-set)
        set-str (str set-sorted)
        no-start-list (replace set-str #"\(" "")
        no-end-list (replace no-start-list #"\)" "")]
    (str " #{" no-end-list "} ")))

(comment
  (list-to-str '(1  "two" '(3 ["4"])))
  ; " '(1 \"two\" (quote (3 [\"4\"]))) "
  )
(defn list-to-str [a-list]
  (str " '" a-list " "))

(comment
  (entry-to-str (first {:a "aa"}))
  ; " :a \"aa\" "
  )
(defn entry-to-str [an-entry]
  (if (string? (val an-entry))
    (str " " (key an-entry) " " STRING-DELIM (val an-entry) STRING-DELIM " ")
    (variable-to-str (str " " (key an-entry) " ") (val an-entry))))

(comment
  (string-to-str "17")
  ; "\"17\""

  (string-to-str "Steel's")
  ; " \"Steel's\" "
  )

(defn boolean-to-str [a-boolean]
  (if a-boolean
    "true"
    "false"))

(comment
  (other-to-str 17)
  ; "17 "
  )
(defn other-to-str [a-variable]
  (str a-variable " "))

(comment
  (nil-to-str nil)
  ; "A_Nil "
  )
(defn nil-to-str [a-nil]
  (str NIL-REPRESENTATION " "))

(comment
  (let [afunc (defn a-func [a] (+ a a))]
    (is-a-function? a-func))
  ; true

  (let [bfunc (fn [b] (+ b b))]
    (is-a-function? bfunc))
  ; true

  (let [not-func "not a function"]
    (is-a-function? not-func))
  ;false
  )
(defn is-a-function? [maybe-func]
  (if (string? maybe-func)
    false
    (if (fn? maybe-func)
      true
      (try
        (if (-> maybe-func symbol resolve deref ifn?)
          true
          false)
        (catch Exception e false)))))

(comment
  (variable-to-str "" [{:a 1
                        :b "2"
                        :c ["a" 12346 "12"]
                        :d '(123 "x")}
                       :f {:a 1 :b "2"}
                       ["a" 12346 "c"]
                       '(123 "x")])
  ; " [ { :a 1  :b \"2\"  :c  [ \"a\" 12346  \"12\"  ] :d  '(123 \"x\") } :f  { :a 1  :b \"2\" }  [ \"a\" 12346  \"c\"  ] '(123 \"x\")  ]"
  )
(defn variable-to-str [accum-str the-variable]
  (if (map-entry? the-variable)
    (str accum-str (entry-to-str the-variable))
    (if (is-a-function? the-variable)
      (str accum-str (function-to-str the-variable))
      (if (vector? the-variable)
        (str accum-str (vector-to-str the-variable))
        (if (map? the-variable)
          (str accum-str (map-to-str the-variable))
          (if (set? the-variable)
            (str accum-str (set-to-str the-variable))
            (if (list? the-variable)
              (str accum-str (list-to-str the-variable))
              (if (string? the-variable)
                (str accum-str (string-to-str the-variable))
                (if (boolean? the-variable)
                  (str accum-str (boolean-to-str the-variable))
                  (if (nil? the-variable)
                    (str accum-str (nil-to-str the-variable))
                    (str accum-str (other-to-str the-variable))))))))))))

(comment
  (var-to-str {:a [1 "j" 3]})
    ;" { :a  [1 \"j\" 3  ]} "
  )
(defn var-to-str [the-variable]
  (let [str-var (variable-to-str "" the-variable)]
    str-var))

(comment
  (var-variety "string")
  ; "VARIETY-STRING"
  )
(defn var-variety [a-variable]
  (if (or (ratio? a-variable) (integer? a-variable))
    VARIETY-RATIOS-INTEGERS
    (if (float? a-variable)
      VARIETY-FLOATS
      (if (string? a-variable)
        VARIETY-STRING
        (if (boolean? a-variable)
          VARIETY-BOOLEAN
          (if (decimal? a-variable)
            VARIETY-BIGDEC
            (if (map? a-variable)
              VARIETY-MAP
              (if (vector? a-variable)
                VARIETY-VECTOR
                (if (list? a-variable)
                  VARIETY-LIST
                  (if (is-a-function? a-variable)
                    VARIETY-FUNCTION
                    (if (nil? a-variable)
                      VARIETY-NIL
                      VARIETY-OTHER)))))))))))

(comment

  (similar-type-diff "123456789" "123456788" {} VARIETY-STRING  1 3)
; ["|START|\"123 ... 678\"\n|DIFF1|\"89\"\n|DIFF2|\"88\"" "9" "8"]


  (similar-type-diff "123456+a+567890" "123456+b+567890"  {} VARIETY-STRING  1 1)
; ["|START|\"1 ... +\"\n|DIFF1|\"+a+\"\n|DIFF2|\"+b+\"\n|  END|\"+ ... 0\"" "a" "b"]


  (similar-type-diff "123456+a+567890" "123456+b+567890"  {} VARIETY-STRING  1 2)
; ["|START|\"12 ... 6+\"\n|DIFF1|\"+a+\"\n|DIFF2|\"+b+\"\n|  END|\"+5 ... 90\"" "a" "b"]

  (similar-type-diff "123456+a+567890" "123456+b+567890"  {} VARIETY-STRING  1 3)
; ["|START|\"123 ... 56+\"\n|DIFF1|\"+a+\"\n|DIFF2|\"+b+\"\n|  END|\"+56 ... 890\"" "a" "b"]
  )

(defn similar-type-diff [value-1 value-2   char-colors vars-variety pre-post-size ellipsis-size]
  (let  [str-value-1  (trim (var-to-str value-1))
         str-value-2  (trim (var-to-str value-2))]
    (if (= str-value-1 str-value-2)
      ["" "" ""]
      (let [[start-show middle-1-show middle-2-show end-show middle-1-plain middle-2-plain]
            (char-difference str-value-1 str-value-2 pre-post-size ellipsis-size char-colors  vars-variety)
            [error-col reset-col legend-col same-col] (get-colors char-colors)
            middle-1-color (str error-col middle-1-show reset-col)
            middle-2-color (str error-col middle-2-show reset-col)
            middle-1-bool (str error-col middle-1-plain reset-col)
            middle-2-bool (str error-col middle-2-plain reset-col)
            new-middle-1 (if (= vars-variety VARIETY-BOOLEAN) middle-1-bool middle-1-color)
            new-middle-2 (if (= vars-variety VARIETY-BOOLEAN) middle-2-bool middle-2-color)
            front-line      (str legend-col "|START|" same-col start-show reset-col)
            middle-1-line (str legend-col "\n|DIFF1|" new-middle-1)
            middle-2-line (str legend-col "\n|DIFF2|" new-middle-2)
            back-line     (str legend-col "\n|  END|" same-col end-show  reset-col)
            colored-output (colored-differences start-show end-show front-line middle-1-line middle-2-line
                                                back-line middle-1-plain middle-2-plain)]
        colored-output))))

(defn prefix-duplicate-sizes [prefix-size duplicate-size]
  (if (> prefix-size duplicate-size)
    [prefix-size prefix-size]
    [prefix-size duplicate-size]))

(comment
  (short-type 1/2)
  ; "Ratio "
  )
(defn short-type [a-var]
  (if (nil? a-var)
    NIL-TYPE-STR
    (let [long-type (str (type a-var))
;        dot-type (re-find #"\.[^\.]*$" long-type)
          dot-type (re-find END-DOT-TYPE long-type)
          short-type (subs dot-type 1)]
      short-type)))

(comment

  (conflicting-type-diff "1" [1] {})
  ; ["\n|DIFF1|\"1\"\n|DIFF2|\"2\" " " \"1\" " " \"2\" "]


  (conflicting-type-diff "1" 1 {})
  ; ["\n|DIFF1|\"1\"\n|DIFF2|\"2\" " " \"1\" " " \"2\" "]
  )

(defn conflicting-type-diff [value-1 value-2 char-colors]
  (let [delim-1 (if (string? value-1) STRING-DELIM " ")
        delim-2 (if (string? value-2) STRING-DELIM " ")
        [error-col reset-col legend-col _same-col type-col] (get-colors char-colors)
        short-type-1 (short-type value-1)
        short-type-2 (short-type value-2)
        diff-1 (str legend-col "\n|diff1|" error-col delim-1 value-1 delim-1 " " type-col short-type-1 reset-col)
        diff-2 (str legend-col "\n|diff2|" error-col delim-2 value-2 delim-2 " " type-col short-type-2 reset-col)]
    [value-1 value-2 (str diff-1 diff-2)]))

(comment

  (pre-duplicate-post "1" "2" 1 3 {})
  ; ["\n|DIFF1|\"1\"\n|DIFF2|\"2\" " " \"1\" " " \"2\" "]

  (pre-duplicate-post 1 2 1 3 {})
  ; ["\n|DIFF1|1 \n|DIFF2| 2 " 1 2]

  (pre-duplicate-post "" "")
  ; ["" "" ""]

  (pre-duplicate-post "abcXdef" "abcYdef" 2 3 {})
  ; ["|START|\"abc\"\n|DIFF1|\"bcXde\"\n|DIFF2|\"bcYde\"\n|  END|\"def\"" "X" "Y"]

  (pre-duplicate-post "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" 1 3 {})
  ;"|START|\"abc ... hij\"\n|DIFF1|\"jXk\"\n|DIFF2|\"jYk\"\n|  END|\"klm ... rst\""  "X" "Y"]

  (pre-duplicate-post "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" 2 3 {})
; ["|START|\"abc ... hij\"\n|DIFF1|\"ijXkl\"\n|DIFF2|\"ijYkl\"\n|  END|\"klm ... rst\"" "X" "Y"]

  (pre-duplicate-post 123456789 987654321 2 3 {})
; ["\n|DIFF1| 123 ... 789 \n|DIFF2| 987 ... 321 "  123 987]

  (pre-duplicate-post "123456789" "987654321" 2 3 {})
; ["\n|DIFF1|\"123 ... 789\"\n|DIFF2|\"987 ... 321\""  "123 ... 789"  "987 ... 321"]

  (pre-duplicate-post "1234a6789" "1234b6789" 2 3 {})
; ["|START|\"1234\"\n|DIFF1|\"34a67\"\n|DIFF2|\"34b67\"\n|  END|\"6789\"" "a" "b"]

  (pre-duplicate-post "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" 1 2 {})
;  ["|START|\"ab ... ij\"\n|DIFF1|\"jXk\"\n|DIFF2|\"jYk\"\n|  END|\"kl ... st\""  "X" "Y"]

  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]
        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]]
    (pre-duplicate-post vec-1 vec-2 1 2 {}))
 ; ["" "" ""]

  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["R" 12346 "c"] '(123 "x")]
        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["R" 12946 "c"] '(123 "x")]]
    (pre-duplicate-post vec-1 vec-2 1 7 {}))
; ["|START| [ { :a  ... [\"R\" 12 \n|DIFF1| 234 \n|DIFF2| 294 \n|  END| 46 \"c\"  ... \"x\")  ] " "3" "9"]





;;;; fix above Q*bert


  (pre-duplicate-post "abcdef" "abcXYZ" 1 3 {})
  ;["def" "XYZ" "|START|\"abc\n|DIFF1|\"cdef\"\n|DIFF2|\"cXYZ\"\n|  END|\""]
  )

(defn pre-duplicate-post
  ([value-1 value-2]
   (pre-duplicate-post value-1 value-2  DEFAULT-PRE-POST-SIZE DEFAULT-ELLIPSIS-SIZE DEFAULT-CHAR-COLORS))
  ([value-1 value-2 pre-post-size]
   (pre-duplicate-post value-1 value-2  pre-post-size         DEFAULT-ELLIPSIS-SIZE DEFAULT-CHAR-COLORS))
  ([value-1 value-2 pre-post-size ellipsis-size]
   (pre-duplicate-post value-1 value-2  pre-post-size         ellipsis-size         DEFAULT-CHAR-COLORS))
  ([value-1 value-2 pre-post-size ellipsis-size char-colors]
   (let [[prefix-size duplicate-size] (prefix-duplicate-sizes pre-post-size ellipsis-size)
         variety-1 (var-variety value-1)
         variety-2 (var-variety value-2)]
     (if (= variety-1 variety-2)
       (similar-type-diff     value-1 value-2 char-colors variety-1 pre-post-size ellipsis-size)
       (conflicting-type-diff value-1 value-2 char-colors)))))

(comment

  (let [colors-only (are-vars-eq "1\t7" "1\"7")])
  ;|START|"1"
  ;|DIFF1|"1\t7"
  ;|DIFF2|"1"7"
  ;|  END|"7"

  (let [colors-only (are-vars-eq "123456789" 123455789)])
  ;|diff1|"123456789" String
  ;|diff2| 123455789  Long

  (let [colors-only (are-vars-eq "1234567891111111111"
                                 "1234567881111111111")])
  ;|START|"12345678"
  ;|DIFF1|"78911"
  ;|DIFF2|"78811"
  ;|  END|"1111111111"

  (let [colors-only (are-vars-eq "a1b" "a2b")])
  ;|START|"a"
  ;|DIFF1|"a1b"
  ;|DIFF2|"a2b"
  ;|  END|"b"

  (let [colors-only (are-vars-eq [{:d 1
                                   :c "2"
                                   :b ["a" 12346 "c"]
                                   :a '(123 "x")}
                                  :j {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]
                                 [{:a '(123 "x")
                                   :b ["a" 12346 "c"]
                                   :c "2"
                                   :d 1}
                                  :j {:a 1 :b 2} ["a" 19346 "c"] '(123 "x")])])
  ;|START|[ { :a  '(123 "x")  :b  ["a"  ... 1 } :j  { :a 1  :b 2 }  ["a" 1
  ;|DIFF1| 1 2 34
  ;|DIFF2| 1 9 34
  ;|  END|346 "c"  ] '(123 "x")  ]


  (let [colors-only (are-vars-eq  [123 "456"]  ["123" 456])])
  ;|START|[
  ;|DIFF1|[ 123 "456" ]
  ;|DIFF2|[ "123" 456 ]
  ;|  END|]

  (let [colors-only (are-vars-eq  [123]  ["123"])])
  ;|START|[
  ;|DIFF1|[ 123 ]
  ;|DIFF2|[ "123" ]
  ;|  END|]

  (let [colors-only (are-vars-eq 5 6)])
  ;|DIFF1|5
  ;|DIFF2|6

  (let [colors-only (are-vars-eq [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/3}]}])]
                                 [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/4}]}])])])
  ;|START| [ { :a "a" } 1  '("2" [1 2 3 {:z 123, :arr [1 "2" 3 {:er 1/
  ;|DIFF1| 1/3}]
  ;|DIFF2| 1/4}]
  ;|  END| }]}])  ]

  (are-vars-eq (defn a-func [a] (+ a a)) (defn b-func [b] (+ b b)))
 ; ["" "" "" "" ""]

  (are-vars-eq (fn [a] (+ a a)) (fn [b] (+ b b)))
  ; ["" "" "" "" ""]
  )

(defn are-vars-eq
  ([value-1 value-2]
   (are-vars-eq value-1 value-2 REPL-ERROR-PRINT DEFAULT-PRE-POST-SIZE DEFAULT-ELLIPSIS-SIZE DEFAULT-CHAR-COLORS))
  ([value-1 value-2 repl-print]
   (are-vars-eq value-1 value-2 repl-print       DEFAULT-PRE-POST-SIZE DEFAULT-ELLIPSIS-SIZE DEFAULT-CHAR-COLORS))
  ([value-1 value-2 repl-print pre-post-size]
   (are-vars-eq value-1 value-2 repl-print       pre-post-size         DEFAULT-ELLIPSIS-SIZE DEFAULT-CHAR-COLORS))
  ([value-1 value-2 repl-print pre-post-size ellipsis-size]
   (are-vars-eq value-1 value-2 repl-print       pre-post-size         ellipsis-size         DEFAULT-CHAR-COLORS))
  ([value-1 value-2 repl-print pre-post-size ellipsis-size char-colors]
   (let [print-errors? (not (= repl-print NO-REPL-PRINT))
         [plain-1-diff plain-2-diff color-diff] (pre-duplicate-post value-1 value-2 pre-post-size ellipsis-size char-colors)
         no-eols-mess (replace color-diff #"\n" TEXT-N-EOL)
         no-escape-mess (replace no-eols-mess REAL-ANSI-COLOR TEXT-ANSI-COLOR)
         no-ansi-mess (replace no-escape-mess MATCH-ANSI-COLOR "")
         values-not-equal? (not (= value-1 value-2))]
     (if (and print-errors? values-not-equal?)
       (println color-diff))
     [plain-1-diff plain-2-diff  no-escape-mess no-ansi-mess color-diff])))
