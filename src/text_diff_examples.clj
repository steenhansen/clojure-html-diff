
  ; How to use
  ; text-diff-examples> (diff-examples)

(ns text-diff-examples
  (:require [clojure.test :refer [is]])
  (:require [text-diff :refer [are-vars-eq]]))

(defn diff-examples []


; START OF README EXAMPLES

  (let [value-1 "abcdfeghijkl\rmnopqrstuvwxyz"
        value-2 "abcdfeghijkl\nmnopqrstuvwxyz"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]   ; /r vs /n
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|"abcdfeghijkl"
;|DIFF1|"kl\rmn"
;|DIFF2|"kl\nmn"
;|  END|"mnopqrstuvwxyz"
;expected: (= diff-1 diff-2)
;  actual: (not (= "\r" "\n"))
;expected: (= value-1 value-2)
;  actual: (not (= "abcdfeghijkl\rmnopqrstuvwxyz" "abcdfeghijkl\nmnopqrstuvwxyz"))

  (let [value-1 "11111111111"
        value-2 "1111111111l"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]    ; 1 vs l
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|"1111111111
;|DIFF1|"111"
;|DIFF2|"11l"
;|  END|"
;expected: (= diff-1 diff-2)
;  actual: (not (= "1" "l"))
;expected: (= value-1 value-2)
;  actual: (not (= "11111111111" "1111111111l"))

  (let [value-1 {:a "a" :b "b" :c "c"}
        value-2 {:b "b" :c "c" :a "X"}
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]   ; out of order maps
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|{ :a
;|DIFF1| " a "
;|DIFF2| " X "
;|  END|  :b "b"  :c "c" }
;expected: (= diff-1 diff-2)
;  actual: (not (= "a" "X"))
;expected: (= value-1 value-2)
;  actual: (not (= {:a "a", :b "b", :c "c"} {:b "b", :c "c", :a "X"}))

  (let [value-1 "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnXooopppqqqrrrssstttuuuvvvxxxyyyzzz"
        value-2 "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnYooopppqqqrrrssstttuuuvvvxxxyyyzzz"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|"aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnn"
;|DIFF1|"nnXoo"
;|DIFF2|"nnYoo"
;|  END|"ooopppqqqrrrssstttuuuvvvxxxyyyzzz"
;expected: (= diff-1 diff-2)
;  actual: (not (= "X" "Y"))
;expected: (= value-1 value-2)
;  actual: (not (= "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnXooopppqqqrrrssstttuuuvvvxxxyyyzzz" "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnYooopppqqqrrrssstttuuuvvvxxxyyyzzz"))

  (let [value-1 [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/3}]}])]
        value-2 [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/4}]}])]
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|[ { :a "a" } 1  '("2" [1 2 3 {:z 123, :arr [1 "2" 3 {:er 1/
;|DIFF1|1/ 3 }]
;|DIFF2|1/ 4 }]
;|  END|}]}])  ]
;expected: (= diff-1 diff-2)
;  actual: (not (= "3" "4"))
;expected: (= value-1 value-2)
;  actual: (not (= [{:a "a"} 1 ("2" [1 2 3 {:z 123, :arr [1 "2" 3 {:er 1/3}]}])] [{:a "a"} 1 ("2" [1 2 3 {:z 123, :arr [1 "2" 3 {:er 1/4}]}])]))

  (let [value-1 "abcdfeghijklmnopqrstuvwxyz"
        value-2 "abcdfeghijklmnopqrstuvwxyz\n"
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]       ;/n at end
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|"abcdfeghijklmnopqrstuvwxyz
;|DIFF1|"yz"
;|DIFF2|"yz\n"
;|  END|"
;expected: (= diff-1 diff-2)
;  actual: (not (= "" "\n"))
;expected: (= value-1 value-2)
;  actual: (not (= "abcdfeghijklmnopqrstuvwxyz" "abcdfeghijklmnopqrstuvwxyz\n"))

  (let [value-1 1234567890
        value-2 1234567790
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2))
    (is (= value-1 value-2)))
;|START|1234567
;|DIFF1|67890
;|DIFF2|67790
;|  END|90
;expected: (= diff-1 diff-2)
;  actual: (not (= 1234567890 1234567790))
;expected: (= value-1 value-2)
;  actual: (not (= 1234567890 1234567790))

; END OF README EXAMPLES

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

  (let [value-1 "abc	xyz"             ; tab versus
        value-2 "abc xyz"                ;     spaces
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; |START|"abc"
  ; |DIFF1|"bc\txy"
  ; |DIFF2|"bc xy"
  ; |  END|"xyz"
  ; actual (not (= "\t" " "))

  (let [value-1 "qwe\r\n_asd"    ; Windows eol versus
        value-2 "qwe\n-asd"      ; Unix eol
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; |START|"qwe"
  ; |DIFF1|"we\r\n_as"
  ; |DIFF2|"we\n-as"
  ; |  END|"asd"
  ; actual (not (= "\r\n_" "\n-"))

  (let [value-1 "123456789"
        value-2  123456789
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ;|diff1|"123456789" String
  ;|diff2| 123406789  Long
  ; actual: (not (= "123456789" 123406789))

  (let [value-1 {:a "a" :b "b" :c "c"}      ; out of order maps
        value-2 {:b "b" :c "c" :a "a"}
        [diff-1 diff-2] (are-vars-eq value-1 value-2)]
    (is (= diff-1 diff-2)))
  ; true, nothing printed to repl

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