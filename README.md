﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿﻿# clojure-text-diffColored short diffs for Clojure tests### To Use```(ns name-space   (:require [text-diff :refer [are-vars-eq]])  (:require [clojure.test :refer :all])  )```****```(let [html-1 "<same>DIFFERENT</same>"      html-2 "<same>different</same>"      [diff-1 diff-2] (are-vars-eq html-1 html-2)]   (is (= diff-1 diff-2)))```                >|START|<same>                >|DIFF1|e>DIFFERENT</                >|DIFF2|e>different</                >|  END|</same>                >FAIL in () (form-init6197705762988680828.clj:_202)                >expected (= diff-1 diff-2)                >actual (not (= "DIFFERENT" "different"))****```(let [html-1 "<div>123<div>"      html-2 "<div>123<div>"      [text-diff-1 text-diff-2] (are-vars-eq html-1 html-2)]   (is (= text-diff-1 text-diff-2)))```                >true****```(let [html-1 "abcdefghijklmnopqrstuvwxyzDIFFERENTabcdefghijklmnopqrstuvwxyz"      html-2 "abcdefghijklmnopqrstuvwxyzdifferentabcdefghijklmnopqrstuvwxyz"      [diff-1 diff-2] (are-vars-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]  (is (= diff-1 diff-2)))```                >|START|abc ... xyz                >|DIFF1|zDIF ... ENTa                >|DIFF2|zdif ... enta                >|  END|abc ... xyz                >FAIL in () (form-init1629240743054083851.clj:363)                >expected (= diff-1 diff-2)                >actual (not (= "DIF ... ENT" "dif ... ent"))****```(let [html-1 "abcdefghijklmnopqrstuvwxyz_1_abcdefghijklmnopqrstuvwxyz"      html-2 "abcdefghijklmnopqrstuvwxyz_l_abcdefghijklmnopqrstuvwxyz"      [diff-1 diff-2] (are-vars-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]   (is (= diff-1 diff-2)))```                >|START|abc ... yz_                >|DIFF1|_1_                >|DIFF2|_l_                >|  END|_ab ... xyz                >FAIL in () (form-init1629240743054083851.clj:363)                >expected (= text-diff-1 text-diff-2)                >actual (not (= "1" "l"))****```(let [html-1 "abc	xyz"      html-2 "abc  xyz"      [diff-1 diff-2] (are-vars-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]  (is (= diff-1 diff-2)))```                >|START|abc                >|DIFF1|c\tx                >|DIFF2|c x                >|  END|xyz                >FAIL in () (form-init1629240743054083851.clj:363)                >expected (= diff-1 diff-2)                >actual (not (= "\t" " "))***```  (let [html-1 "qwe\r\n_asd"        html-2 "qwe\n-asd"        [diff-1 diff-2] (are-vars-eq html-1 html-2 DEFAULT-DIFF-COLORS 1 3)]    (is (= diff-1 diff-2)))```                >|START|qwe                >|DIFF1|e\r\n_                >|DIFF2|e\n-a                >|  END|asd                >FAIL in () (form-init1629240743054083851.clj:363)                >expected (= diff-1 diff-2)                >actual (not (= "\r\n_" "\n-"))***```  (let [vec-1 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 12346 "c"] '(123 "x")]        vec-2 [{:a 1 :b "2" :c ["a" 12346 "c"] :d '(123 "x")} :f {:a 1 :b 2} ["a" 19346 "c"] '(123 "x")]        [diff-1 diff-2] (are-vars-eq vec-1 vec-2)]    (is (= diff-1 diff-2)))```                >|START| [ { :a 1  :b "2"  :c  [ "a" 1 ...  } :f  { :a 1  :b 2 }  [ "a" 1                >|DIFF1| 1234                >|DIFF2| 1934                >|  END|346  "c"  ] '(123 "x")  ]                >["2" "9"]                >FAIL in () (form-init1629240743054083851.clj:363)                >expected (= diff-1 diff-2)                >actual (not (= "2" "9"))***```  (let [afunc  (defn a-func [a] (+ a a))        bfunc (defn b-func [b] (+ b b))        [diff-1 diff-2] (are-vars-eq afunc bfunc)]    (is (= diff-1 diff-2)))```                >true## To Testtest-diff-test> (do-tests "./test/test-data/")