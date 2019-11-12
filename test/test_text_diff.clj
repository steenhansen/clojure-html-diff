
;; HOW TO TEST
;; test-text-diff> (do-tests "./test/test-data/")

(ns test-text-diff
  (:require [clojure.test :refer :all])
  (:require [text-diff :refer :all]))

(defn build-test-show-diff [relative-data-dir]
  (let [check-actual (fn [test-number]
                       (let [test-first (str relative-data-dir "test-" test-number "-first.html")
                             test-second (str relative-data-dir "test-" test-number "-second.html")
                             expected-result (str relative-data-dir "test-" test-number "-expected.txt")
                             file-first (slurp test-first)
                             file-second (slurp test-second)
                             file-expected (slurp expected-result)
                             [actual-result plain-1-diff plain-2-diff] (show-diff file-first file-second TEST-DIFF-COLORS TEST-DIFF-SANDWICH TEST-DIFF-PARTITION)
                             actual-eol (trim-to-n actual-result)
                             expected-eol (trim-to-n file-expected)
                             test-name (str "show-diff " test-number)]
                         (is (= actual-eol expected-eol))))]
    check-actual))

; test-text-diff> (do-tests "./test/test-data/")  
(defn do-tests [relative-data-dir]
  (let [test-show-diff (build-test-show-diff relative-data-dir)]
    (do
      (is (= (get-colors {:RESET-COLOR 42}) ["" 42 "" ""]))
      (is (= (trim-to-n " 1\r\n2\r3 ") "1\n2\n3"))
      (is (= (build-until-diff "_" ["1" "1"]) "_1"))
      (is (= (start-str "123" "129")   "12"))
      (is (= (end-str "123" "423")   "23"))
      (is (= (middle-diffs "abcd123edfg" "abcd987edfg" "abcd" "edfg") ["123" "987"]))
      (is (= (sandwich-start "abc" 2)  "bc"))
      (is (= (sandwich-end "abc" 2) "ab"))
      (is (= (shrink-middle "abcd" 2)   "abcd"))
      (is (= (shrink-middle "abcdefghijklmnopqrstuvwxyz" 5) "abcde ... vwxyz"))
      (is (= (text-matches "1234567890abcdefghijkl0987654321" "1234567890zyxwvutsrqpon0987654321" 3)  ["123 ... 890" "abc ... jkl" "zyx ... pon" "098 ... 321"]))
      (is (= (text-matches "1234abcd0987" "1234zyxw0987" 2) ["1234" "abcd" "zyxw" "0987"]))
      (is (= (show-chars "aa\tbb\r\ncc\rdd\nee" "" "") "aa\\tbb\\r\\ncc\\rdd\\nee"))
      (is (= (char-difference "aaaXbbb" "aaaYbbb" 1 2 {})   ["aaa" "aXb" "aYb" "bbb" "X" "Y"]))
      (is (= (show-diff "" "")     ["" "" ""]))
      (is (= (show-diff true false {})    ["\n|DIFF1|true \n|DIFF2|false \n|  END|e \n" "tru" "fals"]))
      (is (= (show-diff nil 0 {}) ["\n|DIFF1|A_Nil \n|DIFF2|0 \n|  END| \n" "A_Nil" "0"]))
      (is (= (show-diff (fn [a] (+ a a)) (fn [b] (+ b b)) {}) ["" "" ""]))
      (is (= (show-diff [1 "2" {:a 12}]
                        [1 "2" {:a 13}]
                        {})
             ["|START| [1 2 { :a 1\n|DIFF1| 12 }\n|DIFF2| 13 }\n|  END| }  ]\n" "2" "3"]))
      (is (= (show-diff [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/3}]}])]
                        [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/4}]}])]
                        {})
             ["|START| [ { :a \"a\" } 1  '(\"2\" [1 2 3 {:z 123, :arr [1 \"2\" 3 {:er 1/\n|DIFF1|1/3}]\n|DIFF2|1/4}]\n|  END|}]}])  ]\n" "3" "4"]))
      (is (= (show-diff "abcXdef" "abcYdef" {})   ["|START|abc\n|DIFF1|bcXde\n|DIFF2|bcYde\n|  END|def\n" "X" "Y"]))
      (is (= (show-diff "abcXdef" "abcYdef" {}  1)  ["|START|abc\n|DIFF1|cXd\n|DIFF2|cYd\n|  END|def\n" "X" "Y"]))
      (is (= (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 1 3) ["|START|abc ... hij\n|DIFF1|jXk\n|DIFF2|jYk\n|  END|klm ... rst\n" "X" "Y"]))
      (is (= (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 2 3) ["|START|abc ... hij\n|DIFF1|ijXkl\n|DIFF2|ijYkl\n|  END|klm ... rst\n" "X" "Y"]))
      (is (= (show-diff "abcdefghijXklmnopqrst" "abcdefghijYklmnopqrst" {} 1 2) ["|START|ab ... ij\n|DIFF1|jXk\n|DIFF2|jYk\n|  END|kl ... st\n" "X" "Y"]))
      (test-show-diff 1)
      (test-show-diff 2)
      (test-show-diff 3)
      (test-show-diff 4)
      (test-show-diff 5)
      (test-show-diff 6)
      (test-show-diff 7))))

