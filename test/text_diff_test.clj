
  ; How to use
  ; test-text-diff> (do-tests)

(ns text-diff-test
  (:require [clojure.test :refer :all])
  (:require [text-diff :refer :all]))

(defn display-if-error [var-a var-b actual-res expect-res]
  (if (not (= actual-res expect-res))
    (do
      (println "\nVar-A" var-a)
      (println "Var-B" var-b)
      (println "\nActual-Res")
      (println  actual-res)
      (println "\nExpected-Res")
      (println  expect-res  "\n"))))

(defn test-pre-duplicate-post [var-a var-b expect-res pre-post-size ellipsis-size]
  (let [[_ _ actual-res] (pre-duplicate-post var-a var-b pre-post-size ellipsis-size T-NO-ANSI-COLORS)]
    (display-if-error var-a var-b actual-res expect-res)
    actual-res))

(defn build-test-pre-duplicate-post [relative-data-dir]
  (let [check-actual (fn [test-number]
                       (let [test-first (str relative-data-dir "test-" test-number "-first.html")
                             test-second (str relative-data-dir "test-" test-number "-second.html")
                             expected-result (str relative-data-dir "test-" test-number "-expected.txt")
                             file-first (slurp test-first)
                             file-second (slurp test-second)
                             file-expected (slurp expected-result)
                             [plain-1-diff plain-2-diff actual-result] (pre-duplicate-post file-first file-second  3 30 T-NO-ANSI-COLORS)
                             actual-res (n-eoln actual-result)
                             expect-res (n-eoln file-expected)
                             test-name (str " pre-duplicate-post " test-number)]

                         (is (= actual-res expect-res))

                         (if (not (= actual-res expect-res))
                           (do
                             (println "FAIL TEST NAME : " test-name)
                             (test-pre-duplicate-post file-first file-second expect-res 3 30)))
                         [actual-res expect-res]))]
    check-actual))

(deftest tests-plain-prefix []
  (is (= (plain-diff-prefix "abc" 0)  ""))
  (is (= (plain-diff-prefix "abc" 1)  "c"))
  (is (= (plain-diff-prefix "abc" 2)  "bc"))
  (is (= (plain-diff-prefix "abc" 3)  "abc"))
  (is (= (plain-diff-prefix "abc" 4)  "abc"))
  (is (= (plain-diff-prefix "a\"" 1)  "\"")))

(deftest tests-plain-postfix []
  (is (= (plain-diff-postfix "abc" 0)  ""))
  (is (= (plain-diff-postfix "abc" 1)  "a"))
  (is (= (plain-diff-postfix "abc" 2)  "ab"))
  (is (= (plain-diff-postfix "abc" 3)  "abc"))
  (is (= (plain-diff-postfix "abc" 4)  "abc")))


;   (clojure.test/test-vars [#'text-diff-test/tests-text-diff])
(deftest tests-text-diff []
  (let [test-show-diff (build-test-pre-duplicate-post  "./test/test-data/")]
    (do
      (is (= (get-colors {:RESET-COLOR 42})
             ["" "" 42 "" "" ""]))

      (is (= (n-eoln " 1\r\n2\r3 ")
             "1\n2\n3"))

      (is (= (find-start "_" ["1" "1"])
             "_1"))

      (is (= (common-start "123" "129")
             "12"))

      (is (= (common-end "123" "423")
             "23"))

      (is (= (plain-difference "123edfg" "987edfg" "edfg")
             ["123" "987"]))

      (is (= (shrink-middle "abcd" 2)
             "abcd"))

      (is (= (shrink-middle "abcdefghijklmnopqrstuvwxyz" 5)
             "abcde ... vwxyz"))

      (is (= (text-matches "1234567890abcdefghijkl0987654321" "1234567890zyxwvutsrqpon0987654321" 3)
             ["123 ... 890" "abc ... jkl" "zyx ... pon" "098 ... 321"]))

      (is (= (text-matches "1234abcd0987" "1234zyxw0987" 2)
             ["1234" "abcd" "zyxw" "0987"]))

      (is (= (display-rnt-slashes "aa\tbb\r\ncc\rdd\nee" "" "")
             "aa\\tbb\\r\\ncc\\rdd\\nee"))

      (is (= (char-difference "aaaXbbb" "aaaYbbb" 1 2 T-NO-ANSI-COLORS VARIETY-STRING)
             ["\"aaa\"" "\"aXb\"" "\"aYb\"" "\"bbb\"" "X" "Y"]))

      (is (= (char-difference "1234567" "1230567" 1 2 T-NO-ANSI-COLORS  VARIETY-RATIOS-INTEGERS)
             ["123" "345" "305" "567" 1234567 1230567])))

    (test-show-diff 1)
    (test-show-diff 2)        ; tab looks like 4 spaces
    (test-show-diff 3)
    (test-show-diff 4)
    (test-show-diff 5)
    (test-show-diff 6)
    (test-show-diff 7)
    (test-show-diff 8)
;
    ))

(deftest extra-are-vars-eq []

  (let [va "abcdfeghijkl\rmnopqrstuvwxyz"
        vb "abcdfeghijkl\nmnopqrstuvwxyz"
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m\"abcdfeghijkl\"\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"kl\\u001b[31m\\u001b[35m\\r\\u001b[31m\\u001b[32mmn\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"kl\\u001b[31m\\u001b[35m\\n\\u001b[31m\\u001b[32mmn\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"mnopqrstuvwxyz\"\\u001b[0m"
        p-mess
        "\\n|START|\"abcdfeghijkl\"\\n|DIFF1|\"kl\\rmn\"\\n|DIFF2|\"kl\\nmn\"\\n|  END|\"mnopqrstuvwxyz\""
        e-diff ["\r" "\n"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  "11111111111"
        vb  "1111111111l"
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m\"1111111111\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"11\\u001b[31m1\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"11\\u001b[31ml\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"\\u001b[0m"
        p-mess
        "\\n|START|\"1111111111\\n|DIFF1|\"111\"\\n|DIFF2|\"11l\"\\n|  END|\""
        e-diff ["1" "l"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  {:a "a" :b "b" :c "c"}
        vb   {:b "b" :c "c" :a "X"}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ :a \\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m \" \\u001b[31ma \\u001b[32m\" \\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m \" \\u001b[31mX \\u001b[32m\" \\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m  :b \"b\"  :c \"c\" }\\u001b[0m"
        p-mess
        "\\n|START|{ :a \\n|DIFF1| \" a \" \\n|DIFF2| \" X \" \\n|  END|  :b \"b\"  :c \"c\" }"
        e-diff ["a" "X"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnXooopppqqqrrrssstttuuuvvvxxxyyyzzz"
        vb  "aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnnYooopppqqqrrrssstttuuuvvvxxxyyyzzz"
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m\"aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnn\"\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"nn\\u001b[31mX\\u001b[32moo\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"nn\\u001b[31mY\\u001b[32moo\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"ooopppqqqrrrssstttuuuvvvxxxyyyzzz\"\\u001b[0m"
        p-mess
        "\\n|START|\"aaabbbcccdddeeefffggghhhiiijjjkkklllmmmnnn\"\\n|DIFF1|\"nnXoo\"\\n|DIFF2|\"nnYoo\"\\n|  END|\"ooopppqqqrrrssstttuuuvvvxxxyyyzzz\""
        e-diff ["X" "Y"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/3}]}])]
        vb  [{:a "a"} 1 '("2" [1 2 3 {:z 123 :arr [1 "2" 3 {:er 1/4}]}])]
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m[ { :a \"a\" } 1  '(\"2\" [1 2 3 {:z 123, :arr [1 \"2\" 3 {:er 1/\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m1/ \\u001b[31m3 \\u001b[32m}]\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m1/ \\u001b[31m4 \\u001b[32m}]\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}]}])  ]\\u001b[0m"
        p-mess
        "\\n|START|[ { :a \"a\" } 1  '(\"2\" [1 2 3 {:z 123, :arr [1 \"2\" 3 {:er 1/\\n|DIFF1|1/ 3 }]\\n|DIFF2|1/ 4 }]\\n|  END|}]}])  ]"
        e-diff ["3" "4"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "abcdfeghijklmnopqrstuvwxyz"
        vb  "abcdfeghijklmnopqrstuvwxyz\n"
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m\"abcdfeghijklmnopqrstuvwxyz\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"yz\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"yz\\u001b[31m\\u001b[35m\\n\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"\\u001b[0m"
        p-mess
        "\\n|START|\"abcdfeghijklmnopqrstuvwxyz\\n|DIFF1|\"yz\"\\n|DIFF2|\"yz\\n\"\\n|  END|\""
        e-diff ["" "\n"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va 1234567890
        vb 1234567790
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m1234567\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m67\\u001b[31m8\\u001b[32m90\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m67\\u001b[31m7\\u001b[32m90\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m90\\u001b[0m"
        p-mess
        "\\n|START|1234567\\n|DIFF1|67890\\n|DIFF2|67790\\n|  END|90"
        e-diff [1234567890 1234567790]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "11"
        vb "1"
        c-mess
       "\\u001b[34m\\n|START|\\u001b[32m\"1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"1\\u001b[31m1\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"1\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"\\u001b[0m"
        p-mess "\\n|START|\"1\\n|DIFF1|\"11\"\\n|DIFF2|\"1\"\\n|  END|\""
        e-diff ["1" ""]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "1"
        vb "11"
        c-mess
"\\u001b[34m\\n|START|\\u001b[32m\"1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"1\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"1\\u001b[31m1\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"\\u001b[0m"
        p-mess "\\n|START|\"1\\n|DIFF1|\"1\"\\n|DIFF2|\"11\"\\n|  END|\""
        e-diff ["" "1"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "
"
        vb "

"
        c-mess
"\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"\\u001b[35m\\n\\u001b[32m\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"\\u001b[35m\\n\\u001b[32m\\u001b[31m\\u001b[35m\\n\\u001b[31m\\u001b[32m\"\\u001b[0m"
        p-mess "\\n|DIFF1|\"\\n\"\\n|DIFF2|\"\\n\\n\""
        e-diff ["" "\n"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "

"
        vb "
"
        c-mess
"\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"\\u001b[35m\\n\\u001b[32m\\u001b[31m\\u001b[35m\\n\\u001b[31m\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"\\u001b[35m\\n\\u001b[32m\\u001b[31m\\u001b[32m\"\\u001b[0m"
        p-mess "\\n|DIFF1|\"\\n\\n\"\\n|DIFF2|\"\\n\""
        e-diff ["\n" ""]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va 11
        vb 1
        c-mess
"\\u001b[34m\\n|START|\\u001b[32m1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m1\\u001b[31m1\\u001b[32m\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m1\\u001b[31m\\u001b[32m\\u001b[0m"
        p-mess  "\\n|START|1\\n|DIFF1|11\\n|DIFF2|1"
        e-diff [11 1]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va 1
        vb 11
        c-mess
"\\u001b[34m\\n|START|\\u001b[32m1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m1\\u001b[31m\\u001b[32m\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m1\\u001b[31m1\\u001b[32m\\u001b[0m"
        p-mess "\\n|START|1\\n|DIFF1|1\\n|DIFF2|11"
        e-diff [1 11]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "a1c"
        vb "a2c"
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m\"a\"\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"a\\u001b[31m1\\u001b[32mc\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"a\\u001b[31m2\\u001b[32mc\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m\"c\"\\u001b[0m"
        p-mess "\\n|START|\"a\"\\n|DIFF1|\"a1c\"\\n|DIFF2|\"a2c\"\\n|  END|\"c\""
        e-diff ["1" "2"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest test-are-vars-eq []

  (let [va true
        vb false
        c-mess
        "\\u001b[34m\\n|DIFF1|\\u001b[31mtrue\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31mfalse\\u001b[0m"
        p-mess "\\n|DIFF1|true\\n|DIFF2|false"
        e-diff ["true" "false"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va true
        vb true
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va 123
        vb 987
        c-mess
        "\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\\u001b[31m123\\u001b[32m\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\\u001b[31m987\\u001b[32m\\u001b[0m"
        p-mess "\\n|DIFF1|123\\n|DIFF2|987"
        e-diff [123 987]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va "abc"
        vb  "def"
        c-mess
        "\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\"\\u001b[31mabc\\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\"\\u001b[31mdef\\u001b[32m\"\\u001b[0m"
        p-mess "\\n|DIFF1|\"abc\"\\n|DIFF2|\"def\""
        e-diff ["abc" "def"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  [1 2]
        vb  [3 4]
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m[\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m[ \\u001b[31m1 2 \\u001b[32m]\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m[ \\u001b[31m3 4 \\u001b[32m]\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m]\\u001b[0m"
        p-mess "\\n|START|[\\n|DIFF1|[ 1 2 ]\\n|DIFF2|[ 3 4 ]\\n|  END|]"
        e-diff ["1 2" "3 4"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  {:a 1 :b 2}
        vb {:a 1 :b 3}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ :a 1  :b\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m:b \\u001b[31m2 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m:b \\u001b[31m3 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}\\u001b[0m"
        p-mess "\\n|START|{ :a 1  :b\\n|DIFF1|:b 2 }\\n|DIFF2|:b 3 }\\n|  END|}"
        e-diff ["2" "3"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va '(1 2 3)
        vb '(1 7 3)
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m'(1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m(1 \\u001b[31m2 \\u001b[32m3)\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m(1 \\u001b[31m7 \\u001b[32m3)\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m3)\\u001b[0m"
        p-mess "\\n|START|'(1\\n|DIFF1|(1 2 3)\\n|DIFF2|(1 7 3)\\n|  END|3)"
        e-diff ["2" "7"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va #{1 2 3}
        vb #{1 7 3}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m#{1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m{1 \\u001b[31m2 3 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m{1 \\u001b[31m3 7 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}\\u001b[0m"
        p-mess "\\n|START|#{1\\n|DIFF1|{1 2 3 }\\n|DIFF2|{1 3 7 }\\n|  END|}"
        e-diff ["2 3" "3 7"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va #{1 2 3}
        vb #{1 2 5}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m#{1 2\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m 2 \\u001b[31m3 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m 2 \\u001b[31m5 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}\\u001b[0m"
        p-mess "\\n|START|#{1 2\\n|DIFF1| 2 3 }\\n|DIFF2| 2 5 }\\n|  END|}"
        e-diff ["3" "5"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-different-type []

  (let [va  "1"
        vb  1
        c-mess
        "\\u001b[34m\\n|diff1|\\u001b[31m\"1\" \\u001b[33mString\\u001b[0m\\u001b[34m\\n|diff2|\\u001b[31m 1  \\u001b[33mLong\\u001b[0m"
        p-mess "\\n|diff1|\"1\" String\\n|diff2| 1  Long"
        e-diff ["1" 1]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  "1"
        vb  false
        c-mess
        "\\u001b[34m\\n|diff1|\\u001b[31m\"1\" \\u001b[33mString\\u001b[0m\\u001b[34m\\n|diff2|\\u001b[31m false  \\u001b[33mBoolean\\u001b[0m"
        p-mess "\\n|diff1|\"1\" String\\n|diff2| false  Boolean"
        e-diff ["1" false]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  "1"
        vb  [1]
        c-mess
        "\\u001b[34m\\n|diff1|\\u001b[31m\"1\" \\u001b[33mString\\u001b[0m\\u001b[34m\\n|diff2|\\u001b[31m [1]  \\u001b[33mPersistentVector\\u001b[0m"
        p-mess "\\n|diff1|\"1\" String\\n|diff2| [1]  PersistentVector"
        e-diff ["1" [1]]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-sets []
  (let [va  #{1 2 3}
        vb   #{1 2 3}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va  #{3 2 1}
        vb   #{1 2 3}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va  #{1 2 9}
        vb  #{1 2 8}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m#{1 2\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m 2 \\u001b[31m9 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m 2 \\u001b[31m8 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}\\u001b[0m"
        p-mess "\\n|START|#{1 2\\n|DIFF1| 2 9 }\\n|DIFF2| 2 8 }\\n|  END|}"
        e-diff ["9" "8"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-maps []

  (let [va {:a 1 :b 2 :c 3}
        vb  {:a 1 :b 2 :c 3}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va {:a 1 :b 2 :c 3}
        vb  {:c 3  :b 2 :a 1}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va {:a 1 :b 2 :c 3}
        vb  {:a 1 :b 2 :c 7}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ :a 1  :b 2  :c\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m:c \\u001b[31m3 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m:c \\u001b[31m7 \\u001b[32m}\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m}\\u001b[0m"
        p-mess "\\n|START|{ :a 1  :b 2  :c\\n|DIFF1|:c 3 }\\n|DIFF2|:c 7 }\\n|  END|}"
        e-diff ["3" "7"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-lists []

  (let [va '(1 2 3)
        vb '(1 2 3)
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va '(1 2 3)
        vb '(1 "x" 3)
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m'(1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m(1 \\u001b[31m2 \\u001b[32m3)\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m(1 \\u001b[31m\"x\" \\u001b[32m3)\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m3)\\u001b[0m"
        p-mess "\\n|START|'(1\\n|DIFF1|(1 2 3)\\n|DIFF2|(1 \"x\" 3)\\n|  END|3)"
        e-diff ["2" "\"x\""]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va '(1 2 3 4 5 6 7 8 9)
        vb '(1 2 3 4 "y" 6 7 8 9)
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m'(1 2 3 4\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m 4 \\u001b[31m5 \\u001b[32m6 \\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m 4 \\u001b[31m\"y\" \\u001b[32m6 \\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m6 7 8 9)\\u001b[0m"
        p-mess "\\n|START|'(1 2 3 4\\n|DIFF1| 4 5 6 \\n|DIFF2| 4 \"y\" 6 \\n|  END|6 7 8 9)"
        e-diff ["5" "\"y\""]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-fn []

  (let [va  (fn [a] (+ a a))
        vb  (fn [b] (+ b b))
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))
;
  )

(deftest are-vars-nil []

  (let [va nil
        vb nil
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va nil
        vb 0
        c-mess
        "\\u001b[34m\\n|diff1|\\u001b[31m   \\u001b[33mNil\\u001b[0m\\u001b[34m\\n|diff2|\\u001b[31m 0  \\u001b[33mLong\\u001b[0m"
        p-mess "\\n|diff1|   Nil\\n|diff2| 0  Long"
        e-diff [nil 0]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-ratio []

  (let [va  1/2
        vb  2/4
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va  2/1
        vb  2
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va 1/2
        vb 1
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m1\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m1\\u001b[31m/2\\u001b[32m\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m1\\u001b[31m\\u001b[32m\\u001b[0m"
        p-mess "\\n|START|1\\n|DIFF1|1/2\\n|DIFF2|1"
        e-diff [1/2 1]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-float []

  (let [va  0.5
        vb 0.50
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va 0.5
        vb 0.6
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m0.\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m0.\\u001b[31m5\\u001b[32m\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m0.\\u001b[31m6\\u001b[32m\\u001b[0m"
        p-mess "\\n|START|0.\\n|DIFF1|0.5\\n|DIFF2|0.6"
        e-diff ["5" "6"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(deftest are-vars-eq-bigdec []
  (let [va  5M
        vb 5M
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va 5M
        vb 6M
        c-mess
        "\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\\u001b[31m5\\u001b[32mM\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\\u001b[31m6\\u001b[32mM\\u001b[0m"
        p-mess "\\n|DIFF1|5M\\n|DIFF2|6M"
        e-diff ["5" "6"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

;
  )

(deftest are-vars-eq-recursize-maps []

  (let [va   {:a {:x "x"  :y "y"  :z "z"}
              :b {:1 1 :2 2 :3 3}
              :c {:red "RED" :green "GREEN" :blue "BLUE"}}
        vb {:c {:blue "BLUE" :green "GREEN" :red "RED"}
            :b {:3 3 :2 2 :1 1}
            :a {:z "z" :y "y" :x "x"}}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va  {:a {:x "x"  :y "y"  :z "z"}
             :b {:1 1 :2 2 :3 3}
             :c {:red "RED" :green "GREEN" :blue "BLUE"}}
        vb  {:c {:blue "BLUE" :green "GREEN" :red "RED"}
             :b {:3 3 :2 2 :1 1}
             :a {:z "z" :y "y" :x "x"}}
        err (are-vars-eq va vb NO-REPL-PRINT)]
    (is (= err T-NO-ERR)))

  (let [va  {:a {:x "x"  :y "y"  :z "z"}
             :b {:1 1 :2 2 :3 3}
             :c {:red "RED" :green "BLACK" :blue "BLUE"}}
        vb {:c {:blue "BLUE" :green "GREEN" :red "RED"}
            :b {:3 3 :2 2 :1 1}
            :a {:z "z" :y "y" :x "x"}}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ : ... n \\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m\" \\u001b[31mBLACK \\u001b[32m\"\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m\" \\u001b[31mGREEN \\u001b[32m\"\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m   ... } }\\u001b[0m"
        p-mess "\\n|START|{ : ... n \\n|DIFF1|\" BLACK \"\\n|DIFF2|\" GREEN \"\\n|  END|   ... } }"
        e-diff ["BLACK" "GREEN"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT 1 3)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  {:a {:x "x"  :y "y"  :z "z"}
             :b {:1 1 :2 2 :3 3}
             :c {:red "RED" :greek "GREEN" :blue "BLUE"}}
        vb {:c {:blue "BLUE" :green "GREEN" :red "RED"}
            :b {:3 3 :2 2 :1 1}
            :a {:z "z" :y "y" :x "x"}}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ :a  { :x \"x\"  :y \"y\"  :z \"z\" ... 3 }  :c  { :blue \"BLUE\"  :gree\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32mee \\u001b[31mk \\u001b[32m\"G\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32mee \\u001b[31mn \\u001b[32m\"G\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32mGREEN\"  :red \"RED\" } }\\u001b[0m"
        p-mess "\\n|START|{ :a  { :x \"x\"  :y \"y\"  :z \"z\" ... 3 }  :c  { :blue \"BLUE\"  :gree\\n|DIFF1|ee k \"G\\n|DIFF2|ee n \"G\\n|  END|GREEN\"  :red \"RED\" } }"
        e-diff ["k" "n"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))

  (let [va  {:a {:x "x"  :y "y"  :z "z"}
             :b {:1 1 :2 2 :3 3}
             :c {:r 9 :g 8 :b 7}}
        vb  {:c {:b 7 :k 6 :r 9}
             :b {:3 3 :2 2 :1 1}
             :a {:z "z" :y "y" :x "x"}}
        c-mess
        "\\u001b[34m\\n|START|\\u001b[32m{ :a  { :x \"x\"  :y \"y\"  :z \"z\" ... 1  :2 2  :3 3 }  :c  { :b 7  :\\u001b[0m\\u001b[34m\\n|DIFF1|\\u001b[31m\\u001b[32m : \\u001b[31mg 8 \\u001b[32m:r\\u001b[0m\\u001b[34m\\n|DIFF2|\\u001b[31m\\u001b[32m : \\u001b[31mk 6 \\u001b[32m:r\\u001b[0m\\u001b[34m\\n|  END|\\u001b[32m:r 9 } }\\u001b[0m"
        p-mess "\\n|START|{ :a  { :x \"x\"  :y \"y\"  :z \"z\" ... 1  :2 2  :3 3 }  :c  { :b 7  :\\n|DIFF1| : g 8 :r\\n|DIFF2| : k 6 :r\\n|  END|:r 9 } }"
        e-diff ["g 8" "k 6"]]
    (let [[d1 d2 c-res p-res e-mess] (are-vars-eq va vb NO-REPL-PRINT)]
      (is (= c-mess c-res))
      (is (= p-mess p-res))
      (is (= e-diff [d1 d2]))
      (if (not (= (str (ns-name *ns*))  "text-diff-test"))
        (println e-mess))))
;
  )

(defn do-tests []
  (run-tests 'text-diff-test)
  (println "There should be 153 assertions"))
