(ns lexer.util-tests
  (:use lexington.lexer
        lexington.tokens
        lexington.utils.lexer
        clojure.test
        [clojure.string :as string :only [split]]))

(deftest token-seq-filters
  (let [lex (lexer :space " "
                   :tab   "\t"
                   :word  #"[a-z]+")]
    (testing "Discard Tokens by Type"
      (let [discard-lex (-> lex (discard :space :tab))
            tokens (discard-lex "i dont\tneed whitespaces")]
        (is (not (some #{:space :tab} (map token-type tokens))))))
    (testing "Retain Tokens by Type"
      (let [retain-lex (-> lex (retain :word))
            tokens (retain-lex "i still\tonly\tneed words")]
        (is (not (some (comp not #{:word}) (map token-type tokens))))))))

(defmacro test-generator
  [lex phrase f expectation]
  `(let [tokens# (~lex ~phrase)]
     (is (= ~expectation (map ~f tokens#)))))

(deftest token-data-generators
  (let [lex (-> (lexer :ws         #" |\t|\r|\n"
                       :lower-word #"[a-z]+"
                       :upper-word #"[A-Z]+"
                       :number     #"[0-9]+")
              (discard :ws))
        phrase "get the STRING value of these 7 WORDS"]

    (testing "Predefined Generators"
      (test-generator (-> lex (with-string :str)) 
        phrase :str (string/split phrase #" "))
      (let [int-lex (-> lex (with-int :int))]
        (test-generator int-lex 
          "01 02 03 04" :int [1 2 3 4])
        (let [tokens (int-lex "01 two 03 04")]
          (is (thrown? NumberFormatException (doall tokens))))))

    (testing "Predefined Generators with :exclude or :only"
      (test-generator (-> lex (with-string :str :exclude [:number :upper-word])) 
        phrase :str ["get" "the" nil "value" "of" "these" nil nil])
      (test-generator (-> lex (with-string :str :only [:upper-word]))
        phrase :str [nil nil "STRING" nil nil nil nil "WORDS"])
      (test-generator (-> lex (with-int :int :only [:number]))
        phrase :int [nil nil nil nil nil nil 7 nil])
      (test-generator (-> lex (with-int :int :exclude [:lower-word :upper-word]))
        phrase :int [nil nil nil nil nil nil 7 nil]))))




