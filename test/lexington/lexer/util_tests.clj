(ns lexington.lexer.util-tests
  (:use lexington.lexer.core
        lexington.tokens
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
        phrase :int [nil nil nil nil nil nil 7 nil]))

    (testing "Stateless Generators"
      (testing "generate-for"
        (test-generator (-> lex
                          (retain :number)
                          (with-int :int :only [:number])
                          (generate-for :number
                            :sqrt (comp #(Math/sqrt %) :int)
                            :half (comp double #(/ % 2) :int)))
          "1 4 9" #(vector (:sqrt %) (:half %)) [[1.0 0.5] [2.0 2.0] [3.0 4.5]])
        (test-generator (-> lex 
                          (discard :number)
                          (generate-for [:lower-word :upper-word]
                            :str        #(.toLowerCase (apply str (token-data %)))
                            :vowels     (comp count #(filter #{\a \e \i \o \u} %) :str)
                            :consonants (comp count #(filter (comp not #{\a \e \i \o \u}) %) :str)))
          phrase #(vector (:vowels %) (:consonants %)) [[1 2] [1 2] [1 5] [3 2] [1 1] [2 3] [1 4]]))
      (testing "generate"
        (test-generator (-> lex
                          (generate :classification
                            :lower-word (constantly :word)
                            :upper-word (constantly :word)
                            :number     (constantly :num)))
          phrase :classification [:word :word :word :word :word :word :num :word])
        (test-generator (-> lex
                          (generate :classification
                            [:lower-word :upper-word] (constantly :word)
                            :number (constantly :num)))
          phrase :classification  [:word :word :word :word :word :word :num :word])))

    (testing "Stateful Generators"
      (test-generator (-> lex 
                        (retain :number)
                        (with-int :int)
                        (generate-stateful :sum-so-far 0
                          :number #(+ (:int %1) %2))
                        (generate-stateful :product-so-far 1
                          :number #(* (:int %1) %2)))
        "1 2 3" #(vector (:sum-so-far %) (:product-so-far %)) [[1 1] [3 2] [6 6]])
      (test-generator (-> lex
                        (retain :number)
                        (with-int :int)
                        (generate-stateful :previous-number nil
                          :number { :handle (fn [_ n] n)
                                    :after (fn [n _] (:int n)) }))
        "1 2 3" :previous-number [nil 1 2])
      (test-generator (-> lex
                        (discard :number)
                        (with-string :str)
                        (generate-stateful :previous-word nil
                          [:lower-word :upper-word] { :handle (fn [_ n] n)
                                                      :after (fn [n _] (:str n)) }))
        phrase :previous-word [nil "get" "the" "STRING" "value" "of" "these"])
    )
))
