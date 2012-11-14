(ns lexer.basic-tests
  (:use lexington.lexer
        lexington.tokens
        clojure.test))

(deftest string-lexer
  (let [slex (lexer :i     "i"
                    :love  "love"
                    :hate  "hate"
                    :you   "you"
                    :space " ")]
    (testing "Basic String Lexing"
      (let [tokens (slex "i love you")]
        (is (= (count tokens) 5))
        (is (= [:i :space :love :space :you] 
               (map token-type tokens)))
        (is (= ["i" " " "love" " " "you"] 
               (map #(apply str (token-data %)) tokens))))
    (testing "Lazy String Lexing on an infinite Seq"
      (let [n 10 c \i
            token-seq (slex (repeatedly (constantly c)))
            tokens (take n token-seq)]
        (is (= (count tokens) n))
        (doseq [t tokens]
          (is (= (token-type t) :i))
          (is (= (token-data t) [\i]))))))))

(deftest regex-lexer
  (let [rlex (lexer :ws      #" |\t|\r|\n"
                    :pronoun #"i|you"
                    :verb    #"love|hate"
                    :punct   #"(\.|(\!|\?)+)")]
    (testing "Basic Regex Lexing"
      (let [tokens (rlex "i hate you!!!")]
        (is (= (count tokens) 6))
        (is (= [:pronoun :ws :verb :ws :pronoun :punct] 
               (map token-type tokens)))
        (is (= ["i" " " "hate" " " "you" "!!!"] 
               (map #(apply str (token-data %)) tokens)))))))

