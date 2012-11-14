lexington
=========

lexington is aimed at simplifying the creation of extensible and combinable lexers. Written in Clojure it offers a
customizable infrastructure and (so far) some predefined helper utilities. Still a work in progress, I hope it provides
at least a little bit of usefulness - and if not that, perhaps a light chuckle?

[![Build Status of Master](https://travis-ci.org/xsc/lexington.png?branch=master)](https://travis-ci.org/xsc/lexington)

## Lexer
A lexer is just a function consuming a sequence of input entities (e.g. characters) and producing a sequence of tokens 
derived from that input. Tokens are simple Clojure maps with three mandatory fields:
```clojure
(lexington.tokens/new-token :string (seq "Hello"))
; ->
{ :lexington.tokens/type   :string
  :lexington.tokens/data   (\h \e \l \l \o)
  :lexington.tokens/length 5 }
```
So, one can manually build lexers by examining input sequences by hand and creating output using `new-token` when 
needed. Or use the `lexer` macro (and its `def`-pendant `deflexer`) which associates token types with matching 
instructions:
```clojure
(ns test
  (:use lexington.lexer
        lexington.utils.lexer))

(deflexer simple-math*
  :integer #"[1-9][0-9]*"
  :plus    "+"
  :minus   "-")

; e.g. (simple-math* "1+2+3")
```
Matching instruction include:
* regular expressions
* strings
* a matching function (returning the number of input entities that match it)

This list can be extended by implementing the multi-method ``lexington.token-matchers/matcher-for``.

Lexers can include and extend other lexers:
```clojure
(deflexer simple-math-ws*
  (:include simple-math*)
  :ws #" |\t|\r|\n")

; e.g. (simple-math-ws* "1 + 2 + 3")
```
There is a problematic detail regarding the regular-expression-matching which consists of having to realize the whole 
input sequence to perform it. Avoiding this and thus taking advantage of lazy sequences should probably be in the
scope of this project.

## Lexer Logic
So far the lexer is dumb. It produces token after token until it reaches a point it can't cope with. But, for example,
we don't need whitespace in our result, so we should probably remove it; additionally it would probably be nice to
have the actual integer value of our ``:integer`` tokens in the resulting token map. We can do this:
```clojure
(def simple-math
  (-> simple-math-ws*
    (discard :ws)
    (with-string :str 
      :only [:integer])
    (generate-for :integer
      :int #(Integer/parseInt (:str %)))))
```
Have a look at the ``lexington.utils.lexer`` namespace for more possibilities and insight.

## Thoughts on Parsers

Since Clojure supports the generation of Clojure code at compile-time (via macros) it might be desirable to have 
some kind of grammar DSL and means to transform it into parser code, without the hassle of the usual 
"edit grammar"-"regenerate code"-"compile it"-cycle. This is actually what got this project started since a parser
without a usable lexer is only half the fun. Where this aspect of the project goes remains to be seen.
