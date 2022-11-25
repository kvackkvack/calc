# calc

`calc` is an experimental functional programming language and/or proof assistant and/or a simple, natural, extensible tool for calculating, simplifying, graphing, experimenting with mathematical expressions and objects (à la a very mature Desmos).

`calc` is written in PureScript, for the web. Currently it is little more than a parser combinator with inbuilt support for syntax highlighting using an abstract document object instead of strings so that mathematical expressions can be integrated smoothly (with things like an extended alphabet including greek letters and blackboard and curly style + exponents and subscripts and integrands + smart brackets + etc. etc. supported natively). There are also the beginnings of a webapp editor for an implementation of language.