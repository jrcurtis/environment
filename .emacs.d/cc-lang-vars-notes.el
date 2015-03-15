

;;   c-identifier-syntax-modifications         looks like java default is ok. allows $ and @
;; C c-symbol-start                            took the java default, and added $ as well
;; ? c-symbol-chars                            leaving this as default. wonder whether @ metadata should be handled by this?
;;   c-identifier-ops                          java default of . should be all we need
;; C c-after-id-concat-ops                     original haxe mode had this set to nil, but haxe 3 allows import wildcards, so keeping the java default of allowing * makes sense
;;   c-string-escaped-newlines                 don't think this is allowed. java default kept.
;;   c-multiline-string-start-char             don't think this exists either.
;; C c-opt-cpp-symbol                          haxe mode didn't have this set, but did have c-opt-cpp-prefix set. c++ has both of them set, so i'm setting both of them for safety i guess
;; C c-opt-cpp-prefix                          leaving the regex that haxe mode had already, which is similar to the default c++ regex, but not exactly
;; C c-anchored-cpp-prefix                     again, don't know why this is needed in addition to the last two, but i set it to the c++ default oh wellll
;;   c-opt-cpp-start                           this value is derived from previous regex by default so i left it
;; C c-cpp-message-directives                  no such thing in haxe, so set it to nil
;; C c-cpp-include-directives                  see above
;; C c-opt-cpp-macro-define                    see above
;; C c-cpp-expr-directives                     defaults to if, elif, so i changed it to if, elseif
;; C c-cpp-expr-functions                      not relevant in haxe, but i also think maybe i shouldn't worry about any of these cpp things and simplifiy by setting only the ones that actually are needed
;;   c-assignment-operators                    guess: don't worry
;; ? c-operators                               this mostly looks good but i wonder if -> is actually used anywhere in haxe and i wonder if => should be included here because it is used in map literals AND extractors
;;   c-overloadable-operators                  haxe mode sets this to nil.
;;   c-opt-op-identifier-prefix                see above.
;; ? c-other-op-syntax-tokens                  not at all sure what this one is actually for
;;   c-line-comment-starter                    leave it
;;   c-block-comment-starter                   leave it
;;   c-block-comment-ender                     leave it
;;   c-doc-comment-start-regexp                leave it
;;   c-primitive-type-kwds                     haxe mode sets this to nil because all types in the language can be recognized by starting with a capital letter
;;   c-primitive-type-prefix-kwds              haxe doesn't do anything like this so the java default is ok
;; ? c-typedef-kwds                            typedef is a keyword in haxe, but it works with an assignment syntax so i don't think this needs to be addressed
;;   c-type-prefix-kwds                        don't think this applies
;;   c-type-modifier-kwds                      don't think this applies
;;   c-class-decl-kwds                         haxe mode seems to have this taken care of
;; ? c-brace-list-decl-kwds                    haxe mode has enum here and in c-class-decl-kwds, but i don't think it's necessary here. i'm not totally sure what "brace list" means but i assume it's a comma separated list. i don't think enums can be declared with commas, but anonymous structures can, but the point of anonymous structures is that there's no keyword 
;;   c-other-block-decl-kwds                   don't think this applies
;;   c-typedef-decl-kwds                       typedef is listed in c-class-decl-kwds so maybe not needed here
;;   c-typeless-decl-kwds                      function and var are probably all
;;   c-modifier-kwds                           seems correct
;; C c-other-decl-kwds                         changed this to include import, package, and using. they were already listed in c-ref-list-kwds, but the docs for this seem to suggest they should be in both
;; C c-decl-start-kwds                         setting this to include all the c-class-decl-kwds and function because they can be defined as expressions, at least in macros if not normal code
;; ? c-decl-hangon-kwds                        i don't think this helps the problem with functions with return types. haxe mode sets it to "in", but i don't see how that fits.
;;   c-protection-kwds                         this is for label syntax. doesn't apply to haxe
;;   c-block-decls-with-vars                   this assumes c style syntax where the type precedes the variable name, so it doesn't apply
;; C c-postfix-decl-spec-kwds                  haxe mode left the java default, but haxe doesn't technically haxe "throws". not a big deal. 
;;   c-nonsymbol-sexp-kwds                     don't think this applies
;;   c-type-list-kwds                          dunno whatever
;;   c-ref-list-kwds                           haxe mode default stuff
;; C c-colon-type-list-kwds                    as far as i can tell, this is where i want to say that function and var have types after colons
;; ? c-colon-type-list-re                      for a var, there's basically just the identifier, then the colon, but for a function, all manner of things appear before the colon. this might be tricky.
;;   c-paren-nontype-kwds                      don't think this applies
;;   c-paren-type-kwds                         don't think this applies
;;   c-<>-type-kwds                            haxe type parameter lists follow the type name, rather than any keyword so i don't think this is relevant
;;   c-<>-arglist-kwds                         see above
;;   c-brace-id-list-kwds                      ok
;;   c-block-stmt-1-kwds                       haxe mode default
;;   c-block-stmt-1-2-kwds                     haxe leaves the java default of "try". i don't think try can have a paren expression in haxe, but it probably doesn't matter
;;   c-block-stmt-2-kwds                       haxe default
;;   c-simple-stmt-kwds                        haxe default
;;   c-paren-stmt-kwds                         haxe default
;;   c-asm-stmt-kwds                           java default
;;   c-case-kwds                               java default
;;   c-label-kwds                              java default
;;   c-before-label-kwds                       haxe doesn't have loop labels, but the java default doesn't hurt
;;   c-constant-kwds                           haxe mode default
;;   c-primary-expr-kwds                       haxe mode default
;; ? c-lambda-kwds                             i set this to "function" which makes good sense. originally i also had function in c-decl-start-keywords
;; C c-inexpr-block-kwds                       if, switch, try, can be used in expression context. this was particularly causing problems with indentation on switches used in expressions.
;; ? c-inexpr-class-kwds                       i put class here because of macro class reification. i originall had it in c-decl-start-kwd, but i guess this is different.
;;   c-inexpr-brace-list-kwds                  i don't think this applies
;;   c-bitfield-kwds                           java default
;; ? c-other-kwds                              don't know for sure, but i think i should put "macro" in here..
;;   c-decl-start-colon-kwd-re                 no i don't think so
;;   c-decl-prefix-re                          let's not bother
;;   c-decl-start-re                           see above
;;   c-block-prefix-disallowed-chars           who knows
;;   c-recognize-typeless-decls                java defaults this to on
;;   c-recognize-<>-arglists                   haxe defaults this on
;;   c-enums-contain-decls                     java defaults this to true
;;   c-recognize-paren-inits                   defaults to off
;;   c-recognize-paren-inexpr-blocks           haxe allows blocks in expressions, but this implies there are parentheses around the block, so no.
;;   c-recognize-colon-labels                  java mode defaults this on so i guess?


;; how to handle?
;; extern and macro keywords can precede classes and stuff
