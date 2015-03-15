(c-lang-defconst c-make-mode-syntax-table
  "Functions that generates the mode specific syntax tables.
The syntax tables aren't stored directly since they're quite large."
  t `(lambda ()
       (let ((table (make-syntax-table)))
	 (c-populate-syntax-table table)
	 ;; Mode specific syntaxes.
	 ,(cond ((or (c-major-mode-is 'objc-mode) (c-major-mode-is 'java-mode))
		 ;; Let '@' be part of symbols in ObjC to cope with
		 ;; its compiler directives as single keyword tokens.
		 ;; This is then necessary since it's assumed that
		 ;; every keyword is a single symbol.
		 `(modify-syntax-entry ?@ "_" table))
		((c-major-mode-is 'pike-mode)
		 `(modify-syntax-entry ?@ "." table)))
	 table)))

(c-lang-defconst c-identifier-syntax-modifications
  "A list that describes the modifications that should be done to the
mode syntax table to get a syntax table that matches all identifiers
and keywords as words.

The list is just like the one used in `font-lock-defaults': Each
element is a cons where the car is the character to modify and the cdr
the new syntax, as accepted by `modify-syntax-entry'."
  ;; The $ character is not allowed in most languages (one exception
  ;; is Java which allows it for legacy reasons) but we still classify
  ;; it as an identifier character since it's often used in various
  ;; machine generated identifiers.
  t    '((?_ . "w") (?$ . "w"))
  (objc java) (append '((?@ . "w"))
	       (c-lang-const c-identifier-syntax-modifications))
  awk  '((?_ . "w")))

(c-lang-defvar c-identifier-syntax-table
  (let ((table (copy-syntax-table (c-mode-var "mode-syntax-table")))
	(mods c-identifier-syntax-modifications)
	mod)
    (while mods
      (setq mod (car mods)
	    mods (cdr mods))
      (modify-syntax-entry (car mod) (cdr mod) table))
    table)
  "Syntax table built on the mode syntax table but additionally
classifies symbol constituents like '_' and '$' as word constituents,
so that all identifiers are recognized as words.")

(c-lang-defvar c-get-state-before-change-functions
	       (let ((fs (c-lang-const c-get-state-before-change-functions)))
		  (if (listp fs)
		      fs
		    (list fs)))
  "If non-nil, a list of functions called from c-before-change-hook.
Typically these will record enough state to allow
`c-before-font-lock-function' to extend the region to fontify,
and may do such things as removing text-properties which must be
recalculated.

These functions will be run in the order given.  Each of them
takes 2 parameters, the BEG and END supplied to every
before-change function; on entry, the buffer will have been
widened and match-data will have been saved; point is undefined
on both entry and exit; the return value is ignored.

The functions are called even when font locking isn't enabled.

When the mode is initialized, the functions are called with
parameters \(point-min) and \(point-max).")

(c-lang-defvar c-before-font-lock-functions
	       (let ((fs (c-lang-const c-before-font-lock-functions)))
		 (if (listp fs)
		     fs
		   (list fs)))
  "If non-nil, a list of functions called just before font locking.
Typically they will extend the region about to be fontified \(see
below) and will set `syntax-table' text properties on the region.

These functions will be run in the order given.  Each of them
takes 3 parameters, the BEG, END, and OLD-LEN supplied to every
after-change function; point is undefined on both entry and exit;
on entry, the buffer will have been widened and match-data will
have been saved; the return value is ignored.

The functions may extend the region to be fontified by setting the
buffer local variables c-new-BEG and c-new-END.

The functions are called even when font locking is disabled.

When the mode is initialized, these functions are called with
parameters \(point-min), \(point-max) and <buffer size>.")

(c-lang-defvar c-before-context-fontification-functions
  (let ((fs (c-lang-const c-before-context-fontification-functions)))
    (if (listp fs)
	fs
      (list fs)))
  "If non-nil, a list of functions called just before context (or
other non-change) fontification is done.  Typically they will
extend the region.

These functions will be run in the order given.  Each of them
takes 2 parameters, the BEG and END of the region to be
fontified.  Point is undefined on both entry and exit.  On entry,
the buffer will have been widened and match-data will have been
saved; the return value is a cons of the adjusted
region, (NEW-BEG . NEW-END).")

;;; Syntactic analysis ("virtual semicolons") for line-oriented languages (AWK).
(c-lang-defconst c-at-vsemi-p-fn
  "Contains a function \"Is there a virtual semicolon at POS or point?\".
Such a function takes one optional parameter, a buffer position (defaults to
point), and returns nil or t.  This variable contains nil for languages which
don't have EOL terminated statements. "
  t nil
  (c c++ objc) 'c-at-macro-vsemi-p
  awk 'c-awk-at-vsemi-p)

(c-lang-defconst c-vsemi-status-unknown-p-fn
  "Contains a function \"are we unsure whether there is a virtual semicolon on this line?\".
The (admittedly kludgy) purpose of such a function is to prevent an infinite
recursion in c-beginning-of-statement-1 when point starts at a `while' token.
The function MUST NOT UNDER ANY CIRCUMSTANCES call c-beginning-of-statement-1,
even indirectly.  This variable contains nil for languages which don't have
EOL terminated statements."
  t nil
  (c c++ objc) 'c-macro-vsemi-status-unknown-p
  awk 'c-awk-vsemi-status-unknown-p)

;;; Lexer-level syntax (identifiers, tokens etc).

(c-lang-defconst c-has-bitfields
  "Whether the language has bitfield declarations."
  t nil
  (c c++ objc) t)

(c-lang-defconst c-symbol-start
  "Regexp that matches the start of a symbol, i.e. any identifier or
keyword.  It's unspecified how far it matches.	Does not contain a \\|
operator at the top level."
  t    (concat "[" c-alpha "_]")
  java (concat "[" c-alpha "_@]")
  objc (concat "[" c-alpha "_@]")
  pike (concat "[" c-alpha "_`]"))

(c-lang-defconst c-symbol-chars
  "Set of characters that can be part of a symbol.
This is of the form that fits inside [ ] in a regexp."
  ;; Pike note: With the backquote identifiers this would include most
  ;; operator chars too, but they are handled with other means instead.
  t    (concat c-alnum "_$")
  objc (concat c-alnum "_$@"))

(c-lang-defconst c-symbol-key
  "Regexp matching identifiers and keywords (with submatch 0).  Assumed
to match if `c-symbol-start' matches on the same position."
  t    (concat (c-lang-const c-symbol-start)
	       "[" (c-lang-const c-symbol-chars) "]*")
  pike (concat
	;; Use the value from C here since the operator backquote is
	;; covered by the other alternative.
	(c-lang-const c-symbol-key c)
	"\\|"
	(c-make-keywords-re nil
	  (c-lang-const c-overloadable-operators))))

(c-lang-defconst c-nonsymbol-chars
  "This is the set of chars that can't be part of a symbol, i.e. the
negation of `c-symbol-chars'."
  t (concat "^" (c-lang-const c-symbol-chars)))

(c-lang-defconst c-nonsymbol-key
  "Regexp that matches any character that can't be part of a symbol.
It's usually appended to other regexps to avoid matching a prefix.
It's assumed to not contain any submatchers."
  ;; The same thing regarding Unicode identifiers applies here as to
  ;; `c-symbol-key'.
  t (concat "[" (c-lang-const c-nonsymbol-chars) "]"))

(c-lang-defconst c-identifier-ops
  "The operators that make up fully qualified identifiers.  nil in
languages that don't have such things.  See `c-operators' for a
description of the format.  Binary operators can concatenate symbols,
e.g. \"::\" in \"A::B::C\".  Prefix operators can precede identifiers,
e.g. \"~\" in \"~A::B\".  Other types of operators aren't supported.

This value is by default merged into `c-operators'."
  t    nil
  c++  '((prefix "~" "??-" "compl")
	 (right-assoc "::")
	 (prefix "::"))
  ;; Java has "." to concatenate identifiers but it's also used for
  ;; normal indexing.  There's special code in the Java font lock
  ;; rules to fontify qualified identifiers based on the standard
  ;; naming conventions.  We still define "." here to make
  ;; `c-forward-name' move over as long names as possible which is
  ;; necessary to e.g. handle throws clauses correctly.
  java '((left-assoc "."))
  idl  '((left-assoc "::")
	 (prefix "::"))
  pike '((left-assoc "::")
	 (prefix "::")
	 (left-assoc ".")))

(c-lang-defconst c-after-id-concat-ops
  "Operators that can occur after a binary operator on `c-identifier-ops'
in identifiers.  nil in languages that don't have such things.

Operators here should also have appropriate entries in `c-operators' -
it's not taken care of by default."
  t    nil
  ;; '~' for destructors in C++, '*' for member pointers.
  c++  '("~" "*")
  ;; In Java we recognize '*' to deal with "foo.bar.*" that can occur
  ;; in import declarations.  (This will also match bogus things like
  ;; "foo.*bar" but we don't bother.)
  java '("*"))

(c-lang-defconst c-identifier-start
  "Regexp that matches the start of an (optionally qualified) identifier.
It should also match all keywords.  It's unspecified how far it
matches."
  t (concat (c-lang-const c-symbol-start)
	    (if (c-lang-const c-opt-identifier-prefix-key)
		(concat "\\|"
			(c-lang-const c-opt-identifier-prefix-key))
	      "")))

(c-lang-defconst c-identifier-key
  "Regexp matching a fully qualified identifier, like \"A::B::c\" in
C++.  It does not recognize the full range of syntactic whitespace
between the tokens; `c-forward-name' has to be used for that.  It
should also not match identifiers containing parenthesis groupings,
e.g. identifiers with template arguments such as \"A<X,Y>\" in C++."
  ;; This regexp is more complex than strictly necessary to ensure
  ;; that it can be matched with a minimum of backtracking.
  t (concat (if (c-lang-const c-opt-identifier-prefix-key)
		(concat
		 "\\("
		 (c-lang-const c-opt-identifier-prefix-key)
		 (c-lang-const c-simple-ws) "*"
		 "\\)?")
	      "")
	    "\\(" (c-lang-const c-symbol-key) "\\)"
	    (if (c-lang-const c-opt-identifier-concat-key)
		(concat
		 "\\("
		 (c-lang-const c-simple-ws) "*"
		 (c-lang-const c-opt-identifier-concat-key)
		 (c-lang-const c-simple-ws) "*"
		 (if (c-lang-const c-after-id-concat-ops)
		     (concat
		      "\\("
		       (c-make-keywords-re 'appendable
			 (c-lang-const c-after-id-concat-ops))
		      (concat
		       ;; For flexibility, consider the symbol match
		       ;; optional if we've hit a
		       ;; `c-after-id-concat-ops' operator.  This is
		       ;; also necessary to handle the "*" that can
		       ;; end import declaration identifiers in Java.
		       "\\("
		       (c-lang-const c-simple-ws) "*"
		       "\\(" (c-lang-const c-symbol-key) "\\)"
		       "\\)?")
		      "\\|"
		      "\\(" (c-lang-const c-symbol-key) "\\)"
		      "\\)")
		   (concat "\\(" (c-lang-const c-symbol-key) "\\)"))
		 "\\)*")
	      "")))

(c-lang-defconst c-string-escaped-newlines
  "Set if the language support backslash escaped newlines inside string
literals."
  t nil
  (c c++ objc pike) t)

(c-lang-defconst c-multiline-string-start-char
  "Set if the language supports multiline string literals without escaped
newlines.  If t, all string literals are multiline.  If a character,
only literals where the open quote is immediately preceded by that
literal are multiline."
  t    nil
  pike ?#)

(c-lang-defconst c-opt-cpp-symbol
  "The symbol which starts preprocessor constructs when in the margin."
  t "#"
  (java awk) nil)

(c-lang-defconst c-opt-cpp-prefix
  "Regexp matching the prefix of a cpp directive in the languages that
normally use that macro preprocessor.  Tested at bol or at boi.
Assumed to not contain any submatches or \\| operators."
  ;; TODO (ACM, 2005-04-01).  Amend the following to recognize escaped NLs;
  ;; amend all uses of c-opt-cpp-prefix which count regexp-depth.
  t "\\s *#\\s *"
  (java awk) nil)

(c-lang-defconst c-anchored-cpp-prefix
  "Regexp matching the prefix of a cpp directive anchored to BOL,
in the languages that have a macro preprocessor."
  t "^\\s *\\(#\\)\\s *"
  (java awk) nil)

(c-lang-defconst c-opt-cpp-start
  "Regexp matching the prefix of a cpp directive including the directive
name, or nil in languages without preprocessor support.  The first
submatch surrounds the directive name."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   (concat (c-lang-const c-opt-cpp-prefix)
		   "\\([" c-alnum "]+\\)"))
  ;; Pike, being a scripting language, recognizes hash-bangs too.
  pike (concat (c-lang-const c-opt-cpp-prefix)
	       "\\([" c-alnum "]+\\|!\\)"))

(c-lang-defconst c-cpp-message-directives
  "List of cpp directives (without the prefix) that are followed by a
string message."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("error"))
  (c c++ objc pike) '("error" "warning"))

(c-lang-defconst c-cpp-include-directives
  "List of cpp directives (without the prefix) that are followed by a
file name in angle brackets or quotes."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("include"))
  objc '("include" "import"))

(c-lang-defconst c-opt-cpp-macro-define
  "Cpp directive (without the prefix) that is followed by a macro
definition, or nil if the language doesn't have any."
  t (if (c-lang-const c-opt-cpp-prefix)
	"define"))


(c-lang-defconst c-cpp-expr-directives
  "List of cpp directives (without the prefix) that are followed by an
expression."
  t (if (c-lang-const c-opt-cpp-prefix)
	'("if" "elif")))

(c-lang-defconst c-cpp-expr-intro-re
  "Regexp which matches the start of a CPP directive which contains an
expression, or nil if there aren't any in the language."
  t (if (c-lang-const c-cpp-expr-directives)
	(concat
	 (c-lang-const c-opt-cpp-prefix)
	 (c-make-keywords-re t (c-lang-const c-cpp-expr-directives)))))

(c-lang-defconst c-cpp-expr-functions
  "List of functions in cpp expressions."
  t    (if (c-lang-const c-opt-cpp-prefix)
	   '("defined"))
  pike '("defined" "efun" "constant"))

(c-lang-defconst c-assignment-operators
  "List of all assignment operators."
  t    '("=" "*=" "/=" "%=" "+=" "-=" ">>=" "<<=" "&=" "^=" "|=")
  java (append (c-lang-const c-assignment-operators)
	       '(">>>="))
  c++  (append (c-lang-const c-assignment-operators)
	       '("and_eq" "or_eq" "xor_eq" "??!=" "??'="))
  idl  nil)

(c-lang-defconst c-operators
  "List describing all operators, along with their precedence and
associativity.  The order in the list corresponds to the precedence of
the operators: The operators in each element are a group with the same
precedence, and the group has higher precedence than the groups in all
following elements.  The car of each element describes the type of the
operator group, and the cdr is a list of the operator tokens in it.
The operator group types are:

'prefix         Unary prefix operators.
'postfix        Unary postfix operators.
'postfix-if-paren
		Unary postfix operators if and only if the chars have
		parenthesis syntax.
'left-assoc     Binary left associative operators (i.e. a+b+c means (a+b)+c).
'right-assoc    Binary right associative operators (i.e. a=b=c means a=(b=c)).
'right-assoc-sequence
                Right associative operator that constitutes of a
                sequence of tokens that separate expressions.  All the
                tokens in the group are in this case taken as
                describing the sequence in one such operator, and the
                order between them is therefore significant.

Operators containing a character with paren syntax are taken to match
with a corresponding open/close paren somewhere else.  A postfix
operator with close paren syntax is taken to end a postfix expression
started somewhere earlier, rather than start a new one at point.  Vice
versa for prefix operators with open paren syntax.

Note that operators like \".\" and \"->\" which in language references
often are described as postfix operators are considered binary here,
since CC Mode treats every identifier as an expression."

  ;; There's currently no code in CC Mode that exploit all the info
  ;; in this variable; precedence, associativity etc are present as a
  ;; preparation for future work.

  t `(;; Preprocessor.
      ,@(when (c-lang-const c-opt-cpp-prefix)
	  `((prefix "#"
		    ,@(when (c-major-mode-is '(c-mode c++-mode))
			'("%:" "??=")))
	    (left-assoc "##"
			,@(when (c-major-mode-is '(c-mode c++-mode))
			    '("%:%:" "??=??=")))))

      ;; Primary.
      ,@(c-lang-const c-identifier-ops)
      ,@(cond ((or (c-major-mode-is 'c++-mode) (c-major-mode-is 'java-mode))
	       `((postfix-if-paren "<" ">"))) ; Templates.
	      ((c-major-mode-is 'pike-mode)
	       `((prefix "global" "predef")))
	      ((c-major-mode-is 'java-mode)
	       `((prefix "super"))))

      ;; Postfix.
      ,@(when (c-major-mode-is 'c++-mode)
	  ;; The following need special treatment.
	  `((prefix "dynamic_cast" "static_cast"
		    "reinterpret_cast" "const_cast" "typeid")))
      (left-assoc "."
		  ,@(unless (c-major-mode-is 'java-mode)
		      '("->")))
      (postfix "++" "--" "[" "]" "(" ")"
	       ,@(when (c-major-mode-is '(c-mode c++-mode))
		   '("<:" ":>" "??(" "??)")))

      ;; Unary.
      (prefix "++" "--" "+" "-" "!" "~"
	      ,@(when (c-major-mode-is 'c++-mode) '("not" "compl"))
	      ,@(when (c-major-mode-is '(c-mode c++-mode))
		  '("*" "&" "sizeof" "??-"))
	      ,@(when (c-major-mode-is 'objc-mode)
		  '("@selector" "@protocol" "@encode"))
	      ;; The following need special treatment.
	      ,@(cond ((c-major-mode-is 'c++-mode)
		       '("new" "delete"))
		      ((c-major-mode-is 'java-mode)
		       '("new"))
		      ((c-major-mode-is 'pike-mode)
		       '("class" "lambda" "catch" "throw" "gauge")))
	      "(" ")"			; Cast.
	      ,@(when (c-major-mode-is 'pike-mode)
		  '("[" "]")))		; Type cast.

      ;; Member selection.
      ,@(when (c-major-mode-is 'c++-mode)
	  `((left-assoc ".*" "->*")))

      ;; Multiplicative.
      (left-assoc "*" "/" "%")

      ;; Additive.
      (left-assoc "+" "-")

      ;; Shift.
      (left-assoc "<<" ">>"
		  ,@(when (c-major-mode-is 'java-mode)
		      '(">>>")))

      ;; Relational.
      (left-assoc "<" ">" "<=" ">="
		  ,@(when (c-major-mode-is 'java-mode)
		      '("instanceof")))

      ;; Equality.
      (left-assoc "==" "!="
		  ,@(when (c-major-mode-is 'c++-mode) '("not_eq")))

      ;; Bitwise and.
      (left-assoc "&"
		  ,@(when (c-major-mode-is 'c++-mode) '("bitand")))

      ;; Bitwise exclusive or.
      (left-assoc "^"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??'"))
		  ,@(when (c-major-mode-is 'c++-mode) '("xor")))

      ;; Bitwise or.
      (left-assoc "|"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??!"))
		  ,@(when (c-major-mode-is 'c++-mode) '("bitor")))

      ;; Logical and.
      (left-assoc "&&"
		  ,@(when (c-major-mode-is 'c++-mode) '("and")))

      ;; Logical or.
      (left-assoc "||"
		  ,@(when (c-major-mode-is '(c-mode c++-mode))
		      '("??!??!"))
		  ,@(when (c-major-mode-is 'c++-mode) '("or")))

      ;; Conditional.
      (right-assoc-sequence "?" ":")

      ;; Assignment.
      (right-assoc ,@(c-lang-const c-assignment-operators))

      ;; Exception.
      ,@(when (c-major-mode-is 'c++-mode)
	  '((prefix "throw")))

      ;; Sequence.
      (left-assoc ","))

  ;; IDL got its own definition since it has a much smaller operator
  ;; set than the other languages.
  idl `(;; Preprocessor.
	(prefix "#")
	(left-assoc "##")
	;; Primary.
	,@(c-lang-const c-identifier-ops)
	;; Unary.
	(prefix  "+" "-" "~")
	;; Multiplicative.
	(left-assoc "*" "/" "%")
	;; Additive.
	(left-assoc "+" "-")
	;; Shift.
	(left-assoc "<<" ">>")
	;; And.
	(left-assoc "&")
	;; Xor.
	(left-assoc "^")
	;; Or.
	(left-assoc "|")))

(c-lang-defconst c-overloadable-operators
  "List of the operators that are overloadable, in their \"identifier
form\".  See also `c-op-identifier-prefix'."
  t    nil
  c++  '("new" "delete" ;; Can be followed by "[]" but we ignore that.
	 "+" "-" "*" "/" "%"
	 "^" "??'" "xor" "&" "bitand" "|" "??!" "bitor" "~" "??-" "compl"
	 "!" "=" "<" ">" "+=" "-=" "*=" "/=" "%=" "^="
	 "??'=" "xor_eq" "&=" "and_eq" "|=" "??!=" "or_eq"
	 "<<" ">>" ">>=" "<<=" "==" "!=" "not_eq" "<=" ">="
	 "&&" "and" "||" "??!??!" "or" "++" "--" "," "->*" "->"
	 "()" "[]" "<::>" "??(??)")
  ;; These work like identifiers in Pike.
  pike '("`+" "`-" "`&" "`|" "`^" "`<<" "`>>" "`*" "`/" "`%" "`~"
	 "`==" "`<" "`>" "`!" "`[]" "`[]=" "`->" "`->=" "`()" "``+"
	 "``-" "``&" "``|" "``^" "``<<" "``>>" "``*" "``/" "``%"
	 "`+="))

(c-lang-defconst c-opt-op-identifier-prefix
  "Regexp matching the token before the ones in
`c-overloadable-operators' when operators are specified in their
\"identifier form\".  This typically matches \"operator\" in C++ where
operator functions are specified as e.g. \"operator +\".  It's nil in
languages without operator functions or where the complete operator
identifier is listed in `c-overloadable-operators'.

This regexp is assumed to not match any non-operator identifier."
  t   nil
  c++ (c-make-keywords-re t '("operator")))

(c-lang-defconst c-other-op-syntax-tokens
  "List of the tokens made up of characters in the punctuation or
parenthesis syntax classes that have uses other than as expression
operators."
  t '("{" "}" "(" ")" "[" "]" ";" ":" "," "=" "/*" "*/" "//")
  (c c++ pike) (append '("#" "##"	; Used by cpp.
			 "::" "...")
		       (c-lang-const c-other-op-syntax-tokens))
  (c c++) (append '("*") (c-lang-const c-other-op-syntax-tokens))
  c++  (append '("&" "<%" "%>" "<:" ":>" "%:" "%:%:")
	       (c-lang-const c-other-op-syntax-tokens))
  objc (append '("#" "##"		; Used by cpp.
		 "+" "-") (c-lang-const c-other-op-syntax-tokens))
  idl  (append '("#" "##")		; Used by cpp.
	       (c-lang-const c-other-op-syntax-tokens))
  pike (append '("..")
	       (c-lang-const c-other-op-syntax-tokens)
	       (c-lang-const c-overloadable-operators))
  awk '("{" "}" "(" ")" "[" "]" ";" "," "=" "/"))

;;; Syntactic whitespace.

(c-lang-defconst c-simple-ws
  "Regexp matching an ordinary whitespace character.
Does not contain a \\| operator at the top level."
  ;; "\\s " is not enough since it doesn't match line breaks.
  t "\\(\\s \\|[\n\r]\\)")

(c-lang-defconst c-line-comment-starter
  "String that starts line comments, or nil if such don't exist.
Line comments are always terminated by newlines.  At least one of
`c-block-comment-starter' and this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "//"
  awk  "#")

(c-lang-defconst c-block-comment-starter
  "String that starts block comments, or nil if such don't exist.
Block comments are ended by `c-block-comment-ender', which is assumed
to be set if this is.  At least one of `c-line-comment-starter' and
this one is assumed to be set.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "/*"
  awk  nil)

(c-lang-defconst c-block-comment-ender
  "String that ends block comments, or nil if such don't exist.

Note that it's currently not enough to set this to support a new
comment style.  Other stuff like the syntax table must also be set up
properly."
  t    "*/"
  awk  nil)

(c-lang-defconst c-doc-comment-start-regexp
  "Regexp to match the start of documentation comments."
  t    "\\<\\>"
  ;; From font-lock.el: `doxygen' uses /*! while others use /**.
  (c c++ objc) "/\\*[*!]"
  java "/\\*\\*"
  pike "/[/*]!")

(c-lang-defconst comment-start
  "String that starts comments inserted with M-; etc.
`comment-start' is initialized from this."
  ;; Default: Prefer line comments to block comments, and pad with a space.
  t (concat (or (c-lang-const c-line-comment-starter)
		(c-lang-const c-block-comment-starter))
	    " ")
  ;; In C we still default to the block comment style since line
  ;; comments aren't entirely portable.
  c "/* ")

(c-lang-defconst comment-end
  "String that ends comments inserted with M-; etc.
`comment-end' is initialized from this."
  ;; Default: Use block comment style if comment-start uses block
  ;; comments, and pad with a space in that case.
  t (if (string-match (concat "\\`\\("
			      (c-lang-const c-block-comment-start-regexp)
			      "\\)")
		      (c-lang-const comment-start))
	(concat " " (c-lang-const c-block-comment-ender))
      ""))

(c-lang-defconst comment-start-skip
  "Regexp to match the start of a comment plus everything up to its body.
`comment-start-skip' is initialized from this."
  ;; Default: Allow the last char of the comment starter(s) to be
  ;; repeated, then allow any amount of horizontal whitespace.
  t (concat "\\("
	    (c-concat-separated
	     (mapcar (lambda (cs)
		       (when cs
			 (concat (regexp-quote cs) "+")))
		     (list (c-lang-const c-line-comment-starter)
			   (c-lang-const c-block-comment-starter)))
	     "\\|")
	    "\\)\\s *"))

;;; Defun functions

;; The Emacs variables beginning-of-defun-function and
;; end-of-defun-function will be set so that commands like
;; `mark-defun' and `narrow-to-defun' work right.  The key sequences
;; C-M-a and C-M-e are, however, bound directly to the CC Mode
;; functions, allowing optimization for large n.
(c-lang-defconst beginning-of-defun-function
  "Function to which beginning-of-defun-function will be set."
  t 'c-beginning-of-defun
  awk 'c-awk-beginning-of-defun)

(c-lang-defconst end-of-defun-function
  "Function to which end-of-defun-function will be set."
  t 'c-end-of-defun
  awk 'c-awk-end-of-defun)

;;; In-comment text handling.

(c-lang-defconst c-paragraph-start
  "Regexp to append to `paragraph-start'."
  t    "$"
  java "\\(@[a-zA-Z]+\\>\\|$\\)"	; For Javadoc.
  pike "\\(@[a-zA-Z_-]+\\>\\([^{]\\|$\\)\\|$\\)") ; For Pike refdoc.

(c-lang-defconst c-paragraph-separate
  "Regexp to append to `paragraph-separate'."
  t    "$"
  pike (c-lang-const c-paragraph-start))

;;; Keyword lists.

;; Note: All and only all language constants containing keyword lists
;; should end with "-kwds"; they're automatically collected into the
;; `c-kwds-lang-consts' list below and used to build `c-keywords' etc.

(c-lang-defconst c-primitive-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Do not try to modify this list for end user customizations; the
`*-font-lock-extra-types' variable, where `*' is the mode prefix, is
the appropriate place for that."
  t    '("char" "double" "float" "int" "long" "short" "signed"
	 "unsigned" "void")
  c    (append
	'("_Bool" "_Complex" "_Imaginary") ; Conditionally defined in C99.
	(c-lang-const c-primitive-type-kwds))
  c++  (append
	'("bool" "wchar_t")
	(c-lang-const c-primitive-type-kwds))
  ;; Objective-C extends C, but probably not the new stuff in C99.
  objc (append
	'("id" "Class" "SEL" "IMP" "BOOL")
	(c-lang-const c-primitive-type-kwds))
  java '("boolean" "byte" "char" "double" "float" "int" "long" "short" "void")
  idl  '("Object" "ValueBase" "any" "boolean" "char" "double" "fixed" "float"
	 "long" "octet" "sequence" "short" "string" "void" "wchar" "wstring"
	 ;; In CORBA PSDL:
	 "ref"
	 ;; The following can't really end a type, but we have to specify them
	 ;; here due to the assumption in `c-primitive-type-prefix-kwds'.  It
	 ;; doesn't matter that much.
	 "unsigned" "strong")
  pike '(;; this_program isn't really a keyword, but it's practically
	 ;; used as a builtin type.
	 "array" "float" "function" "int" "mapping" "mixed" "multiset"
	 "object" "program" "string" "this_program" "void"))

(c-lang-defconst c-primitive-type-prefix-kwds
  "Keywords that might act as prefixes for primitive types.  Assumed to
be a subset of `c-primitive-type-kwds'."
  t       nil
  (c c++) '("long" "short" "signed" "unsigned")
  idl     '("long" "unsigned"
	    ;; In CORBA PSDL:
	    "strong"))

(c-lang-defconst c-typedef-kwds
  "Prefix keyword\(s\) like \"typedef\" which make a type declaration out
of a variable declaration."
  t        '("typedef")
  (awk idl java) nil)

(c-lang-defconst c-type-prefix-kwds
  "Keywords where the following name - if any - is a type name, and
where the keyword together with the symbol works as a type in
declarations.

Note that an alternative if the second part doesn't hold is
`c-type-list-kwds'.  Keywords on this list are typically also present
on one of the `*-decl-kwds' lists."
  t    nil
  c    '("struct" "union" "enum")
  c++  (append '("class" "typename")
	       (c-lang-const c-type-prefix-kwds c)))

(c-lang-defconst c-type-modifier-kwds
  "Type modifier keywords.  These can occur almost anywhere in types
but they don't build a type of themselves.  Unlike the keywords on
`c-primitive-type-kwds', they are fontified with the keyword face and
not the type face."
  t    nil
  c    '("const" "restrict" "volatile")
  c++  '("const" "volatile" "throw")
  objc '("const" "volatile"))

(c-lang-defconst c-class-decl-kwds
  "Keywords introducing declarations where the following block (if any)
contains another declaration level that should be considered a class.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled.

Note that presence on this list does not automatically treat the
following identifier as a type; the keyword must also be present on
`c-type-prefix-kwds' or `c-type-list-kwds' to accomplish that."
  t    nil
  c    '("struct" "union")
  c++  '("class" "struct" "union")
  objc '("struct" "union"
	 "@interface" "@implementation" "@protocol")
  java '("class" "@interface" "interface")
  idl  '("component" "eventtype" "exception" "home" "interface" "struct"
	 "union" "valuetype"
	 ;; In CORBA PSDL:
	 "storagehome" "storagetype"
	 ;; In CORBA CIDL:
	 "catalog" "executor" "manages" "segment")
  pike '("class"))

(c-lang-defconst c-brace-list-decl-kwds
  "Keywords introducing declarations where the following block (if
any) is a brace list.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t    '("enum")
  (awk) nil)

(c-lang-defconst c-other-block-decl-kwds
  "Keywords where the following block (if any) contains another
declaration level that should not be considered a class.  For every
keyword here, CC Mode will add a set of special syntactic symbols for
those blocks.  E.g. if the keyword is \"foo\" then there will be
`foo-open', `foo-close', and `infoo' symbols.

The intention is that this category should be used for block
constructs that aren't related to object orientation concepts like
classes (which thus also include e.g. interfaces, templates,
contracts, structs, etc).  The more pragmatic distinction is that
while most want some indentation inside classes, it's fairly common
that they don't want it in some of these constructs, so it should be
simple to configure that differently from classes.  See also
`c-class-decl-kwds'.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t   nil
  (c objc) '("extern")
  c++ '("namespace" "extern")
  idl '("module"
	;; In CORBA CIDL:
	"composition"))

(c-lang-defvar c-other-decl-block-key-in-symbols-alist
  (mapcar
   (lambda (elt)
     (cons elt
	   (if (string= elt "extern")
	       'inextern-lang
	     (intern (concat "in" elt)))))
   (c-lang-const c-other-block-decl-kwds))
  "Alist associating keywords in c-other-decl-block-decl-kwds with
their matching \"in\" syntactic symbols.")

(c-lang-defconst c-typedef-decl-kwds
  "Keywords introducing declarations where the identifier(s) being
declared are types.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is a type that's being defined in "class Foo
  ;; {...}").
  t    (append (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds))
  ;; Languages that have a "typedef" construct.
  (c c++ objc idl pike) (append (c-lang-const c-typedef-decl-kwds)
				'("typedef"))
  ;; Unlike most other languages, exception names are not handled as
  ;; types in IDL since they only can occur in "raises" specs.
  idl  (delete "exception" (append (c-lang-const c-typedef-decl-kwds) nil)))

(c-lang-defconst c-typeless-decl-kwds
  "Keywords introducing declarations where the \(first) identifier
\(declarator) follows directly after the keyword, without any type.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  ;; Default to `c-class-decl-kwds' and `c-brace-list-decl-kwds'
  ;; (since e.g. "Foo" is the identifier being defined in "class Foo
  ;; {...}").
  t    (append (c-lang-const c-class-decl-kwds)
	       (c-lang-const c-brace-list-decl-kwds))
  ;; Note: "manages" for CORBA CIDL clashes with its presence on
  ;; `c-type-list-kwds' for IDL.
  idl  (append (c-lang-const c-typeless-decl-kwds)
	       '("factory" "finder" "native"
		 ;; In CORBA PSDL:
		 "key" "stores"
		 ;; In CORBA CIDL:
		 "facet"))
  pike (append (c-lang-const c-class-decl-kwds)
	       '("constant")))

(c-lang-defconst c-modifier-kwds
  "Keywords that can prefix normal declarations of identifiers
\(and typically act as flags).  Things like argument declarations
inside function headers are also considered declarations in this
sense.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t    nil
  (c c++) '("auto" "extern" "inline" "register" "static")
  c++  (append '("explicit" "friend" "mutable" "template" "using" "virtual")
	       (c-lang-const c-modifier-kwds))
  objc '("auto" "bycopy" "byref" "extern" "in" "inout" "oneway" "out" "static")
  ;; FIXME: Some of those below ought to be on `c-other-decl-kwds' instead.
  idl  '("abstract" "attribute" "const" "consumes" "custom" "emits" "import"
	 "in" "inout" "local" "multiple" "oneway" "out" "private" "provides"
	 "public" "publishes" "readonly" "typeid" "typeprefix" "uses"
	 ;; In CORBA PSDL:
	 "primary" "state"
	 ;; In CORBA CIDL:
	 "bindsTo" "delegatesTo" "implements" "proxy" "storedOn")
  ;; Note: "const" is not used in Java, but it's still a reserved keyword.
  java '("abstract" "const" "final" "native" "private" "protected" "public"
	 "static" "strictfp" "synchronized" "transient" "volatile")
  pike '("final" "inline" "local" "nomask" "optional" "private" "protected"
	 "public" "static" "variant"))

(c-lang-defconst c-other-decl-kwds
  "Keywords that can start or prefix any declaration level construct,
besides those on `c-class-decl-kwds', `c-brace-list-decl-kwds',
`c-other-block-decl-kwds', `c-typedef-decl-kwds',
`c-typeless-decl-kwds' and `c-modifier-kwds'.

If any of these also are on `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds' then the associated clauses
will be handled."
  t       nil
  objc    '("@class" "@end" "@defs")
  java    '("import" "package")
  pike    '("import" "inherit"))

(c-lang-defconst c-decl-start-kwds
  "Keywords that always start declarations, wherever they occur.
This can be used for declarations that aren't recognized by the normal
combination of `c-decl-prefix-re' and `c-decl-start-re'."
  t    nil
  ;; Classes can be declared anywhere in a Pike expression.
  pike '("class"))

(c-lang-defconst c-decl-hangon-kwds
  "Keywords that can occur anywhere in a declaration level construct.
This is used for self-contained things that can be tacked on anywhere
on a declaration and that should be ignored to be able to recognize it
correctly.  Typical cases are compiler extensions like
\"__attribute__\" or \"__declspec\":

    __declspec(noreturn) void foo();
    class __declspec(dllexport) classname {...};
    void foo() __attribute__((noreturn));

Note that unrecognized plain symbols are skipped anyway if they occur
before the type, so such things are not necessary to mention here.
Mentioning them here is necessary only if they can occur in other
places, or if they are followed by a construct that must be skipped
over \(like the parens in the \"__attribute__\" and \"__declspec\"
examples above).  In the last case, they alse need to be present on
one of `c-type-list-kwds', `c-ref-list-kwds',
`c-colon-type-list-kwds', `c-paren-nontype-kwds', `c-paren-type-kwds',
`c-<>-type-kwds', or `c-<>-arglist-kwds'."
  ;; NB: These are currently not recognized in all parts of a
  ;; declaration.  Specifically, they aren't recognized in the middle
  ;; of multi-token types, inside declarators, and between the
  ;; identifier and the arglist paren of a function declaration.
  ;;
  ;; FIXME: This ought to be user customizable since compiler stuff
  ;; like this usually is wrapped in project specific macros.  (It'd
  ;; of course be even better if we could cope without knowing this.)
  t nil
  (c c++) '(;; GCC extension.
	    "__attribute__"
	    ;; MSVC extension.
	    "__declspec"))

(c-lang-defconst c-not-primitive-type-keywords
  "List of all keywords apart from primitive types (like \"int\")."
  t (set-difference (c-lang-const c-keywords)
		    (c-lang-const c-primitive-type-kwds)
		    :test 'string-equal)
  ;; The "more" for C++ is the QT keyword (as in "more slots:").
  ;; This variable is intended for use in c-beginning-of-statement-1.
  c++ (append (c-lang-const c-not-primitive-type-keywords) '("more")))

(c-lang-defconst c-protection-kwds
  "Access protection label keywords in classes."
  t    nil
  c++  '("private" "protected" "public")
  objc '("@private" "@protected" "@public"))

(c-lang-defconst c-block-decls-with-vars
  "Keywords introducing declarations that can contain a block which
might be followed by variable declarations, e.g. like \"foo\" in
\"class Foo { ... } foo;\".  So if there is a block in a declaration
like that, it ends with the following ';' and not right away.

The keywords on list are assumed to also be present on one of the
`*-decl-kwds' lists."
  t        nil
  (c objc) '("struct" "union" "enum" "typedef")
  c++      '("class" "struct" "union" "enum" "typedef"))

(c-lang-defconst c-postfix-decl-spec-kwds
  "Keywords introducing extra declaration specifiers in the region
between the header and the body \(i.e. the \"K&R-region\") in
declarations."
  t    nil
  java '("extends" "implements" "throws")
  idl  '("context" "getraises" "manages" "primarykey" "raises" "setraises"
	 "supports"
	 ;; In CORBA PSDL:
	 "as" "const" "implements" "of" "ref"))

(c-lang-defconst c-nonsymbol-sexp-kwds
  "Keywords that may be followed by a nonsymbol sexp before whatever
construct it's part of continues."
  t    nil
  (c c++ objc) '("extern"))

(c-lang-defconst c-type-list-kwds
  "Keywords that may be followed by a comma separated list of type
identifiers, where each optionally can be prefixed by keywords.  (Can
also be used for the special case when the list can contain only one
element.)

Assumed to be mutually exclusive with `c-ref-list-kwds'.  There's no
reason to put keywords on this list if they are on `c-type-prefix-kwds'.
There's also no reason to add keywords that prefixes a normal
declaration consisting of a type followed by a declarator (list), so
the keywords on `c-modifier-kwds' should normally not be listed here
either.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  t    nil
  c++  '("operator")
  objc '("@class")
  java '("import" "new" "extends" "super" "implements" "throws")
  idl  '("manages" "native" "primarykey" "supports"
	 ;; In CORBA PSDL:
	 "as" "implements" "of" "scope")
  pike '("inherit"))

(c-lang-defconst c-ref-list-kwds
  "Keywords that may be followed by a comma separated list of
reference (i.e. namespace/scope/module) identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)  Assumed to
be mutually exclusive with `c-type-list-kwds'.

Note: Use `c-typeless-decl-kwds' for keywords followed by a function
or variable identifier (that's being defined)."
  t    nil
  c++  '("namespace")
  java '("package")
  idl  '("import" "module"
	 ;; In CORBA CIDL:
	 "composition")
  pike '("import"))

(c-lang-defconst c-colon-type-list-kwds
  "Keywords that may be followed (not necessarily directly) by a colon
and then a comma separated list of type identifiers, where each
optionally can be prefixed by keywords.  (Can also be used for the
special case when the list can contain only one element.)"
  t    nil
  c++  '("class" "struct")
  idl  '("component" "eventtype" "home" "interface" "valuetype"
	 ;; In CORBA PSDL:
	 "storagehome" "storagetype"))

(c-lang-defconst c-colon-type-list-re
  "Regexp matched after the keywords in `c-colon-type-list-kwds' to skip
forward to the colon.  The end of the match is assumed to be directly
after the colon, so the regexp should end with \":\".  Must be a
regexp if `c-colon-type-list-kwds' isn't nil."
  t (if (c-lang-const c-colon-type-list-kwds)
	;; Disallow various common punctuation chars that can't come
	;; before the ":" that starts the inherit list after "class"
	;; or "struct" in C++.  (Also used as default for other
	;; languages.)
	"[^\]\[{}();,/#=:]*:"))

(c-lang-defconst c-paren-nontype-kwds
  "Keywords that may be followed by a parenthesis expression that doesn't
contain type identifiers."
  t       nil
  (c c++) '(;; GCC extension.
	    "__attribute__"
	    ;; MSVC extension.
	    "__declspec"))

(c-lang-defconst c-paren-type-kwds
  "Keywords that may be followed by a parenthesis expression containing
type identifiers separated by arbitrary tokens."
  t    nil
  c++  '("throw")
  objc '("@defs")
  idl  '("switch")
  pike '("array" "function" "int" "mapping" "multiset" "object" "program"))

(c-lang-defconst c-<>-type-kwds
  "Keywords that may be followed by an angle bracket expression
containing type identifiers separated by \",\".  The difference from
`c-<>-arglist-kwds' is that unknown names are taken to be types and
not other identifiers.  `c-recognize-<>-arglists' is assumed to be set
if this isn't nil."
  t    nil
  objc '("id")
  idl  '("sequence"
	 ;; In CORBA PSDL:
	 "ref"))

(c-lang-defconst c-<>-arglist-kwds
  "Keywords that can be followed by a C++ style template arglist; see
`c-recognize-<>-arglists' for details.  That language constant is
assumed to be set if this isn't nil."
  t    nil
  c++  '("template")
  idl  '("fixed" "string" "wstring"))

(c-lang-defconst c-brace-id-list-kwds
  "Keywords that may be followed by a brace block containing a comma
separated list of identifier definitions, i.e. like the list of
identifiers that follows the type in a normal declaration."
  t (c-lang-const c-brace-list-decl-kwds))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  t    '("do" "else")
  c++  '("do" "else" "try")
  objc '("do" "else" "@finally" "@try")
  java '("do" "else" "finally" "try")
  idl  nil)

(c-lang-defconst c-block-stmt-1-2-kwds
  "Statement keywords optionally followed by a paren sexp.
Keywords here should also be in `c-block-stmt-1-kwds'."
  t nil
  java '("try"))

(c-lang-defconst c-block-stmt-2-kwds
  "Statement keywords followed by a paren sexp and then by a substatement."
  t    '("for" "if" "switch" "while")
  c++  '("for" "if" "switch" "while" "catch")
  objc '("for" "if" "switch" "while" "@catch" "@synchronized")
  java '("for" "if" "switch" "while" "catch" "synchronized")
  idl  nil
  pike '("for" "if" "switch" "while" "foreach")
  awk  '("for" "if" "while"))

(c-lang-defconst c-simple-stmt-kwds
  "Statement keywords followed by an expression or nothing."
  t    '("break" "continue" "goto" "return")
  objc '("break" "continue" "goto" "return" "@throw")
  ;; Note: `goto' is not valid in Java, but the keyword is still reserved.
  java '("break" "continue" "goto" "return" "throw")
  idl  nil
  pike '("break" "continue" "return")
  awk  '(;; Not sure about "delete", "exit", "getline", etc. ; ACM 2002/5/30
	 "break" "continue" "return" "delete" "exit" "getline" "next"
	 "nextfile" "print" "printf"))

(c-lang-defconst c-paren-stmt-kwds
  "Statement keywords followed by a parenthesis expression that
nevertheless contains a list separated with ';' and not ','."
  t    '("for")
  idl  nil)

(c-lang-defconst c-asm-stmt-kwds
  "Statement keywords followed by an assembler expression."
  t nil
  (c c++) '("asm" "__asm__")) ;; Not standard, but common.

(c-lang-defconst c-case-kwds
  "The keyword\(s) which introduce a \"case\" like construct.
This construct is \"<keyword> <expression> :\"."
  t '("case")
  awk nil)

(c-lang-defconst c-label-kwds
  "Keywords introducing colon terminated labels in blocks."
  t '("case" "default"))

(c-lang-defconst c-before-label-kwds
  "Keywords that might be followed by a label identifier."
  t    '("goto")
  (java pike) (append '("break" "continue")
		      (c-lang-const c-before-label-kwds))
  idl  nil
  awk  nil)

(c-lang-defconst c-constant-kwds
  "Keywords for constants."
  t       nil
  (c c++) '("NULL" ;; Not a keyword, but practically works as one.
	    "false" "true")		; Defined in C99.
  objc    '("nil" "Nil" "YES" "NO" "NS_DURING" "NS_HANDLER" "NS_ENDHANDLER")
  idl     '("TRUE" "FALSE")
  java    '("true" "false" "null") ; technically "literals", not keywords
  pike    '("UNDEFINED")) ;; Not a keyword, but practically works as one.

(c-lang-defconst c-primary-expr-kwds
  "Keywords besides constants and operators that start primary expressions."
  t    nil
  c++  '("operator" "this")
  objc '("super" "self")
  java '("this")
  pike '("this")) ;; Not really a keyword, but practically works as one.

(c-lang-defconst c-lambda-kwds
  "Keywords that start lambda constructs, i.e. function definitions in
expressions."
  t    nil
  pike '("lambda"))

(c-lang-defconst c-inexpr-block-kwds
  "Keywords that start constructs followed by statement blocks which can
be used in expressions \(the gcc extension for this in C and C++ is
handled separately by `c-recognize-paren-inexpr-blocks')."
  t    nil
  pike '("catch" "gauge"))

(c-lang-defconst c-inexpr-class-kwds
  "Keywords that can start classes inside expressions."
  t    nil
  java '("new")
  pike '("class"))

(c-lang-defconst c-inexpr-brace-list-kwds
  "Keywords that can start brace list blocks inside expressions.
Note that Java specific rules are currently applied to tell this from
`c-inexpr-class-kwds'."
  t    nil
  java '("new"))

(c-lang-defconst c-bitfield-kwds
  "Keywords that can introduce bitfields."
  t nil
  (c c++ objc) '("char" "int" "long" "signed" "unsigned"))

(c-lang-defconst c-other-kwds
  "Keywords not accounted for by any other `*-kwds' language constant."
  t    nil
  idl  '("truncatable"
	 ;; In CORBA CIDL: (These are declaration keywords that never
	 ;; can start a declaration.)
	 "entity" "process" "service" "session" "storage"))

;;; Additional constants for parser-level constructs.

(c-lang-defconst c-decl-start-colon-kwd-re
  "Regexp matching a keyword that is followed by a colon, where
  the whole construct can precede a declaration.
  E.g. \"public:\" in C++."
  t "\\<\\>"
  c++ (c-make-keywords-re t (c-lang-const c-protection-kwds)))
(c-lang-defvar c-decl-start-colon-kwd-re
  (c-lang-const c-decl-start-colon-kwd-re))

(c-lang-defconst c-decl-prefix-re
  "Regexp matching something that might precede a declaration, cast or
label, such as the last token of a preceding statement or declaration.
This is used in the common situation where a declaration or cast
doesn't start with any specific token that can be searched for.

The regexp should not match bob; that is done implicitly.  It can't
require a match longer than one token.  The end of the token is taken
to be at the end of the first submatch, which is assumed to always
match.  It's undefined whether identifier syntax (see
`c-identifier-syntax-table') is in effect or not.  This regexp is
assumed to be a superset of `c-label-prefix-re' if
`c-recognize-colon-labels' is set.

Besides this, `c-decl-start-kwds' is used to find declarations.

Note: This variable together with `c-decl-start-re' and
`c-decl-start-kwds' is only used to detect \"likely\"
declaration/cast/label starts.  I.e. they might produce more matches
but should not miss anything (or else it's necessary to use text
properties - see the next note).  Wherever they match, the following
construct is analyzed to see if it indeed is a declaration, cast or
label.  That analysis is not cheap, so it's important that not too
many false matches are triggered.

Note: If a declaration/cast/label start can't be detected with this
variable, it's necessary to use the `c-type' text property with the
value `c-decl-end' on the last char of the last token preceding the
declaration.  See the comment blurb at the start of cc-engine.el for
more info."

  ;; We match a sequence of characters to skip over things like \"};\"
  ;; more quickly.  We match ")" in C for K&R region declarations, and
  ;; in all languages except Java for when a cpp macro definition
  ;; begins with a declaration.
  t "\\([\{\}\(\);,]+\\)"
  java "\\([\{\}\(;,<]+\\)"
  ;; Match "<" in C++ to get the first argument in a template arglist.
  ;; In that case there's an additional check in `c-find-decl-spots'
  ;; that it got open paren syntax.  Match ":" to aid in picking up
  ;; "public:", etc.  This involves additional checks in
  ;; `c-find-decl-prefix-search' to prevent a match of identifiers
  ;; or labels.
  c++ "\\([\{\}\(\);:,<]+\\)"
  ;; Additionally match the protection directives in Objective-C.
  ;; Note that this doesn't cope with the longer directives, which we
  ;; would have to match from start to end since they don't end with
  ;; any easily recognized characters.
  objc (concat "\\([\{\}\(\);,]+\\|"
	       (c-make-keywords-re nil (c-lang-const c-protection-kwds))
	       "\\)")
  ;; Pike is like C but we also match "[" for multiple value
  ;; assignments and type casts.
  pike "\\([\{\}\(\)\[;,]+\\)")

(c-lang-defconst c-decl-start-re
  "Regexp matching the start of any declaration, cast or label.
It's used on the token after the one `c-decl-prefix-re' matched.  This
regexp should not try to match those constructs accurately as it's
only used as a sieve to avoid spending more time checking other
constructs."
  t (c-lang-const c-identifier-start))

(c-lang-defconst c-block-prefix-disallowed-chars
  "List of syntactically relevant characters that never can occur before
the open brace in any construct that contains a brace block, e.g. in
the \"class Foo: public Bar\" part of:

    class Foo: public Bar {int x();} a, *b;

If parens can occur, the chars inside those aren't filtered with this
list.

'<' and '>' should be disallowed even if angle bracket arglists can
occur.  That since the search function needs to stop at them anyway to
ensure they are given paren syntax.

This is used to skip backward from the open brace to find the region
in which to look for a construct like \"class\", \"enum\",
\"namespace\" or whatever.  That skipping should be as tight as
possible for good performance."

  ;; Default to all chars that only occurs in nonsymbol tokens outside
  ;; identifiers.
  t (set-difference
     (c-lang-const c-nonsymbol-token-char-list)
     (c-filter-ops (append (c-lang-const c-identifier-ops)
			   (list (cons nil
				       (c-lang-const c-after-id-concat-ops))))
		   t
		   t
		   (lambda (op)
		     (let ((pos 0) res)
		       (while (string-match "\\(\\s.\\|\\s(\\|\\s)\\)"
					    op pos)
			 (setq res (cons (aref op (match-beginning 1)) res)
			       pos (match-end 0)))
		       res))))

  ;; Allow cpp operations (where applicable).
  t (if (c-lang-const c-opt-cpp-prefix)
	(set-difference (c-lang-const c-block-prefix-disallowed-chars)
			'(?#))
      (c-lang-const c-block-prefix-disallowed-chars))

  ;; Allow ':' for inherit list starters.
  (c++ objc idl) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
				 '(?:))

  ;; Allow ',' for multiple inherits.
  (c++ java) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			     '(?,))

  ;; Allow parentheses for anonymous inner classes in Java and class
  ;; initializer lists in Pike.
  (java pike) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			      '(?\( ?\)))

  ;; Allow '"' for extern clauses (e.g. extern "C" {...}).
  (c c++ objc) (set-difference (c-lang-const c-block-prefix-disallowed-chars)
			       '(?\" ?')))

(c-lang-defconst c-type-decl-prefix-key
  "Regexp matching the declarator operators that might precede the
identifier in a declaration, e.g. the \"*\" in \"char *argv\".  This
regexp should match \"(\" if parentheses are valid in declarators.
The end of the first submatch is taken as the end of the operator.
Identifier syntax is in effect when this is matched \(see
`c-identifier-syntax-table')."
  t (if (c-lang-const c-type-modifier-kwds)
	(concat (regexp-opt (c-lang-const c-type-modifier-kwds) t) "\\>")
      ;; Default to a regexp that never matches.
      "\\<\\>")
  ;; Check that there's no "=" afterwards to avoid matching tokens
  ;; like "*=".
  (c objc) (concat "\\("
		   "[*\(]"
		   "\\|"
		   (c-lang-const c-type-decl-prefix-key)
		   "\\)"
		   "\\([^=]\\|$\\)")
  c++  (concat "\\("
	       "[*\(&]"
	       "\\|"
	       (c-lang-const c-type-decl-prefix-key)
	       "\\|"
	       (concat "\\("   ; 3
		       ;; If this matches there's special treatment in
		       ;; `c-font-lock-declarators' and
		       ;; `c-font-lock-declarations' that check for a
		       ;; complete name followed by ":: *".
		       (c-lang-const c-identifier-start)
		       "\\)")
	       "\\)"
	       "\\([^=]\\|$\\)")
  pike "\\(\\*\\)\\([^=]\\|$\\)")

(c-lang-defconst c-type-decl-suffix-key
  "Regexp matching the declarator operators that might follow after the
identifier in a declaration, e.g. the \"[\" in \"char argv[]\".  This
regexp should match \")\" if parentheses are valid in declarators.  If
it matches an open paren of some kind, the type declaration check
continues at the corresponding close paren, otherwise the end of the
first submatch is taken as the end of the operator.  Identifier syntax
is in effect when this is matched (see `c-identifier-syntax-table')."
  ;; Default to a regexp that matches `c-type-modifier-kwds' and a
  ;; function argument list parenthesis.
  t    (if (c-lang-const c-type-modifier-kwds)
	   (concat "\\(\(\\|"
		   (regexp-opt (c-lang-const c-type-modifier-kwds) t) "\\>"
		   "\\)")
	 "\\(\(\\)")
  (c c++ objc) (concat
		"\\("
		"[\)\[\(]"
		(if (c-lang-const c-type-modifier-kwds)
		    (concat
		     "\\|"
		     ;; "throw" in `c-type-modifier-kwds' is followed
		     ;; by a parenthesis list, but no extra measures
		     ;; are necessary to handle that.
		     (regexp-opt (c-lang-const c-type-modifier-kwds) t)
		     "\\>")
		  "")
		"\\)")
  java "\\([\[\(\)]\\)"
  idl "\\([\[\(]\\)")

(c-lang-defconst c-after-suffixed-type-decl-key
  "This regexp is matched after a declarator expression where
`c-type-decl-suffix-key' has matched.  If it matches then the
construct is taken as a declaration.  It's typically used to match the
beginning of a function body or whatever might occur after the
function header in a function declaration or definition.  It's
undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not.

Note that it's used in cases like after \"foo (bar)\" so it should
only match when it's certain that it's a declaration, e.g., \"{\" but
not \",\" or \";\"."
  t "{"
  ;; If K&R style declarations should be recognized then one could
  ;; consider to match the start of any symbol since we want to match
  ;; the start of the first declaration in the "K&R region".  That
  ;; could however produce false matches on code like "FOO(bar) x"
  ;; where FOO is a cpp macro, so it's better to leave it out and rely
  ;; on the other heuristics in that case.
  t (if (c-lang-const c-postfix-spec-kwds)
	;; Add on the keywords in `c-postfix-spec-kwds'.
	(concat (c-lang-const c-after-suffixed-type-decl-key)
		"\\|"
		(c-make-keywords-re t (c-lang-const c-postfix-spec-kwds)))
      (c-lang-const c-after-suffixed-type-decl-key))
  ;; Also match the colon that starts a base class initializer list in
  ;; C++.  That can be confused with a function call before the colon
  ;; in a ? : operator, but we count on that `c-decl-prefix-re' won't
  ;; match before such a thing (as a declaration-level construct;
  ;; matches inside arglist contexts are already excluded).
  c++ "[{:]")

(c-lang-defconst c-opt-type-concat-key
  "Regexp matching operators that concatenate types, e.g. the \"|\" in
\"int|string\" in Pike.  The end of the first submatch is taken as the
end of the operator.  nil in languages without such operators.  It's
undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not."
  t nil
  pike "\\([|.&]\\)\\($\\|[^|.&]\\)")

(c-lang-defconst c-opt-type-suffix-key
  "Regexp matching operators that might follow after a type, or nil in
languages that don't have such operators.  The end of the first
submatch is taken as the end of the operator.  This should not match
things like C++ template arglists if `c-recognize-<>-arglists' is set.
It's undefined whether identifier syntax (see `c-identifier-syntax-table')
is in effect or not."
  t nil
  (c c++ objc pike) "\\(\\.\\.\\.\\)"
  java (concat "\\(\\[" (c-lang-const c-simple-ws) "*\\]\\|\\.\\.\\.\\)"))

(c-lang-defconst c-special-brace-lists
"List of open- and close-chars that makes up a pike-style brace list,
i.e. for a ([ ]) list there should be a cons (?\\[ . ?\\]) in this
list."
  t    nil
  pike '((?{ . ?}) (?\[ . ?\]) (?< . ?>)))

(c-lang-defconst c-recognize-knr-p
  "Non-nil means K&R style argument declarations are valid."
  t nil
  c t)

(c-lang-defconst c-recognize-typeless-decls
  "Non-nil means function declarations without return type should be
recognized.  That can introduce an ambiguity with parenthesized macro
calls before a brace block.  This setting does not affect declarations
that are preceded by a declaration starting keyword, so
e.g. `c-typeless-decl-kwds' may still be used when it's set to nil."
  t nil
  (c c++ objc java) t)

(c-lang-defconst c-recognize-<>-arglists
  "Non-nil means C++ style template arglists should be handled.  More
specifically, this means a comma separated list of types or
expressions surrounded by \"<\" and \">\".  It's always preceded by an
identifier or one of the keywords on `c-<>-type-kwds' or
`c-<>-arglist-kwds'.  If there's an identifier before then the whole
expression is considered to be a type."
  t (or (consp (c-lang-const c-<>-type-kwds))
	(consp (c-lang-const c-<>-arglist-kwds)))
  java t)

(c-lang-defconst c-enums-contain-decls
  "Non-nil means that an enum structure can contain declarations."
  t nil
  java t)

(c-lang-defconst c-recognize-paren-inits
  "Non-nil means that parenthesis style initializers exist,
i.e. constructs like

Foo bar (gnu);

in addition to the more classic

Foo bar = gnu;"
  t nil
  c++ t)

(c-lang-defconst c-recognize-paren-inexpr-blocks
  "Non-nil to recognize gcc style in-expression blocks,
i.e. compound statements surrounded by parentheses inside expressions."
  t nil
  (c c++) t)

(c-lang-defconst c-recognize-colon-labels
  "Non-nil if generic labels ending with \":\" should be recognized.
That includes labels in code and access keys in classes.  This does
not apply to labels recognized by `c-label-kwds' and
`c-opt-extra-label-key'."
  t nil
  (c c++ objc java pike) t)

(c-lang-defconst c-label-prefix-re
  "Regexp like `c-decl-prefix-re' that matches any token that can precede
a generic colon label.  Not used if `c-recognize-colon-labels' is
nil."
  t "\\([{};]+\\)")

(c-lang-defconst c-nonlabel-token-key
  "Regexp matching things that can't occur in generic colon labels,
neither in a statement nor in a declaration context.  The regexp is
tested at the beginning of every sexp in a suspected label,
i.e. before \":\".  Only used if `c-recognize-colon-labels' is set."
  t (concat
     ;; All keywords except `c-label-kwds' and `c-protection-kwds'.
     (c-make-keywords-re t
       (set-difference (c-lang-const c-keywords)
		       (append (c-lang-const c-label-kwds)
			       (c-lang-const c-protection-kwds))
		       :test 'string-equal)))
  ;; Don't allow string literals, except in AWK.  Character constants are OK.
  (c objc java pike idl) (concat "\"\\|"
				 (c-lang-const c-nonlabel-token-key))
  ;; Also check for open parens in C++, to catch member init lists in
  ;; constructors.  We normally allow it so that macros with arguments
  ;; work in labels.
  c++ (concat "\\s\(\\|\"\\|" (c-lang-const c-nonlabel-token-key)))

(c-lang-defconst c-nonlabel-token-2-key
  "Regexp matching things that can't occur two symbols before a colon in
a label construct.  This catches C++'s inheritance construct \"class foo
: bar\".  Only used if `c-recognize-colon-labels' is set."
  t "\\<\\>"				; matches nothing
  c++ (c-make-keywords-re t '("class")))

(c-lang-defconst c-opt-extra-label-key
  "Optional regexp matching labels.
Normally, labels are detected according to `c-nonlabel-token-key',
`c-decl-prefix-re' and `c-nonlabel-decl-prefix-re'.  This regexp can
be used if there are additional labels that aren't recognized that
way."
  t    nil
  objc (c-make-keywords-re t (c-lang-const c-protection-kwds)))

