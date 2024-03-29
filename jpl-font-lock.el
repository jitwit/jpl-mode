(defgroup jpl-font-lock nil
  "jpl-mode"
  :group 'jpl
  :prefix "j-font-lock-")

;;; hah maybe look to:
;;; https://filmschoolrejects.com/colors-jean-luc-godard/
(defvar j-verb-face
  (defface j-verb-face
    `((t (:foreground "#117EFF")))
  "I."
  :group 'jpl-font-lock))

(defvar j-adverb-face
  (defface j-adverb-face
    `((t (:foreground "#FF9C55")))
  "&"
  :group 'jpl-font-lock))

(defvar j-conjunction-face
  (defface j-conjunction-face
    `((t (:foreground "#FF1C5C")))
  "^:"
  :group 'jpl-font-lock))

(defvar j-noun-face
  (defface j-noun-face
    `((t (:foreground "#BF456E")))
  "a."
  :group 'jpl-font-lock))

(defvar j-is-face
  (defface j-is-face
    `((t (:foreground "#004839")))
    ; "#2F789F"
  "avg =. +/ % #"
  :group 'jpl-font-lock))

(defvar j-copula-face
  (defface j-copula-face
    `((t (:foreground "#808080")))
  "=."
  :group 'jpl-font-lock))

(defvar j-string-face
  (defface j-string-face ;; #14816F
    `((t (:foreground "#424568"))) ;; "#484848" 47AC9A
    "''''"
    :group 'jpl-font-lock))

(defvar j-control-face
  (defface j-control-face
    `((t (:foreground "#484848"))) ; 21184E
    "whilst."
    :group 'jpl-font-lock))

(defvar j-atom-face
  (defface j-atom-face
    ;; "#FFAAFF" 13303B
    `((t (:foreground "#10319B")))
    "_1.2 2p1 3r2j1 ; 'bytes'"
    :group 'jpl-font-lock))

;    ("/\."      . ?⌿)    
;    ("@"        . ?⍛)
;    ("@:"       . ?⍜) ⍤
    ;; a bit like amend: @    
    ;; beside ∘ like ` 
    ;; bind ∘ bond & or ⍥ like over &
;; based on: https://wjmn.github.io/posts/j-can-look-like-apl/
(defvar j->apl
  '(("/\."  . ?⌸)
    (";\."  . ?⌺)
    (",\."  . ?⍪)
    ("\$:"  . ?∇)
    ("/:"   . ?⍋)
    ("\\:"  . ?⍒)
    ("a:"   . ?⍬) ; more like ''
    ("%\."  . ?⌹)
    ("-:"   . ?≡) ; also depth? maximum nesting ≡
    ("=\."  . ?←)
    ("=:"   . ?←)
    ("_:"   . ?∞)
    ("<:"   . ?≤)
    (">:"   . ?≥)
    ("%:"   . ?√)
    ("~:"   . ?≠) ; unique
    ("|."   . ?⌽)
    ("|:"   . ?⍉)
    ("-\."  . ?~) ; not/excluding
    ("\"\." . ?⍎)
    ("#\."  . ?⊥) ; decode
    ("#:"   . ?⊤) ; encode
    ("\":"  . ?⍕)
    ("^:"   . ?⍣)
    ("*\."  . ?∧)
    ("+\."  . ?∨)
    ("e\."  . ?∊) ; maybe also ravel? ; 
    ("o\."  . ?○)
    ("E\."  . ?⍷)
    ("i\."  . ?⍳) ; iota
    ("I\."  . ?⍸) ; where
    (">\."  . ?⌈)
    ("<\."  . ?⌊)
    ("{\."  . ?↑) ; also disclose? ⊃
    ("}\."  . ?↓)
    ("~\."  . ?∪) ; unique
    ("\["   . ?⊣)
    ("^"    . ?*)
    ("^\."  . ?⍟)
    ("<"    . ?⊆) ; nest
    ("#"    . ?≢) ; tally
    ("%"    . ?÷)
    ("]"    . ?⊢)
    ("~"    . ?⍨)
    ("\$"   . ?⍴)
    ("\*"   . ?×))
  "Table to translate J to classic APL characters with pretty-symbols")

(defvar j-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "_" table)
    (modify-syntax-entry ?\= "-" table)
    (modify-syntax-entry ?\( "-" table)
    (modify-syntax-entry ?\) "-" table)
    ;; maybe should be string delim? need to fix comments if so
    ; (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\, "-" table)
    (modify-syntax-entry ?\# "-" table)
    (modify-syntax-entry ?\& "-" table)
    (modify-syntax-entry ?\/ "-" table)
    (modify-syntax-entry ?\\ "-" table)
    (modify-syntax-entry ?\~ "-" table)
    (modify-syntax-entry ?\: "-" table)
    (modify-syntax-entry ?\- "-" table)
    (modify-syntax-entry ?\_ "w" table)
    (modify-syntax-entry ?\@ "-" table)
    (modify-syntax-entry ?\$ "-" table)
    (modify-syntax-entry ?\{ "-" table)
    (modify-syntax-entry ?\} "-" table)
    (modify-syntax-entry ?\< "-" table)
    (modify-syntax-entry ?\> "-" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\r ">" table)
;    (modify-syntax-entry ?\. "-" table)
    table)
  "Syntax table for j-mode")

(defvar j-controls
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."))

(defvar j-verb-3
  '("_1:" "_2:" "_3:" "_4:" "_5:" "_6:" "_7:" "_8:" "_9:" "_0:" "p.." "{::"))
(defvar j-conj-3
  '("&.:" "F:." "F::" "F.." "F.:"))
(defvar j-noun-2
  '("_." "a." "a:"))
(defvar j-verb-2
  '("0:" "1:" "2:" "3:" "4:" "5:" "6:" "7:" "8:" "9:" "_:"
    "x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." ">:" ">." "<:" "<." "\"." "Z:"))
(defvar j-adv-2
  '(;; sadly, "t:" "t."
    "M." "f." "b." "/." "\\." "]:"))
(defvar j-conj-2
  '(; sadly: "T." "D:" "D." "d."
    "S:" "L:" "H." "F:" "F."
    "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:" "]." "[."))
(defvar j-adv-1
  '("}" "\\" "/" "~"))
(defvar j-verb-1
  '("?" "{" "]" "[" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-conj-1
  '("&" "@" "`" "\"" ":"))
;; nb. based numbers (b) can have a-z for bases 10 < b <= 36
(defvar j-numeric-constant
  `(rx bow
       (or (seq (? "_")
		(+ digit)
		(? (or (seq
			(? "." (+ digit)) ;; to allow ending x to mean exact
			(? (seq (or "e" "ad" "ar" "j" "r" "p" "x" "b")
				(? "_")
				(+ digit)
				(? "." (+ digit))
				;; no "b" here?
				(? (seq (or "e" "ad" "ar" "j" "r" "p" "x" "b")
					(? "_")
					(+ digit)
					(? "." (+ digit))
					(? (seq (or "e" "ad" "ar" "j" "r" "p" "x" "b")
						(? "_")
						(+ digit)
						(? "." (+ digit)))))))))
		       "x")))
	   "_"
	   "__")))

;; (defvar j-explicit (rx (or "13" "1" "2" "3" "4") (+ " ") ":" (* " ")))

; https://code.jsoftware.com/wiki/Vocabulary/Words#Words
; note: fixme only one consecutive _ allowed!
(defvar j-identifier
  '(seq alpha (* (or alphanumeric "_"))))

(defvar j-font-locks
  `((
     ;; NB! ' NB. ' gets grabbed as comment!
     (,(rx "NB." (* not-newline))         . font-lock-comment-face)
     (,(rx "{{")                          . j-is-face)
     (,(rx "}}")                          . j-is-face)
     (,(rx (or (submatch-n 1 (eval j-identifier))
	       (seq "'" (submatch-n 1
				    (seq (eval j-identifier)
					 (* (seq (+ " ") (eval j-identifier)))))
		    "'"))
	   (* space)
	   (submatch-n 2 (or "=." "=:")))
      (1 j-is-face)
      (2 j-copula-face))
     (,(rx (submatch-n 1 (or "for_" "goto_" "label_"))
	   (submatch-n 2 (+ alpha))
	   (submatch-n 3 "."))
      (1 j-control-face)
      (2 j-is-face)
      (3 j-control-face))
     (,(rx "'" (* (not (any "'\n"))) "'") . j-string-face)
     (,(rx (eval `(or ,@j-controls)))     . j-control-face)
     (,(rx (eval `(or ,@j-conj-3)))       . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-3)))       . j-verb-face)
     (,(rx (eval `(or ,@j-noun-2)))       . j-noun-face)
     (,(rx (eval `(or ,@j-adv-2)))        . j-adverb-face)
     (,(rx (eval `(or ,@j-conj-2)))       . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-2)))       . j-verb-face)
     (,(rx (eval `(or ,@j-verb-1)))       . j-verb-face)
     (,(rx (eval `(or ,@j-conj-1)))       . j-conjunction-face)
     (,(rx (eval `(or ,@j-adv-1)))        . j-adverb-face)
     ;; kludge				  
     (,(eval j-numeric-constant)          . j-atom-face)
     (,(rx ".")                           . j-conjunction-face)
     ))
  "J Mode font lock keys words")

(provide 'jpl-font-lock)
