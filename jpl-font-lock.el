(defvar j-verb-face
  (defface j-verb-face
    `((t (:foreground "#117EFF")))
  "I."
  :group 'jpl))

(defvar j-adverb-face
  (defface j-adverb-face
    `((t (:foreground "#FF9C55")))
  "&"
  :group 'jpl))

(defvar j-conjunction-face
  (defface j-conjunction-face
    `((t (:foreground "#FF0D4D")))
  "^:"
  :group 'jpl))

(defvar j-noun-face
  (defface j-noun-face
    `((t (:foreground "#BF456E")))
  "a."
  :group 'jpl))

(defvar j-is-face
  (defface j-is-face
    `((t (:foreground "#004839")))
    ; "#2F789F"
  "avg =. +/ % #"
  :group 'jpl))

(defvar j-copula-face
  (defface j-copula-face
    `((t (:foreground "#808080")))
  "=."
  :group 'jpl))

(defvar j-string-face
  (defface j-string-face
    `((t (:foreground "#000000")))
    "''''"
    :group 'jpl))

(defvar j-control-face
  (defface j-control-face
    `((t (:foreground "#484848"))) ; 21184E
    "''''"
    :group 'jpl))

;; based on: https://wjmn.github.io/posts/j-can-look-like-apl/
(defvar j->apl
  '(("/\."      . ?⌸)
    (";\."      . ?⌺)
					; ⍠
    (",\."      . ?⍪)
    ("\$:"      . ?∇)
    ("/:"       . ?⍋)
    ("\\:"      . ?⍒)
    ;; a bit like amend: @    
    ;;    ("@"        . ?⍛)
    ;;    ("@:"       . ?⍜) ⍤
    ;; beside ∘ like ` 
    ;; bind ∘ bond & or ⍥ like over &
    ("a:"       . ?⍬) ; more like ''
    ("%\."      . ?⌹)
    ("-:"       . ?≡) ; also depth? maximum nesting ≡
    ("=\."      . ?←)
    ("=:"       . ?←)
    ("_:"       . ?∞)
    ("<:"       . ?≤)
    (">:"       . ?≥)
    ("%:"       . ?√)
    ("~:"       . ?≠) ; unique
    ("|."       . ?⌽)
    ("|:"       . ?⍉)
    ("-\."      . ?~) ; not/excluding
    ("\"\."     . ?⍎)
    ("#\."      . ?⊥) ; decode
    ("#:"       . ?⊤) ; encode
    ("\":"      . ?⍕)
    ("^:"       . ?⍣)
    ("*\."      . ?∧)
    ("+\."      . ?∨)
    ("e\."      . ?∊) ; maybe also ravel? ; 
    ("o\."      . ?○)
    ("E\."      . ?⍷)
    ("i\."      . ?⍳) ; iota
    ("I\."      . ?⍸) ; where
    (">\."      . ?⌈)
    ("<\."      . ?⌊)
    ("{\."      . ?↑) ; also disclose? ⊃
    ("}\."      . ?↓)
    ("~\."      . ?∪) ; unique
    ("\["       . ?⊣)
    ("^"        . ?*)
    ("^\."      . ?⍟)
    ("<"        . ?⊆) ; nest
    ("#"        . ?≢) ; tally
    ("%"        . ?÷)
    ("]"        . ?⊢)
    ("~"        . ?⍨)
;    ("/\."      . ?⌿)    
    ("\$"       . ?⍴)
    ("\*"       . ?×))
  "Table to translate J to classic APL characters with pretty-symbols")

(defvar j-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "_"   table)
    (modify-syntax-entry ?\= "-"   table)
    (modify-syntax-entry ?\( "-"   table)
    (modify-syntax-entry ?\) "-"   table)
    (modify-syntax-entry ?\' "."   table)
    (modify-syntax-entry ?\, "-"   table)
    (modify-syntax-entry ?\. "-"   table)
    (modify-syntax-entry ?\# "-"   table)
    (modify-syntax-entry ?\& "-"   table)
    (modify-syntax-entry ?\/ "-"   table)
    (modify-syntax-entry ?\\ "-"   table)
    (modify-syntax-entry ?\~ "-"   table)
    (modify-syntax-entry ?\: "-"   table)
    (modify-syntax-entry ?\- "-"   table)
    (modify-syntax-entry ?\n "."   table)
    (modify-syntax-entry ?\r "."   table)
    table)
  "Syntax table for j-mode")

(defvar j-font-lock-constants '())

(defvar j-controls
  '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
    "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
    "try."  "catch."  "catchd."  "catcht."  "end."))

(defvar j-verb-3
  '("_1:" "_2:" "_3:" "_4:" "_5:" "_6:" "_7:" "_8:" "_9:" "_0:" "p.." "{::"))
(defvar j-conj-3
  '("&.:"))
(defvar j-noun-2
  '("_." "a." "a:"))
(defvar j-verb-2
  '("0:" "1:" "2:" "3:" "4:" "5:" "6:" "7:" "8:" "9:" "_:"
    "x:" "u:" "s:" "r." "q:" "p:" "p." "o." "L." "j." "I." "i:" "i." "E." "e."
    "C." "A." "?." "\":" "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
    ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
    "+." ">:" ">." "<:" "<."))
(defvar j-adv-2
  '(;; sadly, "t:" "t."
    "M." "f." "b." "/." "\\."))
(defvar j-conj-2
  '(; sadly: "T." "D:" "D." "d."
    "S:" "L:" "H." 
    "&:" "&." "@:" "@." "`:" "!:" "!." ";."
    "::" ":." ".:" ".." "^:"))

(defvar j-adv-1
  '("}" "." "\\" "/" "~"))
(defvar j-verb-1
  '("?" "{" "]" "[" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
(defvar j-conj-1
  '("&" "@" "`" "\"" ":" "."))

(setq j-comment-rx
      (rx "NB." (* not-newline)))

(setq j-explicit
      (rx (or "13" "1" "2" "3" "4")
	  (+ " ") ":" (* " ")))

; https://code.jsoftware.com/wiki/Vocabulary/Words#Words
; note: fixme only one consecutive _ allowed!
(defvar j-identifier
  '(seq alpha (* (or alphanumeric "_"))))

(defvar j-font-locks
  `((
     ;; one day: multiline strings and inline explicit defs
     (,(rx "NB." (* not-newline))     . font-lock-comment-face)

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
     (,(rx "'" (* (not "'")) "'")     . j-string-face)
     (,(rx (eval `(or ,@j-controls))) . j-control-face)
     (,(rx (eval `(or ,@j-conj-3)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-3)))   . j-verb-face)
     (,(rx (eval `(or ,@j-noun-2)))   . j-noun-face)
     (,(rx (eval `(or ,@j-adv-2)))    . j-adverb-face)
     (,(rx (eval `(or ,@j-conj-2)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-verb-2)))   . j-verb-face)
     (,(rx (eval `(or ,@j-verb-1)))   . j-verb-face)
     (,(rx (eval `(or ,@j-conj-1)))   . j-conjunction-face)
     (,(rx (eval `(or ,@j-adv-1)))    . j-adverb-face)
     ))
  "J Mode font lock keys words")

(provide 'jpl-font-lock)
