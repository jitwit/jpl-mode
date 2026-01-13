(defgroup jpl-font-lock nil
  "jpl-mode"
  :group 'jpl
  :prefix "j-font-lock-")

;;; hah maybe look to:
;;; https://filmschoolrejects.com/colors-jean-luc-godard/

;; Verbs: Bright Blue (Light) -> Sky Blue (Dark)
(defvar j-verb-face
  (defface j-verb-face
    '((((class color) (background light)) (:foreground "#117EFF"))
      (((class color) (background dark))  (:foreground "#6EC6FF")))
    "I."
    :group 'jpl-font-lock))

;; Adverbs: Orange (Light) -> Soft Peach/Orange (Dark)
(defvar j-adverb-face
  (defface j-adverb-face
    '((((class color) (background light)) (:foreground "#FF9C55"))
      (((class color) (background dark))  (:foreground "#FFF59D")))
    "&"
    :group 'jpl-font-lock))

;; Conjunctions: Red/Pink (Light) -> Bright Salmon (Dark)
(defvar j-conjunction-face
  (defface j-conjunction-face
    '((((class color) (background light)) (:foreground "#FF1C5C"))
      (((class color) (background dark))  (:foreground "#FF5C7C")))
    "^:"
    :group 'jpl-font-lock))

;; Nouns: Deep Rose (Light) -> Soft Pink/Mauve (Dark)
(defvar j-noun-face
  (defface j-noun-face
    '((((class color) (background light)) (:foreground "#BF456E"))
;;      (((class color) (background dark))  (:foreground "#E085A3"))
      (((class color) (background dark))  (:foreground "#EC98BA")))
    "a."
    :group 'jpl-font-lock))

;; Is (Definitions): Deep Teal (Light) -> Mint/Aqua (Dark)
;; This required the most drastic change, as #004839 is invisible on black.
(defvar j-is-face
  (defface j-is-face
    '((((class color) (background light)) (:foreground "#004839"))
      (((class color) (background dark))  (:foreground "#4DB6AC")))
    "avg =. +/ % #"
    :group 'jpl-font-lock))

;; Copula: Gray (Light) -> Light Silver (Dark)
(defvar j-copula-face
  (defface j-copula-face
    '((((class color) (background light)) (:foreground "#808080"))
      (((class color) (background dark))  (:foreground "#B0B0B0")))
    "=."
    :group 'jpl-font-lock))

;; Strings: Dark Blue-Gray (Light) -> Periwinkle/Lavender (Dark)
(defvar j-string-face
  (defface j-string-face
    '((((class color) (background light)) (:foreground "#424568"))
      (((class color) (background dark))  (:foreground "#9FA2C5")))
    "''''"
    :group 'jpl-font-lock))

;; Control: Dark Gray (Light) -> Off-White/Platinum (Dark)
(defvar j-control-face
  (defface j-control-face
    '((((class color) (background light)) (:foreground "#484848"))
      (((class color) (background dark))  (:foreground "#FFCDD2")))
    "whilst."
    :group 'jpl-font-lock))

;; Atoms/Numbers: Deep Blue (Light) -> Bright Royal Blue (Dark)
(defvar j-atom-face
  (defface j-atom-face
    '((((class color) (background light)) (:foreground "#10319B"))
      (((class color) (background dark))  (:foreground "#D1EFFF")))
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
