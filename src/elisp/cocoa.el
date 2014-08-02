;; cocoa.el --- major mode for editing and running CoCoA

;; Copyright (c) 2005  Anna Bigatti
;; This file is part of the CoCoA-4 distribution.
;; You are free to use any part of this file in your own programs.
;; Many thanks to Giovanna Roda and Giorgio Vecchiocattivi 
;;   for their substantial early contributions.
;; cocoa-comint-mode introduced by Burkhard Zimmermann (May 2005)

;; Jun 2005: 
;; tab does both indentation and dabbrev completion -- B.Zimmermann
;; C-c C-e for EndDefine also prints comment with function name
;; F2 goes to error line after sourcing a file -- B.Zimmermann

;; Oct 2005: 
;; "CoCoA not responding?" (panic button) in the menu
;; "Source File Parse Error Line" in the menu

;; Nov 2005: 
;; toggle "TAB for Completion" in the menu

(setq cocoa-mode-version "22 Jun 2007")

;;==============================================================>
;;     CUSTOMIZATION in cocoa.emacs
;;==============================================================>

;;==============================================================>
;  \M-x cocoa
;; will open a buffer called *cocoa* with a running CoCoA

;; you can edit a CoCoA file and send commands from it to *cocoa*:
;   "\C-c\C-l" 'cocoa-send-line
;   "\C-c\C-b" 'cocoa-send-buffer
;   "\C-c\C-r" 'cocoa-send-region
;   "\C-c\C-f" 'cocoa-source-file
;   "\C-c\C-c" 'comment-region
;   "\C-c\C-e" 'cocoa-close-block  ;; writes the appropriate "End"
;   "\C-c\C-w" 'cocoa-open-wordlist-file
;   "\C-c\C-q" 'cocoa-shell-quit
;   "\C-c?"    'cocoa-word-man
;   "\C-cm"    'cocoa-word-man
;   "\C-c\C-m" 'cocoa-word-man
;;==============================================================>




;;-------------------------------------------------------------;;
;;                    EDITING CoCoA files                      ;;
;;-------------------------------------------------------------;;

;;-------------------------------------------------------------->
;; abbrev-table
;;-------------------------------------------------------------->
;; to use this, in cocoa-mode, type Esc-x abbrev-mode.
;; it capitalizes the CoCoA keywords 
(defvar cocoa-abbrev-table nil
  "Abbrev table in use in CoCoA buffers.")

(define-abbrev-table 'cocoa-abbrev-table '(
    ("alias"   		"Alias"   	nil 0)
    ("and"    		"And"   	nil 0)
    ("break"   		"Break"   	nil 0)
    ("ciao"    		"Ciao;"   	nil 0)
    ("cond"    		"Cond"    	nil 0)
    ("define"  		"Define"  	nil 0)
    ("deglex"		"DegLex"     	nil 0)
    ("degrevlex"	"DegRevLex"     nil 0)
    ("do"      		"Do"      	nil 0)
    ("else"    		"Else"    	nil 0)
    ("elsif"   		"ElsIf"   	nil 0)
    ("end"     		"End;"    	nil 0)
    ("endalias"         "EndAlias;"   	nil 0)
    ("endblock"         "EndBlock;"   	nil 0)
    ("endcatch"         "EndCatch;"   	nil 0)
    ("endcond"          "EndCond;"   	nil 0)
    ("enddefine"        "EndDefine;"   	nil 0)
    ("endfor"           "EndFor;"   	nil 0)
    ("endforeach"       "EndForeach;"  	nil 0)
    ("endif"            "EndIf;"   	nil 0)
    ("endpackage"       "EndPackage;"  	nil 0)
    ("endrepeat"        "EndRepeat;"  	nil 0)
    ("endusing"         "EndUsing;"   	nil 0)
    ("endwhile"         "EndWhile;"   	nil 0)
    ("error"   		"Error"    	nil 0)
    ("false"     	"False"     	nil 0)
    ("for"     		"For"     	nil 0)
    ("foreach" 		"Foreach" 	nil 0)
    ("ideal"      	"Ideal"      	nil 0)
    ("if"      		"If"      	nil 0)
    ("in"      		"In"      	nil 0)
    ("isin"            	"IsIn"      	nil 0)
    ("indentation"	"Indentation;"	nil 0)
    ("lex"    		"Lex"  	        nil 0)
    ("mat"    		"Mat"  	        nil 0)
    ("module"    	"Module"        nil 0)
    ("not"	        "Not"           nil 0)
    ("on"    		"On"   	        nil 0)
    ("or"    		"Or"   	        nil 0)
    ("posto"   		"PosTo"   	nil 0)
    ("print"   		"Print"   	nil 0)
    ("println" 		"PrintLn" 	nil 0)
    ("record"  		"Record"  	nil 0)
    ("return"  		"Return"  	nil 0)
    ("set"     		"Set"   	nil 0)
    ("step"    		"Step"   	nil 0)
    ("quit"    		"Quit;"   	nil 0)
    ("then"    		"Then"    	nil 0)
    ("time"    		"Time"    	nil 0)
    ("true"    		"True"    	nil 0)
    ("to"      		"To"      	nil 0)
    ("topos"   		"ToPos"      	nil 0)
    ("unset"     	"Unset"     	nil 0)
    ("use"     		"Use"     	nil 0)
    ("using"   		"Using"   	nil 0)
    ("var"   		"Var"   	nil 0)
    ("vector"  		"Vector"   	nil 0)
    ("while"   		"While"   	nil 0)
    ("xel"    		"Xel"  	        nil 0)
    ))

(defvar cocoa-imenu-generic-expression
  '("^[ \t]*\\(Define\\)[ \t\n]+\\([a-zA-Z0-9_.:]+\\)" . (2))
  "Imenu expression for CoCoA-mode.  See `imenu-generic-expression'.")

(defconst cocoa-startkeywords-list
(concat "\\(Alias\\|Block\\|Catch\\|Cond\\|Define\\|For\\|Foreach\\|"
	"If\\|Package\\|Repeat\\|Using\\|While\\)"
	))
(defconst cocoa-endkeywords-list
(concat "\\<End" 
	"\\(\\|Alias\\|Block\\|Catch\\|Cond\\|Define\\|For\\|Foreach\\|"
	"If\\|Package\\|Repeat\\|Using\\|While\\)"
	"\\>"))

;;-------------------------------------------------------------->
;; Regular expressions used to calculate indent, etc.
;;-------------------------------------------------------------->

(defconst cocoa-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst cocoa-beg-block-re
"\\<\\(Alias\\|Block\\|Catch\\|Define\\|If\\|Foreach\\|For\\|Repeat\\|Using\\|While\\|Cond\\)\\>")
(defconst cocoa-end-block-re
(concat "\\<\\("
	cocoa-endkeywords-list "\\|"
	"Until"
	"\\)\\>"))
(defconst cocoa-declaration-re "\\<\\(NIENTE_DA_FISSARE\\)\\>")
(defconst cocoa-defun-re       "\\<\\(Define\\)\\>")
(defconst cocoa-sub-block-re   
"\\<\\(Alias\\|Block\\|Catch\\|If\\|Foreach\\|For\\|Repeat\\|Using\\|While\\|Cond\\)\\>")
(defconst cocoa-noindent-re
(concat "\\<\\("
	cocoa-endkeywords-list "\\|"
	"Else\\|Elsif\\|Until"
	"\\)\\>"))
(defconst cocoa-nosemi-re      "\\<\\(Then\\|Do\\|Else\\)\\>")
(defconst cocoa-autoindent-lines-re
(concat "\\<\\("
	cocoa-endkeywords-list "\\|"
	"Define\\|While\\|Else"
	"\\)\\>"))

;;; Strings used to mark beginning and end of excluded text
(defconst cocoa-exclude-str-start "{-----\\/----- EXCLUDED -----\\/-----")
(defconst cocoa-exclude-str-end " -----/\\----- EXCLUDED -----/\\-----}")

;;-------------------------------------------------------------->
;; cocoa-mode-syntax-table
;;-------------------------------------------------------------->

(defvar cocoa-mode-syntax-table nil
  "Syntax table in use in cocoa-mode buffers.")

(if cocoa-mode-syntax-table
    ()
  (setq cocoa-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "."   cocoa-mode-syntax-table)
  (modify-syntax-entry ?( "()"  cocoa-mode-syntax-table)
  (modify-syntax-entry ?) ")("  cocoa-mode-syntax-table)
;  (modify-syntax-entry ?* "." cocoa-mode-syntax-table)
  (modify-syntax-entry ?+ "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?= "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?% "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?< "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?> "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?& "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?| "."    cocoa-mode-syntax-table)
  (modify-syntax-entry ?_ "w"    cocoa-mode-syntax-table)
  ; string
  (modify-syntax-entry ?\' "\""  cocoa-mode-syntax-table)
  ; -- comments 
  (modify-syntax-entry ?- ". 12"    cocoa-mode-syntax-table)
  ; // and /* */ comments 
  (modify-syntax-entry ?/  ". 124" cocoa-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23b" cocoa-mode-syntax-table)
  (modify-syntax-entry ?\n ">"   cocoa-mode-syntax-table)
)

;;-------------------------------------------------------------->
;; font-lock-keywords
;;-------------------------------------------------------------->

(defvar cocoa-font-lock-keywords
  (list
   ; function definition
   '("^[ \t]*\\(Define\\)\\>[ \t]*\\(\\sw+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
   ; types
   (cons    (concat "\\<\\("
	   "NULL\\|BOOL\\|STRING\\|TYPE\\|ERROR\\|RECORD\\|DEVICE\\|"
	   "INT\\|RAT\\|ZMOD\\|POLY\\|RATFUN\\|VECTOR\\|IDEAL\\|MODULE\\|"
	   "MAT\\|LIST\\|RING\\|TAGGED\\|FUNCTION"
	   "\\)\\>")
	 'font-lock-type-face)
   ; constants
   (cons   (concat "\\<\\("
		   "TRUE\\|FALSE\\|True\\|False\\|"
		   "Lex\\|Xel\\|DegLex\\|DegRevLex\\|"
		   "PosTo\\|ToPos\\|Null\\)\\>") 
	   'font-lock-constant-face)
   ; Ideal, Module...
   (cons   (concat "\\<\\("
		   "Ideal\\|Module\\|Mat\\|Record\\|Vector\\|Error"
		   "\\)\\>") 
	   'font-lock-builtin-face)
   ; keywords
   (concat "\\<\\("
	   "And\\|Or\\|Not\\|Do\\|"
	   "Els\\(e\\|If\\|if\\)\\|"
	   "Break\\|"
	   cocoa-startkeywords-list "\\|"
	   cocoa-endkeywords-list "\\|"
	   "In\\|IsIn\\|"
	   "On\\|Print\\(\\|Ln\\)\\|"
	   "Repeat\\|Until\\|Return\\|Set\\|Step\\|"
	   "T\\(hen\\|ime\\|o\\)\\|"
	   "Unset\\|Use\\|Var"
	   "\\)\\>")
   '("\\<\\(NULLA_DA_FISSARE\\)\\>[ \t]*\\([0-9]+\\)?"
     (1 font-lock-keyword-face) (2 font-lock-reference-face nil t)))
  "Additional expressions to highlight in CoCoA mode.")

;;-------------------------------------------------------------->

(defvar cocoa-indent-level 2
  "*Indentation of CoCoA statements with respect to containing block.")

(defvar cocoa-case-indent 2
  "*Indentation for case statements.")

(defvar cocoa-auto-newline nil
  "*Non-nil means automatically newline after semicolons and the punctuation
mark after an end.")

(defvar cocoa-tab-always-indent t
  "*Non-nil means TAB in CoCoA mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defvar cocoa-auto-endcomments t
  "*Non-nil means a comment { ... } is set after the ends which ends cases and
functions. The name of the function or case will be set between the braces.")

(defvar cocoa-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'paramlist', 'declaration' or 'case', which will
do auto lineup in parameterlist, declarations or case-statements
respectively. The word 'all' will do all lineups. '(case paramlist) for
instance will do lineup in case-statements and parameterlist, while '(all)
will do all lineups.")

;; (defvar cocoa-toggle-completions nil
;;   "*Non-nil means \\<cocoa-mode-map>\\[cocoa-complete-word] should try all
;; possible completions one by one.
;; Repeated use of \\[cocoa-complete-word] will show you all of them.
;; Normally, when there is more than one possible completion,
;; it displays a list of all possible completions.")

;; (defvar cocoa-type-keywords
;;   '("NULLA_DA_FISSARE_COCOA_TYPE_KEYWORDS")
;;   "*Keywords for types used when completing a word in a declaration or
;; parmlist.
;; \(eg. integer, real, char.)  The types defined within the CoCoA program
;; will be completed runtime, and should not be added to this list.")

;; (defvar cocoa-start-keywords
;;   '("Define" "End" "While")
;;   "*Keywords to complete when standing at the first word of a statement.
;; \(eg. begin, repeat, until, readln.)
;; The procedures and variables defined within the CoCoA program
;; will be completed runtime and should not be added to this list.")

;; (defvar cocoa-separator-keywords
;;   '("To" "Else" "Mod" "Div" "Then")
;;   "*Keywords to complete when NOT standing at the first word of a statement.
;; \(eg. downto, else, mod, then.)
;; Variables and function names defined within the
;; CoCoA program are completed runtime and should not be added to this list.")


;;;
;;;  Macros
;;;

(defsubst cocoa-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst cocoa-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defun cocoa-declaration-end ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-forward
		 "[:=]\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)"
		 (save-excursion (end-of-line 2) (point)) t))
      (cond ((match-beginning 1) (setq nest (1+ nest)))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((looking-at "[^(\n]+)") (setq nest 0))))))


(defun cocoa-declaration-beg ()
  (let ((nest 1))
    (while (and (> nest 0)
		(re-search-backward
"[:=]\\|\\<\\(NULLA_DECLARATION_BEG\\)\\>\\|\\(\\<record\\>\\)\\|\\(\\<end\\>\\)
" (cocoa-get-beg-of-line 0) t))
      (cond ((match-beginning 1) (setq nest 0))
	    ((match-beginning 2) (setq nest (1- nest)))
	    ((match-beginning 3) (setq nest (1+ nest)))))
    (= nest 0)))


(defsubst cocoa-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (cocoa-get-beg-of-line) (point)))))




;;======================================================================
;;;  Electric functions
;;======================================================================
(defun electric-cocoa-terminate-line ()
  "Terminate line and indent next line."
  (interactive)
  ;; First, check if current line should be indented
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at cocoa-autoindent-lines-re)
	(cocoa-indent-line)))
  (delete-horizontal-space) ; Removes trailing whitespaces
  (newline)
  ;; Indent next line
  (cocoa-indent-line)
  ;; Maybe we should set some endcomments
  (if cocoa-auto-endcomments
      (cocoa-set-auto-comments))
  ;; Check if we shall indent inside comment
  (let ((setstar nil))
    (save-excursion
      (forward-line -1)
      (skip-chars-forward " \t")
      (cond ((looking-at "\\*[ \t]+/")
	     ;; Delete region between `*' and `/' if there is only whitespaces.
	     (forward-char 1)
	     (delete-horizontal-space))
	    ((and (looking-at "/\\*\\|\\*[^/]")
		  (not (save-excursion
			 (search-forward "\\*/" (cocoa-get-end-of-line) t))))
	     (setq setstar t))))
    ;; If last line was a star comment line then this one shall be too.
;    (if (null setstar)
;	(cocoa-indent-line)
;      (insert "*  "))
))

(defun electric-cocoa-semi-or-dot ()
  "Insert `;' or `.' character and reindent the line."
  (interactive)
  (insert last-command-char)
  ;; Do nothing if within string.
  (if (cocoa-within-string)
      ()
    (save-excursion
      (beginning-of-line)
      (cocoa-indent-line))
    (if cocoa-auto-newline
	(electric-cocoa-terminate-line))))

;; (defun electric-cocoa-colon ()
;;   "Insert `:' and do all indentions except line indent on this line."
;;   (interactive)
;;   (insert last-command-char)
;;   ;; Do nothing if within string.
;;   (if (cocoa-within-string)
;;       ()
;;     (save-excursion
;;       (beginning-of-line)
;;       (cocoa-indent-line))
;;     (let ((cocoa-tab-always-indent nil))
;;       (cocoa-indent-command))))

(defun electric-cocoa-equal ()
  "Insert `=', and do indention if within type declaration."
  (interactive)
  (insert last-command-char)
  (if (eq (car (cocoa-calculate-indent)) 'declaration)
      (let ((cocoa-tab-always-indent nil))
	(cocoa-indent-command))))

;; (defun electric-cocoa-hash ()
;;   "Insert `#', and indent to column 0 if this is a CPP directive."
;;   (interactive)
;;   (insert last-command-char)
;;   (if (save-excursion (beginning-of-line) (looking-at "^[ \t]*#"))
;;       (save-excursion (beginning-of-line)
;; 		      (delete-horizontal-space))))

(defun electric-cocoa-tab-no-completion ()
  "Function called when TAB is pressed in CoCoA mode."
  (interactive)
  ;; Do nothing if within a string or in a CPP directive.
  (if (or (cocoa-within-string)
	  (and (not (bolp))
	       (save-excursion (beginning-of-line) (eq (following-char) ?#))))
      (insert "\t")
    ;; If cocoa-tab-always-indent, indent the beginning of the line.
    (if cocoa-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (cocoa-indent-line))
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
	  (cocoa-indent-line)
	(insert "\t")))
    (cocoa-indent-command))
)


(defun electric-cocoa-tab ()
  (interactive)
  (if cocoa-tab-is-completion
      (progn (electric-cocoa-tab-no-completion) (dabbrev-expand()))
    (electric-cocoa-tab-no-completion)
    )
  )



(defun cocoa-beg-of-defun ()
  "Move backward to the beginning of the current function or procedure."
  (interactive)
  (re-search-backward cocoa-defun-re nil 'move)
)

(defun cocoa-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((parse-sexp-ignore-comments t)
	(nest 0) pos
	(regexp (concat "\\(" cocoa-beg-block-re "\\)\\|\\("
			cocoa-end-block-re "\\)")))
    (if (not (looking-at "[ \t\n]")) (forward-sexp -1))
    (or (looking-at cocoa-beg-block-re)
	;; Skip to end of statement
	(setq pos (catch 'found
		    (while t
		      (forward-sexp 1)
		      (cond ((looking-at "[ \t]*;")
			     (skip-chars-forward "^;")
			     (forward-char 1)
			     (throw 'found (point)))
			    ((save-excursion
			       (forward-sexp -1)
			       (looking-at cocoa-beg-block-re))
			     (goto-char (match-beginning 0))
			     (throw 'found nil))
			    ((eobp)
			     (throw 'found (point))))))))
    (if (not pos)
	;; Skip a whole block
	(catch 'found
	  (while t
	    (re-search-forward regexp nil 'move)
	    (setq nest (if (match-end 1)
			   (1+ nest)
			 (1- nest)))
	    (cond ((eobp)
		   (throw 'found (point)))
		  ((= 0 nest)
		   (throw 'found (cocoa-end-of-statement))))))
      pos)))

;;;
;;; Other functions
;;;
(defun cocoa-set-auto-comments ()
  "Insert `{ case }' or `{ NAME }' on this line if appropriate.
Insert `{ case }' if there is an `end' on the line which
ends a case block.  Insert `{ NAME }' if there is an `end'
on the line which ends a function or procedure named NAME."
  (save-excursion
    (forward-line -1)
    (skip-chars-forward " \t")
    (if (and (or (looking-at "\\<EndDefine;") (looking-at "\\<End;") )
	     (not (save-excursion
		    (end-of-line)
		    (search-backward "--" (cocoa-get-beg-of-line) t))))
	(let ((type (car (cocoa-calculate-indent))))
	  (if (eq type 'declaration)
	      ()
	    (if (eq type 'case)
		;; This is a case block
		(progn
		  (end-of-line)
		  (delete-horizontal-space)
		  (insert " -- case "))
	      (let ((nest 1))
		;; Check if this is the end of a function
		(save-excursion
		  (while (not (or (looking-at cocoa-defun-re) (bobp)))
		    (backward-sexp 1)
		    (cond ((looking-at cocoa-beg-block-re)
			   (setq nest (1- nest)))
			  ((looking-at cocoa-end-block-re)
			   (setq nest (1+ nest)))))
		  (if (bobp)
		      (setq nest 1)))
		(if (zerop nest)
		    (progn
		      (end-of-line)
		      (delete-horizontal-space)
		      (insert " -- ")
		      (let (b e)
			(save-excursion
			  (setq b (progn (cocoa-beg-of-defun)
					 (skip-chars-forward "^ \t")
					 (skip-chars-forward " \t")
					 (point))
				e (progn (skip-chars-forward "a-zA-Z0-9_")
					 (point))))
			(insert-buffer-substring (current-buffer) b e))
		      (insert ""))))))))))



;;;
;;; Indentation
;;;
(defconst cocoa-indent-alist
  '((block . (+ ind cocoa-indent-level))
    (case . (+ ind cocoa-case-indent))
    (caseblock . ind) (cpp . 0)
    (declaration . (+ ind cocoa-indent-level))
    (paramlist . (cocoa-indent-paramlist t))
    (comment . (cocoa-indent-comment t))
    (defun . ind) (contexp . ind)
    (unknown . 0) (string . 0)))

(defun cocoa-indent-command ()
  "Indent for special part of code."
  (let* ((indent-str (cocoa-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (cond ((and (eq type 'paramlist)
		(or (memq 'all cocoa-auto-lineup)
		    (memq 'paramlist cocoa-auto-lineup)))
	   (cocoa-indent-paramlist)
	   (cocoa-indent-paramlist))
	  ((and (eq type 'declaration)
		(or (memq 'all cocoa-auto-lineup)
		    (memq 'declaration  cocoa-auto-lineup)))
	   (cocoa-indent-declaration))
	  ((and (eq type 'case) (not (looking-at "^[ \t]*$"))
		(or (memq 'all cocoa-auto-lineup)
		    (memq 'case cocoa-auto-lineup)))
	   (cocoa-indent-case)))
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))))

(defun cocoa-indent-line ()
  "Indent current line as a CoCoA statement."
  (let* ((indent-str (cocoa-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str))))
    (if (looking-at "^[0-9a-zA-Z]+[ \t]*:[^:=]")
	(search-forward ":" nil t))
    (delete-horizontal-space)
    ;; Some things should not be indented
    (if (or (and (eq type 'declaration) (looking-at cocoa-declaration-re))
	    (eq type 'cpp)
	    (looking-at cocoa-defun-re))
	()
      ;; Other things should have no extra indent
      (if (looking-at cocoa-noindent-re)
	  (indent-to ind)
	;; But most lines are treated this way:
	(indent-to (eval (cdr (assoc type cocoa-indent-alist))))
	))))

(defun cocoa-calculate-indent ()
  "Calculate the indent of the current CoCoA line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point))
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (nest 0) (par 0) (complete (looking-at "[ \t]*End\\>"))
	   (elsed (looking-at "[ \t]*Else\\>"))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> (car state) 0)
			  (goto-char (scan-lists (point) -1 (car state)))
			  (setq par (1+ (current-column))))
			 ((save-excursion (beginning-of-line)
					  (eq (following-char) ?#))
			  (throw 'nesting 'cpp)))
		   ;; Loop until correct indent is found
		   (while t
		     (if (bobp) (throw 'nesting 'unknown))
		     (backward-sexp 1)
		     (cond (;--Escape from case statements
			    (and (looking-at "[A-Za-z0-9]+[ \t]*:[^:=]")
				 (not complete)
				 (save-excursion (skip-chars-backward " \t")
						 (bolp))
				 (= (save-excursion
				      (end-of-line) (backward-sexp) (point))
				    (point))
				 (> (save-excursion (goto-char oldpos)
						    (beginning-of-line)
						    (point))
				    (point)))
			    (throw 'nesting 'caseblock))
			   (;--Nest block outwards
			    (looking-at cocoa-beg-block-re)
			    (if (= nest 0)
				(cond ((looking-at "NULLA_DA_FISSARE\\>")
				       (throw 'nesting 'case))
				      ((looking-at "NULLA_DA_FISSARE\\>")
				       (throw 'nesting 'declaration))
				      (t (throw 'nesting 'block)))
			      (setq nest (1- nest))))
			   (;--Nest block inwards
			    (looking-at cocoa-end-block-re)
			    (if (and (looking-at "End\\s ")
				     elsed (not complete))
				(throw 'nesting 'block))
			    (setq complete t
				  nest (1+ nest)))
			   (;--Defun (or parameter list)
			    (looking-at cocoa-defun-re)
			    (if (= 0 par)
				(throw 'nesting 'defun)
			      (setq par 0)
			      (let ((n 0))
				(while (re-search-forward

	"\\(\\<NULLA_DA_FISSARE\\>\\)\\|\\<NULLA_DA_FISSARE\\>"
					oldpos t)
				  (if (match-end 1)
				      (setq n (1+ n)) (setq n (1- n))))
				(if (> n 0)
				    (throw 'nesting 'declaration)
				  (throw 'nesting 'paramlist)))))
			   (;--Declaration part
			    (looking-at cocoa-declaration-re)
			    (if (save-excursion
				  (goto-char oldpos)
				  (forward-line -1)
				  (looking-at "^[ \t]*$"))
				(throw 'nesting 'unknown)
			      (throw 'nesting 'declaration)))
			   (;--If, else or while statement
			    (and (not complete)
				 (looking-at cocoa-sub-block-re))
			    (throw 'nesting 'block))
			   (;--Found complete statement
			    (save-excursion (forward-sexp 1)
					    (= (following-char) ?\;))
			    (setq complete t))
			   (;--No known statements
			    (bobp)
			    (throw 'nesting 'unknown))
			   )))))

      ;; Return type of block and indent level.
      (if (> par 0)                               ; Unclosed Parenthesis
	  (list 'contexp par)
	(list type (cocoa-indent-level))))))

(defun cocoa-indent-level ()
  "Return the indent-level the current statement has.
Do not count labels, case-statements or records."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*[0-9a-zA-Z]+[ \t]*:[^:=]")
	(search-forward ":" nil t)
      (if (looking-at ".*=[ \t]*NULLA_DA_FISSARE\\>")
	  (search-forward "=" nil t)))
    (skip-chars-forward " \t")
    (current-column)))

(defun cocoa-indent-comment (&optional arg)
  "Indent current line as comment.
If optional arg is non-nil, just return the
column number the line should be indented to."
    (let* ((stcol (save-excursion
		    (re-search-backward "--" nil t)
		    (1+ (current-column)))))
      (if arg stcol
	(delete-horizontal-space)
	(indent-to stcol))))


;from b to e nicely. The lineup string is str."

(defun cocoa-get-lineup-indent (b e str)
  (save-excursion
    (let ((ind 0)
	  (reg (concat str "\\|\\(\\<NULLA_DA_FISSARE\\>\\)"))
	  nest)
      (goto-char b)
      ;; Get rightmost position
      (while (< (point) e)
	(setq nest 1)
	(if (re-search-forward reg (min e (cocoa-get-end-of-line 2)) 'move)
	    (progn
	      ;; Skip record blocks
	      (if (match-beginning 1)
		  (cocoa-declaration-end)
		(progn
		  (goto-char (match-beginning 0))
		  (skip-chars-backward " \t")
		  (if (> (current-column) ind)
		      (setq ind (current-column)))
		  (goto-char (match-end 0)))))))
      ;; In case no lineup was found
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))

;;-------------------------------------------------------------->
;; cocoa-close-block  {like tex-close-latex-block} C-c C-e
;;-------------------------------------------------------------->

(defun cocoa-last-unended-begin ()
  "Leave point at the beginning of the last `\\begin{...}' that is unended."
;  (while (and (re-search-backward "\\(\\\\begin\\s *{\\)\\|\\(\\\\end\\s *{\\)")
;              (looking-at "\\\\end{"))
  (while (and (re-search-backward 
	       (concat 
;		"\\([^a-zA-Z0-9]\\(If\\|Define\\)[^a-zA-z0-9]\\|"
		"\\(\\<" cocoa-startkeywords-list "\\>\\|"
		cocoa-endkeywords-list "\\)")
	       )
              (looking-at cocoa-endkeywords-list))
    (cocoa-last-unended-begin)))

(defun cocoa-close-block ()
  "Creates an \\end{...} to match the last unclosed \\begin{...}."
  (interactive "*")
  (let ((new-line-needed (bolp))
	text indentation)
    (save-excursion
      (condition-case nil
          (cocoa-last-unended-begin)
        (error (error "Couldn't find unended command")))
      (setq indentation (current-column))
      (re-search-forward "[ \n]*\\(\\s *[^)\n ]*\\)")
      (setq text (buffer-substring (match-beginning 1) (match-end 1))))
    (indent-to indentation)
    (insert "End" text ";")
    (electric-cocoa-terminate-line)))
;    (if new-line-needed (insert ?\n))))

;;-------------------------------------------------------------;;
;;                        RUNNING CoCoA                        ;;
;;-------------------------------------------------------------;;

(defun cocoa ()
  "CoCoA interface.
The function splits the current window in two parts, one containing
a CoCoA shell and the other containing a file chosen by the user
in cocoa mode.

The file in cocoa mode can be modified as any text file; in addition
to that, there is a special key binding (C-c C-l) that allows to send
the current line as input to CoCoA. This saves the effort of cutting
and pasting lines of input.

Type C-h C-f cocoa-mode to know about the other functions
available in the cocoa mode.

Customization:
- change the value of the global variable cocoa-dir to the 
  directory where your CoCoA input files are;
- change the value of cocoa-shell-launch-dir to the directory where the 
  CoCoA executable is.
"
;  (setq default-directory cocoa-shell-launch-dir)
  (interactive)
  (require 'comint)
  (require 'easymenu)  ;; MENU

;;;; uncomment the following lines if you have problems with trailing ^M
;;;; you can also add them to your .emacs file
;;(if (null system-uses-terminfo) nil
;;    (autoload 'shell-strip-ctrl-m "shell" nil t)
;;    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
;;    (add-hook 'comint-input-filter-functions 'shell-ctrl-m)
;;)
  (cocoa-make-shell)
;;  (delete-other-windows)
  (save-excursion
    (switch-to-buffer (process-buffer (get-process "cocoa")))
;;;; opens a cocoa file from cocoa-dir
;;    (split-window-vertically)
;;    (other-window 1)
  )
)

(defun cocoaserver ()
;  (setq default-directory cocoa-shell-launch-dir)
  (interactive)
  (require 'comint)
  (require 'easymenu)  ;; MENU

;;;; uncomment the following lines if you have problems with trailing ^M
;;;; you can also add them to your .emacs file
;;(if (null system-uses-terminfo) nil
;;    (autoload 'shell-strip-ctrl-m "shell" nil t)
;;    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
;;    (add-hook 'comint-input-filter-functions 'shell-ctrl-m)
;;)
  (cocoa-make-server-shell)
  (switch-to-buffer-other-frame "*CoCoAServer*")
  (previous-multiframe-window) ;possibly (other-frame -1)
)

;;----------------------------------------------------------------------
;; cocoa-mode-map
;;----------------------------------------------------------------------
(message "cocoa-mode-map")

(defvar cocoa-mode-map ()  "Keymap used in CoCoA mode.")
(if cocoa-mode-map
    ()
;; syntax
  (setq cocoa-mode-map (make-sparse-keymap))
  (define-key cocoa-mode-map ";"       'electric-cocoa-semi-or-dot)
;;  (define-key cocoa-mode-map "."       'electric-cocoa-semi-or-dot)
;;  (define-key cocoa-mode-map ":"       'electric-cocoa-colon)
  (define-key cocoa-mode-map "="       'electric-cocoa-equal)
;;  (define-key cocoa-mode-map "#"       'electric-cocoa-hash)
  (define-key cocoa-mode-map "\r"      'electric-cocoa-terminate-line)
  (define-key cocoa-mode-map "\t"      'electric-cocoa-tab)
  (define-key cocoa-mode-map "\177"    'backward-delete-char-untabify)
  (define-key cocoa-mode-map "\M-\C-a" 'cocoa-beg-of-defun)
;; to send commands to CoCoA
  (define-key cocoa-mode-map "\C-c\C-l" 'cocoa-send-line)
  (define-key cocoa-mode-map "\C-c\C-r" 'cocoa-send-region)
  (define-key cocoa-mode-map "\C-c\C-f" 'cocoa-source-file)
  (define-key cocoa-mode-map "\C-c\C-p" 'pop-to-buffer-cocoa-source-find-error)
  (define-key cocoa-mode-map "\C-c\C-b" 'cocoa-send-buffer)
;; other useful things
  (define-key cocoa-mode-map "\C-c\C-c" 'comment-region)
  (define-key cocoa-mode-map "\C-c\C-u" 'uncomment-region)
  (define-key cocoa-mode-map "\C-c\C-e" 'cocoa-close-block)
  (define-key cocoa-mode-map "\C-c\C-s" 'cocoa-restart-cocoaserver)
    (define-key cocoa-mode-map "\C-c\C-m" 'cocoa-word-man)
  (define-key cocoa-mode-map "\C-cm"    'cocoa-word-man)
  (define-key cocoa-mode-map "\C-c?"    'cocoa-word-man)
;  (define-key cocoa-mode-map "\C-c\C-n" 'cocoa-open-file)
  (define-key cocoa-mode-map "\C-c\C-o" 'cocoa-shell-panic-string)
  (define-key cocoa-mode-map "\C-c\C-q" 'cocoa-shell-quit)
  (define-key cocoa-mode-map "\C-c\C-k" 'cocoa-restart-shell)
;  (define-key cocoa-mode-map "\C-c\C-w" 'cocoa-open-wordlist-file)
)

;;-------------------------------------------------------------->
;; cocoa-menu
;;-------------------------------------------------------------->
(message "cocoa-menu")

(defvar cocoa-mode-menu nil)
(defvar cocoa-menu nil  "Menu for `cocoa-mode'.")
(setq cocoa-menu
      '("CoCoA"
	["Send Line to CoCoA"             cocoa-send-line   t]
	["Send Region to CoCoA"           cocoa-send-region t]
;	["Send Buffer to CoCoA"           cocoa-send-buffer t]
	["Source File into CoCoA"         cocoa-source-file t]
	["Source File Parse Error Line"   pop-to-buffer-cocoa-source-find-error t]
	"---"
	["Use TAB for Completion"         (setq cocoa-tab-is-completion
					    (not cocoa-tab-is-completion))
	 :style toggle
	 :selected cocoa-tab-is-completion
	 :active 't]
	"---"
	["Manual Word"                    cocoa-word-man]
	["Complete Word"                  dabbrev-expand]
	["Indent Region"                  indent-region]
	["Comment Region"                 comment-region]
	["Uncomment Region"               uncomment-region]
	["Print End***;"                  cocoa-close-block]
	"---"
	["Show cocoa-mode version"        cocoa-show-mode-version t]
	["Show cocoa version"             cocoa-show-version t]
	"---"
	["CoCoA not responding?"          cocoa-shell-panic-string t]
	["(Re)start CoCoAServer"          cocoa-restart-cocoaserver t]
	["(Re)start CoCoA4"               cocoa-restart-shell t]
	["Quit CoCoA and Emacs"           cocoa-quit-emacs t]
	))

(message "cocoa-comint-menu")

(defvar cocoa-comint-mode-menu nil)
(defvar cocoa-comint-menu nil  "Menu for `cocoa-comint-mode'.")
(setq cocoa-comint-menu
      '("CoCoA"
	["Source File Parse Error Line"   pop-to-buffer-cocoa-source-find-error t]
	"---"
	["(Re)start CoCoAServer"          cocoa-restart-cocoaserver t]
	["(Re)start CoCoA4"               cocoa-comint-restart-shell t]
	["Quit CoCoA and Emacs"           cocoa-quit-emacs t]
	))

;; (defvar cocoa-statements-menu nil  "Menu for cocoa statements.")
;; (setq cocoa-statements-menu
;;       '("Statements"
;; 	["If"   cocoa-if-statement]
;; 	["For"   cocoa-for-statement]
;; 	["Foreach"   cocoa-foreach-statement]
;; 	["While"   cocoa-while-statement]
;; ))


;; (defun cocoa-while-statement ()
;; ""
;; (insert
;; "While . Do
;;   .
;; EndWhile"
;; )
;; )

(defun cocoa-mode-setup-menubar ()
  "Initial setup of cocoa and insertions menus."
  (progn
    (easy-menu-define			; set up cocoa menu
      cocoa-mode-menu cocoa-mode-map "Menu used in cocoa-mode"
      cocoa-menu)
    (easy-menu-add cocoa-mode-menu cocoa-mode-map) )
)

(defun cocoa-comint-setup-menubar ()
  "Initial setup of cocoa and insertions menus."
  (progn
    (easy-menu-define			; set up menu
      cocoa-comint-mode-menu cocoa-comint-mode-map "Menu used in cocoa-comint-mode"
      cocoa-comint-menu)
    (easy-menu-add cocoa-comint-mode-menu cocoa-comint-mode-map) )
)



(defconst cocoa-xemacs-p (string-match "XEmacs" (emacs-version)))


;;-------------------------------------------------------------->
;; cocoa-open-wordlist-file
;;-------------------------------------------------------------->

(defun cocoa-open-wordlist-file ()
  "The function opens a wordlist file chosen by the user in cocoa-mode"
  (interactive)
  (setq cocoa-wordlist-file 
	(read-file-name "CoCoA word list file: " cocoa-wordlist-default-file))
  (if cocoa-wordlist-file  
      (setq cocoa-wordlist-file cocoa-wordlist-default-file))
  (find-file-noselect cocoa-wordlist-file)
)

;;-------------------------------------------------------------->
;; cocoa-open-file
;;-------------------------------------------------------------->

(setq cocoa-shell-buffer-list '())

(defun cocoa-open-file ()
  "The function opens a new file chosen by the user in cocoa-mode"
  (interactive)
  (setq cocoa-file 
	(read-file-name "Enter name of a CoCoA file: " cocoa-dir))
  (setq cocoa-file-buffer-name (find-file-noselect cocoa-file))
  (switch-to-buffer cocoa-file-buffer-name)
  (cocoa-mode)
  (setq cocoa-shell-buffer-list 
	(cons cocoa-file-buffer-name cocoa-shell-buffer-list))
)

;;-------------------------------------------------------------->
;; cocoa-make-shell
;;-------------------------------------------------------------->
(message "cocoa-make-shell")

(defun cocoa-make-shell ()
  (save-excursion
;;    (setq default-directory cocoa-shell-launch-dir)
    (set-buffer  (make-comint "cocoa" 
			      (concat cocoa-executable)))
    (cocoa-comint-mode) ;; <-- added line.
;; Load wordlist
    (if cocoa-auto-load-wordlist
	(find-file-noselect cocoa-wordlist-default-file 'NOWARN))
  )
)

;;-------------------------------------------------------------->
;; cocoa-make-server-shell
;;-------------------------------------------------------------->

(defun cocoa-make-server-shell ()
  (save-excursion
;;    (setq default-directory cocoa-shell-launch-dir)
    (set-buffer  (make-comint "CoCoAServer" cocoaserver-executable))
  )
)

;;-------------------------------------------------------------->
;; cocoa-mode
;;-------------------------------------------------------------->

;;;###autoload
(defun cocoa-mode ()
  "Major mode for editing a cocoa file that sends input to CoCoA. 
Editing mode with \\[cocoa-send-line] sending the current line as input.

Special commands:
\\{cocoa-mode-map}
"
;;  (setq default-directory cocoa-shell-launch-dir)
  (interactive)
  (kill-all-local-variables)
  (use-local-map cocoa-mode-map)
  (setq major-mode 'cocoa-mode
	mode-name "CoCoA")

  (run-hooks 'cocoa-mode-hook)
)

;;-------------------------------------------------------------->
;; cocoa-send-line
;;-------------------------------------------------------------->

(defun cocoa-send-line ()
  "Send current line as input to CoCoA in the other window.
Note that the cursor can be at any position in the line;
after the input has been sent, the cursor goes to the next non-empty
line in the cocoa file.

Note: if the *cocoa* buffer has no process, CoCoA will be restarted
automatically when this function is called.
"
  (interactive)
  (cocoa-shell-check-process)
  (beginning-of-line)
  (setq cocoa-line-beg (point))
  (end-of-line)
  (setq cocoa-input-line (buffer-substring cocoa-line-beg (point)))
  (setq cocoa-source-buffer (current-buffer)) 
  (cocoa-shell-check-window)
  (save-excursion
;    (switch-to-buffer-other-window "*cocoa*")
    (pop-to-buffer-cocoa-comint)
    (goto-char (point-max))
    (insert cocoa-input-line)
    (comint-send-input)
;    (switch-to-buffer-other-window cocoa-file-buffer-name)
;    (other-window 1)
    (pop-to-buffer-cocoa-source)
    )
  (skip-chars-forward "-*\n-*")
)

;;-------------------------------------------------------------->
;; cocoa-word-man
;;-------------------------------------------------------------->

;; (defun cocoa-word-man1 ()
;;   "Send current word input to CoCoA 'Man' in the other window.
;; Note that the cursor can be at any position in the word;
;; after the input has been sent, the cursor goes to the next non-empty
;; line in the cocoa file.
;; Note: if the *cocoa* buffer has no process, CoCoA will be restarted
;; automatically when this function is called.
;; "
;;   (interactive)
;;   (cocoa-shell-check-process)
;;   (setq word-regexp "[a-zA-Z][a-zA-Z0-9_]*")
;;   (setq word-boundary "\\(^\\|[^a-zA-Z_]\\)")
;;   ;; Now mark the word and save to string.
;;   (forward-char)
;;   (re-search-backward (concat word-boundary word-regexp))
;;   (or (re-search-forward word-regexp (point-max) t)
;;       (error "No word found to check!"))
;;   (setq start (match-beginning 0)
;; 	end (point)
;; 	word (buffer-substring start end))
;; ;  (setq cocoa-input-line (concat "Man('" word "');"))
;;   (setq cocoa-input-line (concat "? " word))
;;   (cocoa-shell-check-window)
;;   (save-excursion
;;     (switch-to-buffer-other-window "*cocoa*")
;; ;    (pop-to-buffer "*cocoa*")
;;     (goto-char (point-max))
;;     (insert cocoa-input-line)
;;     (comint-send-input)
;; ;    (switch-to-buffer-other-window cocoa-file-buffer-name)
;;     (other-window 1)
;;     )
;;   (skip-chars-forward "-*\n-*")
;; )

;;-------------------------------------------------------------->
;; cocoa-show-version
;;-------------------------------------------------------------->

(defun cocoa-exec-this (arg)
  "Send arg to CoCoA
"
  (cocoa-shell-check-process)
  (cocoa-shell-check-window)
  (setq cocoa-source-buffer (current-buffer)) ;  for jumping back to it. Burki
  (save-excursion
;    (switch-to-buffer-other-window "*cocoa*")
;    (pop-to-buffer "*cocoa*")
    (pop-to-buffer-cocoa-comint)
    (goto-char (point-max))
    (insert arg)
    (comint-send-input)
;    (switch-to-buffer-other-window cocoa-file-buffer-name)
;    (other-window 1)
    (pop-to-buffer-cocoa-source)
    )
  (skip-chars-forward "-*\n-*")
)

(defun cocoa-show-version ()
  (interactive)
  (cocoa-exec-this "Comp(CocoaInfos(), \"Version\");")
)

(defun cocoa-show-mode-version ()
  (interactive)
  (cocoa-exec-this (concat "-- " cocoa-mode-version))
)

(defun cocoa-shell-panic-string ()
  (interactive)
  (cocoa-exec-this "\"'*/ # \"CoCoA \"+/*NOT*/\"ready\"; -- repeat to get out of pending statements")
)

(defun cocoa-word-man ()
  "Send current word input to CoCoA 'Man' in the *cocoa* window.
Note that the cursor can be at any position in the word;
after the input has been sent, the cursor goes to the next non-empty
line in the cocoa file.
Note: if the *cocoa* buffer has no process, CoCoA will be restarted
automatically when this function is called.
"
  (interactive)
  (cocoa-shell-check-process)
  (setq word-regexp "[a-zA-Z][a-zA-Z0-9_]*")
  (setq word-boundary "\\(^\\|[^a-zA-Z_]\\)")
  ;; Now mark the word and save to string.
  (forward-char)
  (re-search-backward (concat word-boundary word-regexp))
  (or (re-search-forward word-regexp (point-max) t)
      (error "No word found to check!"))
  (setq start (match-beginning 0)
	end (point)
	word (buffer-substring start end))
  (cocoa-exec-this (concat "? " word))
)
;;-------------------------------------------------------------->
;; cocoa-shell-quit
;;-------------------------------------------------------------->
(message "cocoa-shell-quit")

(defun cocoa-shell-quit ()
  "Quit CoCoA after saving all notebook files."
  (interactive)
  (message "saving all notebook files . . .")
  (save-excursion
    (while (not (null cocoa-shell-buffer-list))
      (setq cocoa-shell-buffer-tosave (car cocoa-shell-buffer-list))
      (if (buffer-name cocoa-shell-buffer-tosave) 
	  (progn 
	    (switch-to-buffer cocoa-shell-buffer-tosave)
	    (save-buffer)
	    (kill-buffer cocoa-shell-buffer-tosave)))
      (setq cocoa-shell-buffer-list (cdr cocoa-shell-buffer-list))
      ))
  (cocoa-shell-quit-cocoa-shell)
  (message "  . . . bye!  ")
)

;;-------------------------------------------------------------->
;; cocoa-kill-shell-process, cocoa-kill-server-process
;;-------------------------------------------------------------->

(defun cocoa-kill-shell-process ()
  (if (not (eq (get-buffer-process "*cocoa*") nil)) 
      (delete-process "*cocoa*"))
;;      (process-kill-without-query (get-process "cocoa")))
)

(defun cocoa-kill-server-process ()
  (if (not (eq (get-buffer-process "*CoCoAServer*") nil)) 
      (delete-process "*CoCoAServer*"))
)

;;-------------------------------------------------------------->
;; cocoa-quit-emacs
;;-------------------------------------------------------------->

(defun cocoa-quit-emacs ()
"Quit cocoa and then emacs."

(interactive)
;;(process-kill-without-query (get-process "cocoa"))
(cocoa-kill-shell-process)
(cocoa-kill-server-process)
(save-buffers-kill-emacs)
)

;;-------------------------------------------------------------->
;; cocoa-restart-cocoaserver
;;-------------------------------------------------------------->
(message "cocoa-restart-cocoaserver")

(defun cocoa-restart-cocoaserver ()
"(Re)start CoCoAServer in the buffer named *CoCoAServer*."

(interactive)
(cocoa-kill-server-process)
(cocoa-make-server-shell)
(switch-to-buffer-other-frame "*CoCoAServer*")
(previous-multiframe-window) ;possibly (other-frame -1)
)

;;-------------------------------------------------------------->
;; cocoa-restart-shell
;;-------------------------------------------------------------->
(message "cocoa-restart-shell")

(defun cocoa-comint-restart-shell ()
"(re)start CoCoA process in the buffer named *cocoa*."

(interactive)
(cocoa-kill-shell-process)
(cocoa-make-shell)
)

(defun cocoa-restart-shell ()
"(Re)start CoCoA process in the buffer named *cocoa*."

(interactive)
(cocoa-kill-shell-process)
(cocoa-make-shell)
(switch-to-buffer-other-window "*cocoa*")
(other-window 1)
)

;;-------------------------------------------------------------->
;; cocoa-send-buffer
;;-------------------------------------------------------------->

(defun cocoa-send-buffer ()
  "Send current buffer as input to CoCoA. See \\[cocoa-send-region] for 
more information. Does not save the buffer, so it's useful for trying 
experimental versions.
See \\[cocoa-send-file] for an alternative.

Note: if the *cocoa* buffer has no process, CoCoA will be restarted
automatically when this function is called.
"
  (interactive)
  (cocoa-shell-check-process)
  (setq kill-ring '())
  (copy-region-as-kill (point-min) (point-max))
  (cocoa-shell-check-window)
  (save-excursion
    (switch-to-buffer-other-window "*cocoa*")
    (if (not (eq (process-status "cocoa") "run")) (cocoa-make-shell))
    (goto-char (point-max))
    (yank)
    (comint-send-input)
;  (switch-to-buffer-other-window cocoa-file-buffer-name)
    (other-window 1)
  )
)

;;-------------------------------------------------------------->
;; cocoa-send-region
;;-------------------------------------------------------------->

(defun cocoa-send-region ()
  "Send current region as input to CoCoA.

Note: if the *cocoa* buffer has no process, CoCoA will be restarted
automatically when this function is called.
"
  (interactive)
  (cocoa-shell-check-process)
  (setq kill-ring '())
  (copy-region-as-kill (point) (mark))
  (cocoa-shell-check-window)
  (save-excursion
    (switch-to-buffer-other-window "*cocoa*")
    (if (not (eq (process-status "cocoa") "run")) (cocoa-make-shell))
    (goto-char (point-max))
    (yank)
    (comint-send-input)
;    (switch-to-buffer-other-window cocoa-file-buffer-name)
    (other-window 1)
  )
)

;;-------------------------------------------------------------->
;; cocoa-source-file
;;-------------------------------------------------------------->

(defun cocoa-source-file ()
  "Saves current file (no prompt!) and send current file's as input to CoCoA.

Note: if the *cocoa* buffer has no process, CoCoA will be restarted
automatically when this function is called.
"
  (interactive)
  (cocoa-shell-check-process)
  (save-buffer)
  (setq cocoa-source-buffer (current-buffer)) ;  for jumping back to it. Burki
  (setq cocoa-source-file buffer-file-name)
  (setq cocoa-shell-source-command (concat "Source \"" cocoa-source-file "\";"))
  ;;    (if (cocoa-shell-running)
  ;;        (cocoa-shell-kill-job)
  ;;      (cocoa-make-shell)
  ;;     )
  (cocoa-shell-check-window)
  (save-excursion
    (pop-to-buffer-cocoa-comint)
    (if (not (eq (process-status "cocoa") "run")) (cocoa-make-shell))
    (goto-char (point-max))
    (insert cocoa-shell-source-command)
    (comint-send-input)
    (pop-to-buffer-cocoa-source)
  )
)

;;-------------------------------------------------------------->
;; cocoa-shell-check-process
;;-------------------------------------------------------------->
(message "cocoa-shell-check-process")

(defun cocoa-shell-check-process ()
  (if (eq (get-buffer-process "*cocoa*") nil) (cocoa-make-shell) )
)

(defun cocoa-shell-check-window ()
  (if (get-buffer-window "*cocoa*" 0)
      ()
      (progn
	(delete-other-windows)
	(split-window-vertically)
	(other-window -1)
      )
  )
)



;; ;; installation: Append this file to your cocoa.el file.

;; ;;-------------------------------------------------------------->
;; ;; cocoa-comint-mode
;; ;; =
;; ;; comint-mode + things for CoCoA.
;; ;;
;; ;; things = F2 key, ...
;; ;;
;; ;; Burkhard Zimmermann, Friday May 13, 2005.
;; ;;-------------------------------------------------------------->


;; ;; tested with: emacs:              GNU emacs 21.3.1, 
;; ;;              cocoa-mode-version: "21 December 2004",
;; ;;              Windows XP.

;; (message "Adding F2 to CoCoA mode.")
;; (set 'cocoa-mode-version (concat cocoa-mode-version " - [F2]"))

;; ;; F2 = send file to CoCoA, similarly to C-c C-a.
;; (add-hook 'cocoa-mode-hook 
;;    (lambda () (define-key cocoa-mode-map [f2] 'cocoa-send-file-xxx)
;;               (define-key cocoa-mode-map [f1] 'cocoa-word-man))) ; Burki.
;; ; (I put the to the define-key in the hook so that I don't need to modify cocoa.el at all.)

;; ;; change to the function: cocoa-make-server-shell
;; (defun cocoa-make-server-shell ()
;;   (save-excursion
;;     (set-buffer  (make-comint "CoCoA5Server" 
;; 			      (concat cocoa5server-executable)))
;;     (cocoa-comint-mode) ;; <-- added line.
;;   ))

;; ;;-------------------------------------------------------------->
;; ;; cocoa-send-file-xxx
;; ;;
;; ;; Similar to cocoa-send-file. differences:
;; ;;
;; ;; - remembers cocoa-source-buffer. that's used for jumping back.
;; ;; - gives focus to the comint buffer.
;; ;; - keeps the windows layout. (uses pop-to-buffer. doesn't call cocoa-shell-check-window)
;; ;;-------------------------------------------------------------->

;; (defun cocoa-send-file-xxx ()
;;   "Sends the content of the buffer to CoCoA. Similar to cocoa-send-file."
;;   (interactive)
;;   (cocoa-shell-check-process)
;;   (save-buffer) ;; <--- ;; to do: don't save autoamtically. save in a temporal file only. 
;;   (setq cocoa-shell-input-file buffer-file-name)
;;   (setq cocoa-shell-input-file-name (concat (concat "<< '" cocoa-shell-input-file) "';"))
;;   ;; I don't like to have my window layout changed to a horizontal split by:
;;   ;; (cocoa-shell-check-window)
;;   (save-excursion
;;     (set 'cocoa-source-buffer (current-buffer)) ; <---  for jumping back to it. ; Burki
;;     (pop-to-buffer "*cocoa*")
;;     (if (not (eq (process-status "cocoa") "run")) (cocoa-make-shell))
;;     (goto-char (point-max))
;;     (insert cocoa-shell-input-file-name)
;;     (comint-send-input)
;;   ))


;; ;;-------------------------------------------------------------->
;; ;; cocoa-comint-mode
;; ;; =
;; ;; comint-mode + things for CoCoA.
;; ;;-------------------------------------------------------------->

(message "cocoa-comint-mode")
(defun cocoa-comint-mode ()
  "Major mode for interacting with CoCoA. Similar to `comint-mode'.
\\{cocoa-comint-mode-map}"
  (interactive)
  ;; stolen from maplev.el:
  (comint-mode)
  (setq ;;comint-prompt-regexp (concat "^\\(" maplev-cmaple-prompt "\\)+ *")
          ;; Maple uses "> " where CoCoA uses "" as a prompt.
        ;; comint-use-prompt-regexp-instead-of-fields t
        comint-eol-on-send t ;; ?
        major-mode 'cocoa-comint-mode
        mode-name  "cocoa-comint")
  (use-local-map cocoa-comint-mode-map)
  (cocoa-comint-setup-menubar)
  (run-hooks 'cocoa-comint-mode-hook)
  )

;; shouldn't this go into the function cocoa-comint-mode?
(defvar cocoa-comint-mode-map nil
  "Keymap used in cocoa-comint mode.")
(require 'comint)
(unless cocoa-comint-mode-map
  (let ((map (copy-keymap comint-mode-map)  ))
    (define-key map [f2] 'pop-to-buffer-cocoa-source-find-error) 
;;    (define-key map [f1] 'cocoa-word-man) 
    (setq cocoa-comint-mode-map map)))



;; ; jump to the error line.

;; ; ERROR: parse error in line 5 of device e:\burki\systems\cocoa\test.coc
;; ; -> put the cursor there.
;; ; ERROR: parse error in line 31 of device stdin
;; ; -> take no action.

(defun pop-to-buffer-cocoa-source-find-error ()
  "If there is an error, set the cursor at the error line."
  (interactive)
  (let ((found nil)
        (emesg nil) 
        (eline nil)
        (efile nil))
  (progn
;;    (message "debug cocoa-find-error.")
;;    (switch-to-buffer "*cocoa*") ; useless, since we are already there. 
    (pop-to-buffer-cocoa-comint)
    (goto-char comint-last-input-start)
    ; search for an error message.
    (if (re-search-forward "^ERROR: \\(.*\\) in line \\([0-9]+\\) of device \\(.*\\)$" nil t)
	(progn 
	   (setq found t)
	   (setq emesg (buffer-substring (match-beginning 1) (match-end 1)))
	   (setq eline (buffer-substring (match-beginning 2) (match-end 2)))
	   (setq efile (buffer-substring (match-beginning 3) (match-end 3)))
	))
        
    (goto-char (point-max)) ; go to the end of the *cocoa* buffer. 
;;    (message "debug cocoa-find-error: found=%s, efile=%s, eline=%s, emesg=%s" found efile eline emesg)
    ; messages such as "ERROR: parse error in line 31 of device stdin" 
    ; cannot be used for jumping to the error line.
    ; in such a case, proceed as if no error was found.
    (if (and found (string-equal efile "stdin"))
      (progn
	(message "error in stdin: %s" emesg)
	(setq found nil)
      ))
    ; messages about files that do not exist are useless.
    ; should never happen.
    ; in such a case, proceed as if no error was found.
    (if (and found (not (file-exists-p efile)))
      (progn
	(message "%s, line %s: %s. Emacs can't open the file efile." efile eline emesg efile)
	(setq found nil)
      ))
    (if found
      (progn 
	 (message "%s, line %s: %s" efile eline emesg)

         (pop-to-buffer-cocoa-source) ;; <--- bad. ignores the efile. 
;;	 (find-file efile)            ;; <--- bad: doesn't behave like pop-to-buffer.

	 (goto-line (string-to-int eline))
      )
      (pop-to-buffer-cocoa-source) ; no error found. still, go back to the source buffer.
      )
   ))) 

(defun pop-to-buffer-cocoa-source ()
  "Switch to the cocoa source buffer, if it exists."
  (interactive)
  (try-pop-to-buffer cocoa-source-buffer))

(defun pop-to-buffer-cocoa-comint ()
  "Switch to the *cocoa* comint buffer, if it exists."
  (interactive)
  (try-pop-to-buffer "*cocoa*"))

(defun try-pop-to-buffer (buffer)
  "Switch to buffer, if it exists."
  (let ((buf (get-buffer buffer)))
    (if buf
; switches buffer, but keeps the window layout:
; never changes the number and the sizes of the visible windows in the current frame.
	(pop-to-buffer buf)  
        ;; would destroy the window layout: (switch-to-buffer buf)
      (message "No buffer \"%s\"." buffer))))


; Optional feature: tab completion.
;; ANNA: achieved in electric-cocoa-tab
;(add-hook 'cocoa-mode-hook 
;   (lambda () (local-set-key (kbd "<tab>") 'dabbrev-expand)))

;; (add-hook 'cocoa-comint-mode-hook 
;;    (lambda () (local-set-key (kbd "<tab>") 'dabbrev-expand)))

(defun cocoa-syntax () 
  (progn 
  ;; Copied from the function cocoa-mode. 
  ;; to do: avoid to duplicate code. 
  (setq local-abbrev-table cocoa-abbrev-table)
  (set-syntax-table cocoa-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'cocoa-indent-line)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)

  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip  "/\\*+ *\\|// *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'cocoa-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)

;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cocoa-font-lock-keywords nil nil))
;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression cocoa-imenu-generic-expression)
;; Load wordlist
  (if cocoa-auto-load-wordlist
	 (find-file-noselect cocoa-wordlist-default-file 'NOWARN))
;; Menu
  (cocoa-mode-setup-menubar)
  ))

; Optional feature: syntax highlighting. doesn't work fully. 
;;(add-hook 'cocoa-comint-mode-hook 'cocoa-syntax);; loses bindings in the menu!
(add-hook 'cocoa-mode-hook 'cocoa-syntax)

;(set 'pop-up-windows nil)

(defun cocoa-exec-this-and-stay-in-comint (arg)
  "Send arg to CoCoA
"
;  (cocoa-shell-check-process)
;;  (cocoa-shell-check-window) ; <---
  (save-excursion
;    (switch-to-buffer-other-window "*cocoa*")
    (pop-to-buffer-cocoa-comint)
;    (if (not (eq (process-status "cocoa") "run")) (cocoa-make-shell))
    (goto-char (point-max))
;    (insert "debug ")
    (insert arg)
    (comint-send-input)
;    (switch-to-buffer-other-window cocoa-file-buffer-name)
;    (other-window 1)

    ;; scroll to the beginning of the help
    (goto-char comint-last-input-start)
    ; todo: scroll so that the line with the cursor is the top line. 
    (recenter 0)

;    (insert "aasdf")
    )
;  (skip-chars-forward "-*\n-*")
)

;; ;(global-set-key [f1] 'cocoa-word-man)

;; ;; don't split windows:
;; ; (set 'pop-up-windows nil)

;; ;; don't create new frames:
;; ; (set 'pop-up-frames nil)
