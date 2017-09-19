;; emacsForth --- Forth in Emacs Lisp  uh 2017-09-14

(require 'cl-lib)
(require 'derived)

(define-derived-mode emacsForth-mode text-mode "emacsForth"
  "Major mode with embedded Forth system.
Special Commands:
\\(emacsForth-mode-map)"
  (make-variable-buffer-local 'lexical-binding)
  (setq mode-line-format
	(append mode-line-format
		'((:eval (propertize (concat "B: " (format "%d" forth-base))))
		  "  "
		  (:eval (propertize (concat "S: " (forth-stack-to-string)))))))
  (define-key emacsForth-mode-map (kbd "<RET>") 'forth-interpret-line)
  (forth-cold)
  (insert " ok\n")
  )


(defvar forth-stack)
(defvar forth-return-stack)


(defun forth-clear-stack ()
  (setq forth-stack '()))

(defun forth-clear-return-stack ()
  (setq forth-return-stack '()))

(defun forth-lit (x)
  (push x forth-stack))

(defun forth-dup ()
  (push (car forth-stack) forth-stack))

(defun forth-swap ()
  (let ((tos (pop forth-stack))
	(nos (pop forth-stack)))
    (push tos forth-stack)
    (push nos forth-stack)))
    
(defun forth-drop ()
  (pop forth-stack))

(defun forth-over ()
  (push (cadr forth-stack) forth-stack))

(defmacro forth-binop (op)
  `(let ((tos (pop forth-stack))
	 (nos (pop forth-stack)))
     (push (,op nos tos) forth-stack)))

(defun forth-add () (forth-binop +))
(defun forth-sub () (forth-binop -))
(defun forth-mul () (forth-binop *))
(defun forth-div () (forth-binop /))

(defun forth-zero-equal ()
  (push (if (eql (pop forth-stack) 0) -1 0) forth-stack ))

(defun forth-toR ()
  (push (pop forth-stack)  forth-return-stack))

(defun forth-Rfrom ()
  (push (pop forth-return-stack) forth-stack))

(defun forth-emit ()
  (insert (pop forth-stack)))

(defun forth-cr ()
  (newline))


(defvar forth-base 10)
(defun forth-hex () (setq forth-base 16))
(defun forth-decimal () (setq forth-base 10))


(defmacro forth-defheader (name &rest props)
  `(list ,name
	 (list 'name ,name)
	 ,@(mapcar
	    (lambda (e)
	      (if (atom e)
		  (list 'quote (list e e)) 
		  (list 'quote e)))
	    props)))

(defun forth-get-header-property (header key)
  (cadr (assoc key (cdr header))))

(defun forth-remove-header-property (header key)
  (let* ((props (cdr header))
	 (oldprop (assoc key props)))
	 (if oldprop
	     (setcdr header (delq oldprop props))) ; remove old property
	 header))

(defun forth-set-header-property (header key &optional value)
  (let* ((prop (if (null value) (list key key) (list key value)))
	 (props (cdr header))
	 (oldprop (assoc key props)))
	 (if oldprop
	     (setcdr header (delq oldprop props))) ; remove old property
	 (setcdr header (cons prop (cdr header))))
  header)


(defmacro forth-defword (name interp comp &rest props)
   `(list ,name
	 (list 'name ,name)
	 (list 'interpretation-semantics (function ,interp))
	 (list 'compilation-semantics (function ,comp))
	 ,@(mapcar
	    (lambda (e)
	      (if (atom e)
		  (list 'quote (list e e)) 
		  (list 'quote e)))
	    props)))

(defmacro forth-defprimitive (name interp &rest props)
  `(forth-defword ,name ,interp (forth-compile . ,interp) ,@props))

;(defmacro forth-primitive (name sem &optional imm)
;   `(list ,name
;	 (list 'name ,name)
;	 (list 'interpretation-semantics (function ,sem))
;	 (list 'compilation-semantics (forth-compile (function ,sem)))
;	 ,@(if imm (list (list 'list (list 'quote 'immediate))))))
	 
(defmacro forth-defcompiler (name sem &optional imm)
   `(list ,name
	 (list 'name ,name)
	 (list 'compilation-semantics (function ,sem))
	 ,@(if imm (list (list 'list (list 'quote 'immediate))))))

(defvar forth-wordlist
  (list
   (forth-defprimitive "drop" forth-drop)
   (forth-defprimitive "dup" forth-dup)
   (forth-defprimitive "swap" forth-swap)
   (forth-defprimitive "over" forth-over)
   (forth-defprimitive "+" forth-add)
   (forth-defprimitive "-" forth-sub)
   (forth-defprimitive "0=" forth-zero-equal)
   (forth-defprimitive "emit" forth-emit)
   (forth-defprimitive "ok" forth-noop)
   (forth-defprimitive ">r" forth-toR)
   (forth-defprimitive "r>" forth-Rfrom)
   (forth-defprimitive "." forth-dot)
   (forth-defprimitive ".s" forth-dot-stack)
   (forth-defprimitive "hex" forth-hex)
   (forth-defprimitive "decimal" forth-decimal)
   (forth-defword "[" forth-start-interpretation forth-start-interpretation)
   (forth-defprimitive "]" forth-start-compilation)
   (forth-defprimitive "words" forth-words)
   (forth-defprimitive "page" forth-page)
   (forth-defprimitive "cold" forth-cold)
   (forth-defprimitive ":" forth-colon)
   (forth-defcompiler ";" forth-semicolon)
   (forth-defprimitive "include" forth-include)
   (forth-defprimitive "clearstack" forth-clear-stack)
   (forth-defword ".(" forth-dot-paren forth-dot-paren)
   (forth-defword "(" forth-paren forth-paren)
   (forth-defprimitive "cr" forth-cr)
   (forth-defprimitive "immediate" forth-set-immediate)
   (forth-defcompiler "ahead" forth-ahead)
   (forth-defcompiler "if" forth-if)
   (forth-defcompiler "else" forth-else)
   (forth-defcompiler "then" forth-then)
   (forth-defcompiler "begin" forth-begin)
   (forth-defcompiler "while" forth-while)
   (forth-defcompiler "repeat" forth-repear)
   (forth-defcompiler "again" forth-again)
   (forth-defcompiler "until" forth-until)
   ))

   
  ;;   `("drop" (name "drop") (stack-comment "x -- ") (help "remove the top stack item")
  ;;    (interpretation-semantics ,(function forth-drop))
  ;;    (compilation-semantics ,(forth-compile (function forth-drop)))
  ;;    )
  ;;   `("dup" (name "dup") (stack-comment "x -- x x") (help "duplicate top stack item")
  ;;    (interpretation-semantics ,(function forth-dup))
  ;;    (compilation-semantics ,(forth-compile (function forth-dup)))
  ;;    )
  ;;   `("swap" (name "swap") (stack-comment "x1 x2 -- x2 x1") (help "swap the two topmost stack items")
  ;;    (interpretation-semantics ,(function forth-swap))
  ;;    (compilation-semantics ,(forth-compile (function forth-swap)))
  ;;    )
  ;;   `("over" (name "over") (stack-comment "x1 x2 -- x1 x2 x2") (help "duplicate the second topmost stack itemxs")
  ;;    (interpretation-semantics ,(function forth-over))
  ;;    (compilation-semantics ,(forth-compile (function forth-over)))
  ;;    )
  ;;   `("+" (name "+") (stack-comment "x x -- x") (help "sum the two topmost stack items")
  ;;    (interpretation-semantics ,(function forth-add))
  ;;    (compilation-semantics ,(forth-compile (function forth-add)))
  ;;    )
  ;;   `("-" (name "-") (stack-comment "x x -- x") (help "subtract the two topmost stack items")
  ;;    (interpretation-semantics ,(function forth-sub))
  ;;    (compilation-semantics ,(forth-compile (function forth-sub)))
  ;;    )
  ;;   `("emit" (name "emit") (stack-comment "x -- ") (help "output character x")
  ;;    (interpretation-semantics ,(function forth-emit))
  ;;    (compilation-semantics ,(forth-compile (function forth-emit)))
  ;;    )
  ;;   ;("ok" (name "ok") (stack-comment "-- ") (help "ignored")
  ;;   ; (interpretation-semantics ,(lambda ()))
  ;;   ; (compilation-semantics ,(lambda ()))
  ;;   ; )
  ;;   `(">r" (name ">r") (stack-comment "x --") (help "move top of stack item to rturn stack")
  ;;    (interpretation-semantics ,(function forth-toR))
  ;;    (compilation-semantics ,(forth-compile (function forth-toR)))
  ;;    )
  ;;   `("r>" (name "r>") (stack-comment "-- x") (help "move top return stack item to data stack")
  ;;    (interpretation-semantics ,(function forth-Rfrom))
  ;;    (compilation-semantics ,(forth-compile (function forth-Rfrom)))
  ;;    )
  ;;   `("." (name ".") (stack-comment "x --") (help "")
  ;;    (interpretation-semantics ,(function forth-dot))
  ;;    (compilation-semantics ,(forth-compile (function forth-dot)))
  ;;    )
  ;;   `(".s" (name ".") (stack-comment "i*x -- i*x") (help "")
  ;;    (interpretation-semantics ,(function forth-dot-stack))
  ;;    (compilation-semantics ,(forth-compile (function forth-dot-stack)))
  ;;    )
  ;;   `("hex" (name "hex") (stack-comment "--") (help "")
  ;;    (interpretation-semantics ,(function forth-hex))
  ;;    (compilation-semantics ,(forth-compile (function forth-hex)))
  ;;    )
  ;;   `("decimal" (name "decimal") (stack-comment "--") (help "")
  ;;    (interpretation-semantics ,(function forth-decimal))
  ;;    (compilation-semantics ,(forth-compile (function forth-decimal)))
  ;;    )
  ;;   `("[" (name "[") (stack-comment "--") (help "")
  ;;    (interpretation-semantics ,(function forth-start-interpretation))
  ;;    (compilation-semantics ,(forth-compile (function start-interpretation)))
  ;;    (immediate)
  ;;    )
  ;;   `("]" (name "]") (stack-comment "-- x") (help "")
  ;;    (interpretation-semantics ,(function forth-start-compilation))
  ;;    (compilation-semantics ,(forth-compile (function forth-start-compilation)))
  ;;    )
  ;;   `("words" (name "words") (stack-comment "--") (help "")
  ;;    (interpretation-semantics ,(function forth-words))
  ;;    (compilation-semantics ,(forth-compile (function forth-words)))
  ;;    )
  ;;   `("page" (name "page") (stack-comment "--") (help "")
  ;;    (interpretation-semantics ,(function forth-page))
  ;;    (compilation-semantics ,(forth-compile (function forth-page)))
  ;;    )
  ;;   `("if" (name "if") (stack-comment "f --") (help "conditionally execute words")
  ;;    (compilation-semantics ,(forth-compile (function forth-drop)))
  ;;    (immediate)
  ;;    )
  ;;   (forth-primitive "cold" forth-cold)
  ;;   )


(defvar forth-last-definition nil)
(defvar forth-last-header nil)
(defvar forth-dp nil)
(defvar forth-ip nil)

(defun forth-compile (sem)
  (setcdr forth-dp (cons sem nil))
  (setq forth-dp (cdr forth-dp)))

(defun forth-compile-lit (num)
  (forth-compile (cons (function forth-lit) num)))

(defun forth-docolon (ip)
  (push forth-ip forth-return-stack)
  (setq forth-ip ip)
  (while forth-ip
    (let ((op (car forth-ip)))
      (setq forth-ip (cdr forth-ip))
      (forth-execute op)))
  (setq forth-ip (pop forth-return-stack)))

(defun forth-colon ()
  (let ((name (forth-parse-name)))
    (setq forth-last-definition (cons 'FORTH-CODE nil))
    (setq forth-dp forth-last-definition)
    (setq forth-last-header (forth-defheader name))
    (forth-start-compilation)))

(defun forth-semicolon ()
  (let* ((h forth-last-header)
	 (d forth-last-definition)
	 (s (cons (function forth-docolon) (cdr d)))
	 (c (cons (function forth-compile) s)))
    (forth-set-header-property h 'interpretation-semantics s)
    (forth-set-header-property h 'compilation-semantics c) ; set standard compilation semantics
    (setq forth-wordlist (cons h forth-wordlist))
    (forth-start-interpretation)))

(defun forth-ahead ()
  (forth-compile (function forth-branch))
  (push forth-dp forth-stack)
  (forth-compile 'UNRESOLVED-FORWARD-BRANCH))

(defun forth-question-branch (dest)
  (if (eql 0 (pop forth-stack))
      (setq forth-ip (cdr dest))))

(defun forth-branch (dest)
  (setq forth-ip (cdr dest)))


(defun forth-if ()
  (let ((bra (cons (function forth-question-branch) 'UNRESOLVED-CONDITIONAL-FORWARD-BRANCH)))
    (forth-compile bra)
    (push bra forth-stack)))

(defun forth-else ()
  (let ((orig (pop forth-stack))
	(bra (cons (function forth-branch) 'UNRESOLVED-FORWARD-BRANCH)))
    (forth-compile bra)
    (push bra forth-stack)
    (setcdr orig forth-dp)))
	
(defun forth-then ()
  (setcdr (pop forth-stack) forth-dp))

(defun forth-begin ()
  (push forth-dp forth-stack))


(defun forth-while ())
(defun forth-repeat ())

(defun forth-until ()
  (let* ((orig (cdr (pop forth-stack)))
	 (bra (cons (function forth-question-branch) orig)))
    (forth-compile bra)))

(defun forth-again ())

(defun forth-noop ())


;(defmacro forth-primitive (name sem)
;   (list 'list name
;	 (list 'list (list 'quote 'name) name)
;	 (list 'list (list 'quote 'interpretation-semantics)
;	       (list 'function sem))
;	 (list 'list (list 'quote 'compilation-semantics))
;	       (list 'forth-compile (list 'function sem))))

;(defun forth-cold ()
;  (insert "emacsForth\n")
;  (forth-clear-return-stack)
;  (forth-clear-stack)
;  (forth-quit))

(defun forth-quit ()
  (setq forth-handle-token (function forth-interpreter))
  (let ((input ""))
    (while (setq input (forth-refill))
      (let ((err (catch 'forth-quit-loop (forth-interpret input))))
	(cond
	 ((eq err 'ok)
	  (insert " ok\n")
	  (insert (format "Stack: %s\n" (forth-stack-to-string))))
	 (t
	  (insert (format " Error: %s\n" err))))))))

(defun forth-stack-to-string ()
  (if (null forth-stack)
      "empty"
    (mapconcat (lambda (e) (format "%s" e)) (reverse forth-stack) " ")))

(defun forth-dot-stack ()
  (insert (forth-stack-to-string)))

(defun forth-refill ()
  (read-string "forth> "))

(defun forth-string-to-number (token)
  (let ((num (string-to-number token)))
    (cond
     ((/= num 0) num)
     ((string-equal token "0") 0)
     (t nil))))

(defvar forth-handle-token (function forth-interpreter))

(defun forth-start-compilation ()
  (setq forth-handle-token (function forth-compiler)))

(defun forth-start-interpretation ()
  (setq forth-handle-token (function forth-interpreter)))


(defun forth-cold ()
  (forth-clear-stack)
  (forth-clear-return-stack)
  (forth-start-interpretation)
  (insert "emacsForth 1.0\n "))


(defun forth-dot ()
  (insert (format "%s" (pop forth-stack)))
  (insert " "))

(defun forth-dot-paren ()
  (insert (forth-parse ")")))

(defun forth-paren ()
  (forth-parse ")"))


(defun forth-words ()
  (insert (format "%s"
		  (mapconcat (lambda (w) (car w))
			     forth-wordlist " ")))
  (insert (format "\n( %d words )" (length forth-wordlist))))

(defun forth-page ()
  (erase-buffer))

(defun forth-execute (sem)
  ; (insert (format "execute %s\n" sem))
  (if (consp sem)
      (apply (car sem) (list (cdr sem)))
    (funcall sem)))

(defvar forth-input nil)
(defvar forth-input-index 0)


(defun forth-interpreter (token)
  (let ((word (assoc token forth-wordlist)))
    (if word
	(let ((sem (cadr (assoc 'interpretation-semantics word))))
	  (if sem
	      (forth-execute sem)
	    (throw 'forth-quit-loop (format "%s has no interpretation semantics" token))))
      (let ((num (forth-string-to-number token)))
	(if num
	    (forth-lit num)
	  (throw 'forth-quit-loop (format "%s?" token)))))))
     
(defun forth-compiler (token)
  (let ((word (assoc token forth-wordlist)))
    (if word
	(let ((sem (cadr (assoc 'compilation-semantics word))))
	  (if sem
	      (forth-execute sem)
	    (throw 'forth-quit-loop (format "%s has no compilation semantics" token))))
      (let ((num (forth-string-to-number token)))
	(if num
	    (forth-compile-lit num)
	  (throw 'forth-quit-loop (format "%s?" token)))))))
      

(defun forth-interpret ()
  (while (> (length forth-input) forth-input-index)
    (let ((token (forth-parse-name)))
      (if token (apply forth-handle-token (list token)))))
    'ok)

(defun forth-parse-name ()
  (if (string-match "[ \t\n\f]*" forth-input forth-input-index)
      (setq forth-input-index (match-end 0)))
  (if (string-match "\\([^ \t\n\f]+\\)[ \t\n\f]?" forth-input forth-input-index)
      (progn
	(setq forth-input-index (match-end 0))
	(match-string 1 forth-input))))

(defun forth-parse (delim)
  (if (member (string-to-char delim) (string-to-list ".*+?[]^$\\"))
      (setq delim (concat "\\" delim)))
  (if (string-match  (concat "\\([^" delim "]*\\)" delim) forth-input forth-input-index)
      (progn
	(setq forth-input-index (match-end 0))
	(match-string 1 forth-input))))

(defun forth-set-immediate ()
  (forth-set-header-property
   forth-last-header
   'compilation-semantics
   (forth-get-header-property forth-last-header 'interpretation-semantics)))


(defun forth-interpret-line ()
  (interactive)
  (insert " ")
  (setq forth-input (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
  (message forth-input)
  (setq forth-input-index 0)
  (let ((err (catch 'forth-quit-loop (forth-interpret))))
    (message "done")
    (cond
     ((eq err 'ok)
      (insert " ok")
      (newline))
     (t
      (insert (format " %s" err))
      (newline)
      (forth-start-interpretation)))))


(defun forth-interpret-buffer ()
  (interactive)
  (forth-interpret-string (buffer-substring-no-properties 1 (point-max))))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun forth-include-file (filename)
  (interactive)
  (forth-interpret-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun forth-include ()
  (forth-include-file (forth-parse-name)))
  
(defun forth-interpret-string (string)
  (let ((saved-input forth-input)
	(saved-index forth-input-index))
    (setq forth-input string)
    (setq forth-input-index 0)
    (unwind-protect
	(forth-interpret)
      (setq forth-input saved-input)
      (setq forth-input-index saved-index))))
    
