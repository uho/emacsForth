
(defun send-command (cmd)
  (term-send-string (get-buffer-process (get-buffer "/dev/tty.usbserial-DA00875J")) (concat cmd "\r")))


(read-string "> ")


(defvar DP)

(defvar forth-dictionary
  '((DROP . 10)
    (SWAP . 20)
    (ROT  . 30)
    (IF   . 40)))

(defun forth-find-word (name)
  (assoc name forth-dictionary))


(defun forth-allocate (n)
  (setq DP (+ DP n)))

(defun forth-create-header (name)
  (setq forth-dictionary
	(concat
	 (cons name DP)
	 forth-dictionary)))

;; read current line from buffer
(thing-at-point 'line t)



(defvar forth-source)


(defun accept (prompt)
  (interactive)
  (goto-char (point-max))
  (insert prompt)
  (let ((oldmap (current-local-map))
	(map (make-keymap)))
    (define-key map "a" (lambda ()
			  (interactive)
				 (setq forth-source
				       (substring (thing-at-point 'line t) (length prompt)))
				 (use-local-map oldmap)))
    (use-local-map map)))
    


;; Byte-Protocol
;;
;;    $00  NUL start - init and reset
;;
;;    $01  SOH header ( <name> xt -- )    \ create new symbol table entry
;;         $01 --->   
;;         xt0 ---> 
;;         xt1 ---> 
;;         xt2 --->
;;     $03 ETX ---> 
;;             <--- $06 ACK
;;
;;    $02 STX  include ( <name> -- )    \ start parsing in file <name>
;;         $02 --->
;;             <--- $06 ACK
;;
;;    $03 ETX  \\ ( -- )                \  skip rest of input  
;;         $03 --->
;;             <--- $06 ACK
;;
;;    $05 ENQ query ( -- )      get one line of user input
;;
;;            immediate, smudge, hide, reveal, headerflags
;;            
;;            key/key?
;;
;;    $11  DC1 parse-name ( <name> -- )   \ set token
;;         $11 --->
;;             <--- $06 ACK
;;
;;    $12  DC2 find ( -- xt 1 | xt -1 | 0 ) \ uses token
;;         $12 --->
;;             <--- 0
;;
;;         $12 --->
;;             <--- -1 | 1 
;;             <--- xt0
;;             <--- xt1
;;             <--- xt2
;;             ...
;;             <--- $06 ACK
;;
;;    $13  DC3 number ( -- n -1 | 0 )     \ uses token convert to number
;;         $13 --->
;;             <--- 0
;;             <--- $06 ACK
;;
;;         $13 --->
;;             <--- -1
;;             <--- n0
;;             <--- n1
;;             <--- n2
;;             ...
;;             <--- $06 ACK
;;
;;    $14  DC4 parse ( c -- )             \ parse for delimiter transmit skipped characters
;;         $14 --->
;;           c --->
;;             <--- c0
;;             <--- c1
;;             <--- c2
;;             ...
;;             <--- cn
;;             <--- $06 ACK
;;
;;    $1A  SUB skip ( c -- )             \ just skip character until next c
;;         $1A --->
;;           c --->
;;             <--- $06 ACK
;;
;;        
;;    
;;    all other characters print
;;
;; parameters are transitted with bit $80 set
;; cells are sent most significant byte first
;; so: control character less $80, parameter greater equal $80



;; : x  ( -- )  ." hallo" ;
;; 
;; $11 parse-name --->           \ :
;;                <--- $06 ACK
;; $12 find       --->           \ :
;;                <--- 1
;;                <--- xt0
;;                <--- xt1
;;                <--- xt2
;;                <--- $06 ACK   ; xt of :
;; ##### execute : ####
;; $01 header     --->           \ create header
;;            xt0 --->
;;            xt1 --->
;;            xt2 --->
;; $03 ETX        --->
;;                <--- $06 ACK
;; $11 parse-name --->           \ (
;;                <--- $06 ACK
;; $12 find       --->           \ :
;;                <--- -1
;;                <--- xt0
;;                <--- xt1
;;                <--- xt2
;;                <--- $06 ACK   ; xt of (
;; ##### execute ( ####
;; $1A skip       --->
;;     41         --->           \ )
;;                <--- $06 ACK
;;
;; $11 parse-name --->           \ ."
;;                <--- $06 ACK
;; $12 find       --->           \ :
;;                <--- -1
;;                <--- xt0
;;                <--- xt1
;;                <--- xt2
;;                <--- $06 ACK   ; xt of ."
;; ##### execute ." ####
;; $14 parse      --->
;;     34         --->           \ "
;;                <--- 'h'       \ compile directly in target memory
;;                <--- 'a'
;;                <--- 'l'
;;                <--- 'l'
;;                <--- 'o'
;;                <--- $06 ACK
;;
;; $11 parse-name --->           \ ;
;; $12 find       --->           \ ;
;;                <--- -1
;;                <--- xt0
;;                <--- xt1
;;                <--- xt2
;;                <--- $06 ACK   ; xt of ;
;; ##### execute ; ####


