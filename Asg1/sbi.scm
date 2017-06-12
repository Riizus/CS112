#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; Author
;;    Riise Kennard
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

;; Finds basename of file
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; Exits program and prints content of list to stderr
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; Incorrect usage
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; Returns contents of input file as a linked list with each line
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; Tail Recursive function to find length of list.
(define (length line)
   (define (length. line count)
     (if (null? line)
         count
         (length. (cdr line) (+ count 1))))

   (length. line 0))

(define prog-len 0)

;;;;;;;;; Hash Tables ;;;;;;;;;

(define *symbol-hash* (make-hash))    ;; Symbol table
(define *label-hash* (make-hash))     ;; Label table
(define *func-hash* (make-hash))      ;; Function table

;; Sets value and key for various Tables
(define (set-symbol! key value)
	(hash-set! *symbol-hash* key value))

(define (set-label! key value)
	(hash-set! *label-hash* key value))

(define (set-func! key value)
	(hash-set! *func-hash* key value))

;; Initialize the symbol table.
(for-each (lambda (pair) (set-symbol! (car pair) (cadr pair)))
    `(  (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (<>      ,(lambda (x y) (not (= x y))))
        (+ ,+) (- ,-) (* ,*) (/ ,/) (abs ,abs) 
        (<= ,<=) (>= ,>=) (= ,=) (> ,>) (tan ,tan)
        (< ,<) (^ ,expt) (atan ,atan) (sin ,sin) (cos ,cos)
        (ceil ,ceiling) (exp ,exp) (floor ,floor)
        (asin ,asin) (acos ,acos) (round ,round)
        (log ,log) (sqrt ,sqrt)))

;;;;;;;;;; Silly Basic Functions ;;;;;;;;;;;

;; Prints out each item in list str
(define (s-print str)
	(map (lambda (x) (display (eval-exp x))) str)
	(newline))

;; Creates and array given by the variable name and value in exp 
(define (s-dim exp)
	(if (pair? exp)
		(let ((array (car exp)))
			(set-symbol! (car array) (make-vector (eval-exp (cadr array)))))
		(die "Invalid Array Declaration :" exp)))

;; Assigns values in the symbol table
(define (s-let value)
	(set-symbol! (car value) (eval-exp (cadr value))))

;; Assigns values in str to input from read, only reads in numbers
(define (s-input str)
	(define (s-input. str count)
		(if (null? (car str)) 
			(set-symbol! 'inputcount count)
			(let ((tok (read)))
				(cond 
					((eof-object? tok)
						(set-symbol! 'inputcount (- 1)))
					(else
						(cond 
							((number? tok) 
								(set! count (+ count 1))
								(set-symbol! (car str) tok) 
								(set-symbol! 'inputcount count))
							(else 
								(printf "Error: Input Token not a number")))
						(when (not (null? (cdr str))) (s-input. (cdr str) count)))))))

	(s-input. str 0))



;; Fill Function table with our functions. 
(for-each (lambda (pair) (hash-set! *func-hash* (car pair) (cadr pair)))
    `(  (print ,s-print) 
   	    (dim   ,s-dim)
        (let   ,s-let)
        (input ,s-input)
        (if    (void))
        (goto  (void))))


;;;;;;;;; Interpreter ;;;;;;;;;

;; Runs through program filling out label table with any labels it finds
(define (make-label program)
	(for-each (lambda (line)
		(cond
			((= (length line) 3)
       			(set-label! (cadr line) (- (car line)1)))
			((= (length line) 2)
				(when (not(pair? (cadr line)))
					(set-label! (cadr line) (- (car line) 1))))))
		program))

;; Evaulates a line of the program given by numbr looks for a function
;; and send it to eval-func
(define (eval-line program numbr)
	(when (< numbr prog-len)
		(let* ((line (list-ref program numbr)) (L (length line)))
			(cond
				((= L 3)
       				(eval-func program (caddr line) numbr))
				((and(= L 2)(not(hash-has-key? *label-hash* (cadr line))))
					(eval-func program (cadr line) numbr))
				(else
					(eval-line program (+ numbr 1)))))))

;; Checks for the function in the function table then evaluates it
;; Send individual expressions to eval-exp and returns to eval-line to continue 
(define (eval-func program inst numbr) ; Execute a function.
	(when (not (hash-has-key? *func-hash* (car inst))) ; Die if function not in table.
		(die '((car inst) " is not a valid instruction.")))
	(cond
        ((eq? (car inst) 'goto)
        	(if(hash-has-key? *label-hash* (cadr inst)) 
        		(eval-line program (hash-ref *label-hash* (cadr inst)))
        		(die '("Invalid Label : " (cadr inst)))))
        ((eq? (car inst) 'if)
        	(if (eval-exp (cadr inst))
				(eval-line program (hash-ref *label-hash* (caddr inst)))
				(eval-line program (+ numbr 1))))
        (else
			((hash-ref *func-hash* (car inst)) (cdr inst))
			(eval-line program (+ numbr 1)))))

;; Evaluates an expression if the operator is in the symbol table,
;; or just returns the value of the input.
(define (eval-exp inst)
	(cond 
		((number? inst) 
			(+ inst 0.0))
		((string? inst)
			inst)
		((hash-has-key? *symbol-hash* inst)
			(hash-ref *symbol-hash* inst))
		((pair? inst)
			(define op (car inst))
			(if (hash-has-key? *symbol-hash* op)
				(let ((real-op (hash-ref *symbol-hash* op)))
					(cond 
						((vector? real-op)
							(when (not(null? (cdr inst)))
								(define index (eval-exp (cadr inst)))
								(if (and( < index (vector-length real-op))(number? index))
									(vector-ref real-op index)
									(die '("Index Error: " op "(" index ")")))))
						((procedure? real-op)
							(apply real-op (map eval-exp (cdr inst))))
						(else
							(die '("Undefined Expression : " inst)))))
				(die '("Undefined Symbol: " op))))
		(else
			(die '("Invalide Instuction" inst)))))

;; Interprets input file.
(define (interpret-program filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "~n")
    (set! prog-len (length program))
   	(make-label program)
    (eval-line program 0))

;; Main function runs the show
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist)) (program (readlist-from-inputfile sbprogfile)))
			(interpret-program sbprogfile program))))

;; Runs main function with command line arguments
(main (vector->list (current-command-line-arguments)))
