;; what follows is a jelly port of the library proposed in srfi 1
;;; most of the present code has been taken from the reference implementation
;;; present in the srfi document
;;;
;;; some parts were rewritten for either clarity, efficiency (jelly has no tco
;;; which the reference implementations uses a lot, being scheme code)
;;; or compatibility reasons (jelly also has no continuations or macros)

;;; 
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

(define set-car! setCar!)
(define set-cdr! setCdr!)

(define pair? cons?)
(define (not-pair? x) (not (pair? x)))

(define (first x)   (nth x 0))
(define (second x)  (nth x 1))
(define (third x)   (nth x 2))
(define (fourt x)   (nth x 3))
(define (fifth x)   (nth x 4))
(define (sixth x)   (nth x 5))
(define (seventh x) (nth x 6))
(define (eight x)   (nth x 7))
(define (ninth x)   (nth x 8))
(define (tenth x)   (nth x 9))

(define (caar   x) (car (car x)))
(define (cadr   x) (car (cdr x)))
(define (cdar   x) (cdr (car x)))
(define (cddr   x) (cdr (cdr x)))

(define (caaar  x) (caar (car x)))
(define (caadr  x) (caar (cdr x)))
(define (cadar  x) (cadr (car x)))
(define (caddr  x) (cadr (cdr x)))
(define (cdaar  x) (cdar (car x)))
(define (cdadr  x) (cdar (cdr x)))
(define (cddar  x) (cddr (car x)))
(define (cdddr  x) (cddr (cdr x)))

(define (caaaar x) (caaar (car x)))
(define (caaadr x) (caaar (cdr x)))
(define (caadar x) (caadr (car x)))
(define (caaddr x) (caadr (cdr x)))
(define (cadaar x) (cadar (car x)))
(define (cadadr x) (cadar (cdr x)))
(define (caddar x) (caddr (car x)))
(define (cadddr x) (caddr (cdr x)))
(define (cdaaar x) (cdaar (car x)))
(define (cdaadr x) (cdaar (cdr x)))
(define (cdadar x) (cdadr (car x)))
(define (cdaddr x) (cdadr (cdr x)))
(define (cddaar x) (cddar (car x)))
(define (cddadr x) (cddar (cdr x)))
(define (cdddar x) (cdddr (car x)))
(define (cddddr x) (cdddr (cdr x)))

;;; Make a list of length LEN.
(define (make-list len &rest maybe-elt)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
  (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		   ((null? (cdr maybe-elt)) (car maybe-elt))
		   (else (error "Too many arguments to MAKE-LIST"
				(cons len maybe-elt))))))
    (do ((i len (- i 1))
	 (ans nil (cons elt ans)))
	((<= i 0) ans))))

;;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
(define (list-tabulate len proc)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len list-tabulate)
  (check-arg procedure? proc list-tabulate)
  (do ((i (- len 1) (- i 1))
       (ans nil (cons (proc i) ans)))
      ((< i 0) ans)))

;;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
;;; (cons* a1) = a1	(cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
;;;
;;; (cons first (unfold not-pair? car cdr rest values))
(define (cons* first &rest rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

(define (list-copy lis)
  (let recur ((lis lis))
    (if (pair? lis)
	(cons (car lis) (recur (cdr lis)))
	lis)))

(define (iota count &rest maybe-start+step)
  (unless (null? (third maybe-start+step))
    (error "iota: too many arguments given"))
  (check-arg integer? count iota)
  (if (< count 0) (error "Negative step count" iota count))
  (let ((start (if (null? (nth maybe-start+step 0))
                   0
                   (nth maybe-start+step 0)))
        (step (if (null? (nth maybe-start+step 1))
                  1
                  (nth maybe-start+step 1))))
    (check-arg number? start iota)
    (check-arg number? step iota)

    (let loop ((n 0) (r nil))
      (if (= n count)
	  (reverse r)
	  (loop (+ 1 n)
		(cons (+ start (* n step)) r))))))

(define (circular-list val1 &rest vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))

;;; <proper-list> ::= ()			; Empty proper list
;;;		  |   (cons <x> <proper-list>)	; Proper-list pair
;;; Note that this definition rules out circular lists -- and this
;;; function is required to detect this case and return false.
;;; credo sta funzione usi la tartaruga/lepre di hoare, credo
(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))


;;; A dotted list is a finite list (possibly of length 0) terminated
;;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;;; is a dotted list of length 0.
;;;
;;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
;;;               |   (cons <x> <dotted-list>)	; Proper-list pair
(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

;;; This is a legal definition which is fast and sloppy:
;;;     (define null-list? not-pair?)
;;; but we'll provide a more careful one:
(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "null-list?: argument out of domain" l))))

(define (list= = &rest lists)
  (or (null? lists) ; special case

      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
		  (lp1 list-b others)
		  (let lp2 ((pair-a list-a) (pair-b list-b))
		    (if (null-list? pair-a)
			(and (null-list? pair-b)
			     (lp1 list-b others))
			(and (not (null-list? pair-b))
			     (= (car pair-a) (car pair-b))
			     (lp2 (cdr pair-a) (cdr pair-b)))))))))))

(define (length+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))

(define (zip &rest lsts) (map list lsts))


(define (take lis k)
  (check-arg integer? k take)
  (let recur ((lis lis) (k k))
    (if (zero? k) nil
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define (drop lis k)
  (check-arg integer? k drop)
  (let iter ((lis lis) (k k))
    (if (zero? k) lis (iter (cdr lis) (- k 1)))))

(define (take! lis k)
  (check-arg integer? k take!)
  (if (zero? k) nil
      (begin (set-cdr! (drop lis (- k 1)) nil)
	     lis)))


;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.
(define (take-right lis k)
  (check-arg integer? k take-right)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (check-arg integer? k drop-right)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	nil)))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().
(define (drop-right! lis k)
  (check-arg integer? k drop-right!)
  (let ((lead (drop lis k)))
    (if (pair? lead)

	(let lp ((lag lis)  (lead (cdr lead)))	; Standard case
	  (if (pair? lead)
	      (lp (cdr lag) (cdr lead))
	      (begin (set-cdr! lag nil)
		     lis)))
	nil)))	; Special case dropping everything -- no cons to side-effect.

(define (split-at x k)
  (check-arg integer? k split-at)
  (let recur ((lis x) (k k))
    (if (zero? k) (values nil lis)
	(receive (prefix suffix) (recur (cdr lis) (- k 1))
	  (values (cons (car lis) prefix) suffix)))))

(define (split-at! x k)
  (check-arg integer? k split-at!)
  (if (zero? k) (values nil x)
      (let* ((prev (drop x (- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev nil)
	(values x suffix))))

(define (last lis) (car (last-pair lis)))

(define (unzip1 lis) (map car lis))

(define (unzip2 lis) (values (map car lis)
                             (map cadr lis)))

(define (unzip3 lis) (values (map car lis)
                             (map cadr lis)
                             (map caddr lis)))

(define (unzip4 lis) (values (map first lis)
                             (map second lis)
                             (map third lis)
                             (map fourth lis)))

(define (unzip5 lis) (values (map first lis)
                             (map second lis)
                             (map third lis)
                             (map fourth lis)
                             (map fifth lis)))

(define (append! &rest lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev nil))
    (if (not (pair? lists)) prev
	(let ((first (car lists))
	      (rest (cdr lists)))
	  (if (not (pair? first)) (lp rest first)

	      ;; Now, do the splicing.
	      (let lp2 ((tail-cons (last-pair first))
			(rest rest))
		(if (pair? rest)
		    (let ((next (car rest))
			  (rest (cdr rest)))
		      (set-cdr! tail-cons next)
		      (lp2 (if (pair? next) (last-pair next) tail-cons)
			   rest))
		    first)))))))


(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(let ((next-rev (cdr rev-head)))
	  (set-cdr! rev-head tail)
	  (lp next-rev rev-head)))))


(define (concatenate  lists) (reduce-right append  nil lists))
(define (concatenate! lists) (reduce-right append! nil lists))

;;; Return (map cdr lists).
;;; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs lsts)
  (if (any (map null? lsts))
      nil
      (map cdr lsts)))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].
(define (%cars+cdrs lists)
  (if (any (map null? lsts))
      (list nil nil)
      (values (map car lists)
              (map cdr lists))))

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list. What a hack.
(define (%cars+cdrs+ lists cars-final)
  (if (any (map null? lsts))
      (list nil nil)
      (values (append (map car lists) (list cars-final))
              (map cdr lists))))


;;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs lists)
  (values (map car lists)
          (map cdr lists)))

(define (count pred &rest lsts)
  (length (filter id (apply map (cons pred lsts)))))


(define (unfold-right p f g seed &rest maybe-tail)
  (check-arg procedure? p unfold-right)
  (check-arg procedure? f unfold-right)
  (check-arg procedure? g unfold-right)
  (let lp ((seed seed) (ans (optional maybe-tail nil)))
    (if (p seed) ans
	(lp (g seed)
	    (cons (f seed) ans)))))

(define (unfold p f g seed &rest maybe-tail-gen)
  (check-arg procedure? p unfold)
  (check-arg procedure? f unfold)
  (check-arg procedure? g unfold)
  (if (pair? maybe-tail-gen)

      (let ((tail-gen (car maybe-tail-gen)))
	(if (pair? (cdr maybe-tail-gen))
	    (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)

	    (let recur ((seed seed))
	      (if (p seed) (tail-gen seed)
		  (cons (f seed) (recur (g seed)))))))

      (let recur ((seed seed))
	(if (p seed) nil
	    (cons (f seed) (recur (g seed)))))))


(define (pair-fold-right f zero lis1 &rest lists)
  (check-arg procedure? f pair-fold-right)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) zero
	      (apply f (append! lists (list (recur cdrs)))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) zero (f lis (recur (cdr lis)))))))

(define (pair-fold f zero lis1 &rest lists)
  (check-arg procedure? f pair-fold)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))	; N-ary case
	(let ((tails (%cdrs lists)))
	  (if (null? tails) ans
	      (lp tails (apply f (append! lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
	(if (null-list? lis) ans
	    (let ((tail (cdr lis)))		; Grab the cdr now,
	      (lp tail (f lis ans)))))))	; in case F SET-CDR!s LIS.

(define (append-map fn &rest lists)
  (apply append (apply map (cons fn lists))))

(define (append-map! fn &rest lists)
  (apply append! (apply map (cons fn lists))))

(define flatmap append-map)
(define flatmap! append-map!)

(define map-in-order map)


(define (pair-for-each proc lis1 &rest lists)
  (check-arg procedure? proc pair-for-each)
  (if (pair? lists)

      (let lp ((lists (cons lis1 lists)))
	(let ((tails (%cdrs lists)))
	  (if (pair? tails)
	      (begin (apply proc lists)
		     (lp tails)))))

      ;; Fast path.
      (let lp ((lis lis1))
	(if (not (null-list? lis))
	    (let ((tail (cdr lis)))	; Grab the cdr now,
	      (proc lis)		; in case PROC SET-CDR!s LIS.
	      (lp tail))))))

(define (filter-map fn &rest lists)
  (filter id (apply map (cons fn lists))))

;; to be tested
(define (map! fn lst &rest lsts)
  (do ((l lst (cdr l))
       (ll lsts (map cdr ll)))
      ((null? l) lst)
    (set-car! l (apply fn (cons (car l) (map car ll))))))

