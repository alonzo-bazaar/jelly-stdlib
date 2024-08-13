;;; what follows is a jelly port of the library proposed in srfi 1
;;; most of the present code has been taken from the reference implementation
;;; present in the srfi document
;;; https://srfi.schemers.org/srfi-1/srfi-1-reference.scm
;;;
;;; some parts were rewritten for either clarity, efficiency (jelly has no tco
;;; which the reference implementations uses a lot, being scheme code)
;;; or compatibility reasons (jelly also has no continuations or macros)

;;; copyright for the reference implementation:
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

(define-library (list library)
  (import (base utils))
  (export xcons tree-copy make-list list-tabulate cons* list-copy
          proper-list? circular-list? dotted-list? not-pair? null-list? list=
          circular-list length+
          iota
          car+cdr
          take       drop
          take-right drop-right
          take!      drop-right!
          split-at   split-at!
          last last-pair
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count
          append! append-reverse append-reverse! concatenate concatenate!

          unfold       fold       pair-fold       reduce
          unfold-right fold-right pair-fold-right reduce-right

          append-map append-map! map! pair-for-each filter-map map-in-order
          filter  partition  remove
          filter! partition! remove!
          find find-tail any every list-index
          take-while drop-while take-while!
          span break span! break!
          delete delete!
          alist-cons alist-copy
          delete-duplicates delete-duplicates!
          alist-delete alist-delete!
          reverse!
          lset<= lset= lset-adjoin
          lset-union  lset-intersection  lset-difference  lset-xor  lset-diff+intersection
          lset-union! lset-intersection! lset-difference! lset-xor! lset-diff+intersection!)

  (begin
    (define (check-arg pred val caller)
      (unless (pred val)
        (error "Bad argument" val pred caller)))

    (define (last-pair lis)
      (check-arg pair? lis last-pair)
      (let lp ((lis lis))
        (let ((tail (cdr lis)))
          (if (pair? tail) (lp tail) lis))))

    ;; Make a list of length LEN.
    (define (make-list len &rest maybe-elt)
      (check-arg (lambda (n) (and (integer? n) (>= n 0))) len make-list)
      (let ((elt (cond ((null? maybe-elt) #f) ; Default value
		       ((null? (cdr maybe-elt)) (car maybe-elt))
		       (else (error "Too many arguments to MAKE-LIST"
				    (cons len maybe-elt))))))
        (do ((i len (- i 1))
	     (ans nil (cons elt ans)))
	    ((<= i 0) ans))))

    ;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
    (define (list-tabulate len proc)
      (check-arg (lambda (n) (and (integer? n) (>= n 0))) len list-tabulate)
      (check-arg procedure? proc list-tabulate)
      (do ((i (- len 1) (- i 1))
           (ans nil (cons (proc i) ans)))
          ((< i 0) ans)))

    ;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
    ;; (cons* a1) = a1	(cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
    ;;
    ;; (cons first (unfold not-pair? car cdr rest values))
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

    ;; <proper-list> ::= ()			; Empty proper list
    ;;		  |   (cons <x> <proper-list>)	; Proper-list pair
    ;; Note that this definition rules out circular lists -- and this
    ;; function is required to detect this case and return false.
    ;; credo sta funzione usi la tartaruga/lepre di hoare, credo
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


    ;; A dotted list is a finite list (possibly of length 0) terminated
    ;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
    ;; is a dotted list of length 0.
    ;;
    ;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
    ;;               |   (cons <x> <dotted-list>)	; Proper-list pair
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

    ;; This is a legal definition which is fast and sloppy:
    ;;     (define null-list? not-pair?)
    ;; but we'll provide a more careful one:
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

    ;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
    ;; off by K, then chasing down the list until the lead pointer falls off
    ;; the end.
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

    ;; In this function, LEAD is actually K+1 ahead of LAG. This lets
    ;; us stop LAG one step early, in time to smash its cdr to ().
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

    (define (filter pred lst)
      (let ((acc nil))
        (dolistFn 
         (lambda (x) (if (pred x) (set! acc (cons x acc))))
         lst)
        (reverse acc)))

    ;; per adesso
    (define filter! filter)

    (define (concatenate  lists) (reduce-right append  nil lists))
    (define (concatenate! lists) (reduce-right append! nil lists))

    ;; Return (map cdr lists).
    ;; However, if any element of LISTS is empty, just abort and return '().
    (define (%cdrs lsts)
      (if (any (map null? lsts))
          nil
          (map cdr lsts)))

    (define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
      (let recur ((lists lists))
        (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

    ;; LISTS is a (not very long) non-empty list of lists.
    ;; Return two lists: the cars & the cdrs of the lists.
    ;; However, if any of the lists is empty, just abort and return [() ()].
    (define (%cars+cdrs lists)
      (if (any (map null? lsts))
          (list nil nil)
          (values (map car lists)
                  (map cdr lists))))

    ;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
    ;; cars list. What a hack.
    (define (%cars+cdrs+ lists cars-final)
      (if (any (map null? lsts))
          (list nil nil)
          (values (append (map car lists) (list cars-final))
                  (map cdr lists))))

    ;; Like %CARS+CDRS, but blow up if any list is empty.
    (define (%cars+cdrs/no-test lists)
      (values (map car lists)
              (map cdr lists)))

    (define (count pred &rest lsts)
      (length (filter id (apply map (cons pred lsts)))))

;;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;;; These cannot meaningfully be n-ary.
    (define (reduce-left fn initial lst)
      (do ((val initial (fn (car lst) val))
           (lst lst (cdr lst)))
          ((null? lst) val)))

    (define (reduce-right fn initial lst)
      (let ((lst (reverse lst)))
        (do ((val initial (fn (car lst) val))
             (lst lst (cdr lst)))
            ((null? lst) val))))

    (define reduce reduce-left)
    
    (define (fold-left fn initial &rest lsts)
      (do ((val initial (apply fn (append (cars lsts) (list val))))
           (lsts lsts (cdrs lsts)))
          ((any? null? lsts) val)))

    (define (fold-right kons knil lis1 &rest lists)
      (define (%cdrs lsts)
        (if (any1 null? lsts)
            nil
            (map cdr lsts)))
      (check-arg procedure? kons fold-right)
      (if (pair? lists)
          (let recur ((lists (cons lis1 lists)))		; N-ary case
	    (let ((cdrs (%cdrs lists)))
	      (if (null? cdrs) knil
	          (apply kons (%cars+ lists (recur cdrs))))))

          (let recur ((lis lis1))				; Fast path
	    (if (null-list? lis) knil
	        (let ((head (car lis)))
	          (kons head (recur (cdr lis))))))))

    (define fold fold-left)

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

    (define (map1 fn lst)
      (do ((iter lst (cdr iter))
           (acc nil (cons (fn (car iter)) acc)))
          ((null? iter) (reverse acc))))

    (define (map fn &rest lsts)
      (let ((acc nil))
        (do ((lsts lsts (map1 cdr lsts)))
            ((all? null? lsts) (reverse acc))
          (set! acc (cons (apply fn (map1 car lsts)) acc)))))

    ;; to be tested
    (define (map! fn lst &rest lsts)
      (do ((l lst (cdr l))
           (ll lsts (map cdr ll)))
          ((null? l) lst)
        (set-car! l (apply fn (cons (car l) (map car ll))))))

    (define (find pred lst)
      (do ((lst lst (cdr lst)))
          ((or (null? lst) (pred (car lst))) (unless (null? lst) (car lst)))))

    (define (find-tail pred lst)
      (do ((lst lst (cdr lst)))
          ((or (null? lst) (pred (car lst))) (unless (null? lst) lst))))

    (define (take-while pred lst)
      (let ((acc nil))
        (do ((lst lst (cdr lst)))
            ((or (null? lst)
                 (not (pred (car lst))))
             (reverse acc))
          (set! acc (cons (car lst) acc)))))

    (define (take-while! pred lst)
      (if (pred (car lst))
          (do ((lst lst (cdr lst)))
              ((or (null? lst)
                   (not (pred (cadr lst))))
               (begin
                 (unless (null? lst)
                   (set-cdr! lst nil))
                 lst)))
          nil))

    ;; TODO ain't tested shit beyond here
    (define (span pred lis)
      (check-arg procedure? pred span)
      (values (take-while pred clist)
              (drop-while pred clist)))

    (define (span! pred lis)
      (check-arg procedure? pred span!)
      (if (or (null-list? lis) (not (pred (car lis)))) (values nil lis)
          (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
		          (if (null-list? rest) rest
			      (let ((x (car rest)))
			        (if (pred x) (lp rest (cdr rest))
				    (begin (set-cdr! prev nil)
				           rest)))))))
	    (values lis suffix))))


    (define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))
    (define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))

    (define (any1 pred lst)
      (do ((lst lst (cdr lst)))
          ((or (null? lst)
               (pred (car lst)))
           (not (null? lst)))))

    (define (every1 pred lst)
      ;; de morgan's
      (not (any1 (complement pred) lst)))

    (define (anyzip pred lsts)
      (do ((lsts lsts (map1 cdr lsts)))
          ((or (any1 null? lsts)
               (apply pred (map1 car lsts)))
           (not (any1 null? lsts)))))

    (define (any pred lst &rest lsts)
      (if (null? lsts)
          (any1 pred lst)
          (anyzip pred (cons lst lsts))))

    (define (everyzip pred lsts)
      (do ((lsts lsts (map1 cdr lsts)))
          ((or (any1 null? lsts)
               (not (apply pred (map1 car lsts))))
           (any1 null? lsts))))

    (define (every pred lst &rest lsts)
      (if (null? lsts)
          (every1 pred lst)
          (everyzip pred (cons lst lsts))))

    (define (indexzip pred lsts)
      (do ((lsts lsts (map1 cdr lsts))
           (index 0 (+ 1 index)))
          ((or (any1 null? lsts)
               (apply pred (map1 car lsts)))
           (unless (any1 null? lsts)
             index))))

    (define (index1 pred lst)
      (do ((lst lst (cdr lst))
           (index 0 (+ 1 index)))
          ((or (null? lst)
               (pred (car lst)))
           (when (not (null? lst))
             index))))

    (define (list-index pred lst &rest lsts)
      (if (null? lsts)
          (index1 pred lst)
          (indexzip pred (cons lst lsts))))

    (define (reverse lst)
      (do ((lst lst (cdr lst))
           (res nil (cons (car lst) res)))
          ((null? lst) res)))

    ;; TODO update dis shite
    (define reverse! reverse)

    ;; deletey
    (define (delete x lis &rest maybe-=)
      (let ((= (optional maybe-= equal?)))
        (filter (lambda (y) (not (= x y))) lis)))

    (define (delete! x lis &rest maybe-=)
      (let ((= (optional maybe-= equal?)))
        (filter! (lambda (y) (not (= x y))) lis)))

    ;; Lists-as-sets
;;;;;;;;;;;;;;;;

    ;; This is carefully tuned code; do not modify casually.
    ;; - It is careful to share storage when possible;
    ;; - Side-effecting code tries not to perform redundant writes.
    ;; - It tries to avoid linear-time scans in special cases where constant-time
    ;;   computations can be performed.
    ;; - It relies on similar properties from the other list-lib procs it calls.
    ;;   For example, it uses the fact that the implementations of MEMBER and
    ;;   FILTER in this source code share longest common tails between args
    ;;   and results to get structure sharing in the lset procedures.

    (define (%lset2<= = lis1 lis2) (every (lambda (x) (member x lis2 =)) lis1))

    (define (lset<= = &rest lists)
      (check-arg procedure? = lset<=)
      (or (not (pair? lists)) ; 0-ary case
          (let lp ((s1 (car lists)) (rest (cdr lists)))
	    (or (not (pair? rest))
	        (let ((s2 (car rest))  (rest (cdr rest)))
	          (and (or (eq? s2 s1)	; Fast path
		           (%lset2<= = s1 s2)) ; Real test
		       (lp s2 rest)))))))

    (define (lset= = &rest lists)
      (define (flip proc) (lambda (x y) (proc y x)))
      (check-arg procedure? = lset=)
      (or (not (pair? lists)) ; 0-ary case
          (let lp ((s1 (car lists)) (rest (cdr lists)))
            (or (not (pair? rest))
                (let ((s2   (car rest))
                      (rest (cdr rest)))
                  (and (or (eq? s1 s2)            ; Fast path
                           (and (%lset2<= = s1 s2) ; Real test
                                (%lset2<= (flip =) s2 s1)))
                       (lp s2 rest)))))))

    (define (lset-adjoin = lis &rest elts)
      (check-arg procedure? = lset-adjoin)
      (fold (lambda (elt ans)
              (if (member elt ans =)
                  ans
                  (cons elt ans)))
	    lis elts))

    (define (lset-union = &rest lists)
      (check-arg procedure? = lset-union)
      (reduce (lambda (lis ans)		; Compute ANS + LIS.
	        (cond ((null? lis) ans)	; Don't copy any lists
		      ((null? ans) lis)	; if we don't have to.
		      ((eq? lis ans) ans)
		      (else
		       (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
					           ans
					           (cons elt ans)))
			     ans lis))))
	      nil lists))

    (define (lset-union! = &rest lists)
      (check-arg procedure? = lset-union!)
      (reduce (lambda (lis ans)		; Splice new elts of LIS onto the front of ANS.
	        (cond ((null? lis) ans)	; Don't copy any lists
		      ((null? ans) lis)	; if we don't have to.
		      ((eq? lis ans) ans)
		      (else
		       (pair-fold (lambda (pair ans)
				    (let ((elt (car pair)))
				      (if (any (lambda (x) (= x elt)) ans)
				          ans
				          (begin (set-cdr! pair ans) pair))))
			          ans lis))))
	      nil lists))


    (define (lset-intersection = lis1 &rest lists)
      (check-arg procedure? = lset-intersection)
      (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
        (cond ((any null-list? lists) nil)		; Short cut
	      ((null? lists)          lis1)		; Short cut
	      (else (filter (lambda (x)
			      (every (lambda (lis) (member x lis =)) lists))
			    lis1)))))

    (define (lset-intersection! = lis1 @rest lists)
      (check-arg procedure? = lset-intersection!)
      (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
        (cond ((any null-list? lists) nil)		; Short cut
	      ((null? lists)          lis1)		; Short cut
	      (else (filter! (lambda (x)
			       (every (lambda (lis) (member x lis =)) lists))
			     lis1)))))

    (define (lset-difference = lis1 &rest lists)
      (check-arg procedure? = lset-difference)
      (let ((lists (filter pair? lists)))	; Throw out empty lists.
        (cond ((null? lists)     lis1)	; Short cut
	      ((memq lis1 lists) nil)	; Short cut
	      (else (filter (lambda (x)
			      (every (lambda (lis) (not (member x lis =)))
				     lists))
			    lis1)))))

    (define (lset-difference! = lis1 &rest lists)
      (check-arg procedure? = lset-difference!)
      (let ((lists (filter pair? lists)))	; Throw out empty lists.
        (cond ((null? lists)     lis1)	; Short cut
	      ((memq lis1 lists) nil)	; Short cut
	      (else (filter! (lambda (x)
			       (every (lambda (lis) (not (member x lis =)))
				      lists))
			     lis1)))))

    (define (lset-xor1 = lst1 lst2)
      (lset-union (lset-difference lst1 lst2)
                  (lset-difference lst2 lst1)))

    (define (lset-xor = lst &rest lsts)
      (cond ((null? lsts) lst)
            ((null? (cdr lsts)) (lst-xor1 lst (car lsts)))
            (#t (lset-xor1 (car lst) (apply lset-xor (cons* = lst lsts))))))

    ;; TODO
    (define lset-xor! lset-xor)

    (define (lset-diff+intersection = lis1 &rest lists)
      (check-arg procedure? = lset-diff+intersection)
      (cond ((every null-list? lists) (values lis1 nil))	; Short cut
	    ((memq lis1 lists)        (values nil lis1))	; Short cut
	    (else (partition (lambda (elt)
			       (not (any (lambda (lis) (member elt lis =))
				         lists)))
			     lis1))))

    (define (lset-diff+intersection! = lis1 &rest lists)
      (check-arg procedure? = lset-diff+intersection!)
      (cond ((every null-list? lists) (values lis1 nil))	; Short cut
	    ((memq lis1 lists)        (values nil lis1))	; Short cut
	    (else (partition! (lambda (elt)
			        (not (any (lambda (lis) (member elt lis =))
				          lists)))
			      lis1))))))
