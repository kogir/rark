#lang planet kogir/rark

;; TODO: Kill this before release
(displayln "loading rark.rkt...")

(provide (all-defined-out))

;; I implemented square brackets in the reader as follows:
;; [stuff ...] => (square-brackets stuff ...)
;; This idea was stolen from multiple of the arc spinoffs on the arc forum
;(mac square-brackets (body ...)

;; Let's make ~ ssyntax extensible:
;(mac complement-ssyntax (body ...)

;; do - mapped to racket (begin ...) (performance)
;; safeset - mostly integrated into def and mac. Now more of a noisy =
;; def - sucked into core (required)

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

;; no - sucked into core (performance)

(def acons (x) (is (type x) 'cons))

(def atom (x) (no (acons x)))

(def copylist (xs)
  (if (no xs) 
      nil 
      (cons (car xs) (copylist (cdr xs)))))

;; TODO: map this to Racket's (list)?
;; No more need to copy since rest args are proper lists
(def list args args)

(def idfn (x) x)

;; map1 - sucked into core (performance)

(def pair (xs (o f list))
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

;; mac - sucked into core (required)
;; and - Sucked into core (performance, nil handling)

(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key) (cadr (assoc key al)))

;; with - sucked into core (performance)
;; let - sucked into core (performance)
;; withs - sucked into core (performance)

(def join args
  (if (no args)
      nil
      (let a (car args) 
        (if (no a) 
            (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))

;; rfn - no longer needed, since afn is now hygenic :)
;; afn - sucked into core (performance + hygene)

;; reader expands x:y:z into (compose x y z), ~x into (no x)
;; compose - Sucked into core (performance + ssyntax support)
;; complement - sucked into core (performance)

(def rev (xs) 
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def isnt (x y) (no (is x y)))

;; w/uniq - killed for now - see how far hygiene can get us

;; or - sucked into core (performance, nil handling)

(def alist (x) (or (no x) (is (type x) 'cons)))

(mac in (x choice ...)
  (or (is x choice) ...))

(def iso (x y)
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))

(mac when (test body ...)
  (if test (do body ...)))

(mac unless (test body ...)
  (if (no test) (do body ...)))

(mac while (test body ...)
  ((afn () (when test body ... (self)))))

(def empty (seq) 
  (or (no seq) 
      (and (or (is (type seq) 'string) (is (type seq) 'table))
           (is (len seq) 0))))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))

(def testify (x)
  (if (isa x 'fn) x [is _ x]))

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq) 
  (~some (complement (testify test)) seq))
       
(def mem (test seq)
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

(def isa (x y) (is (type x) y))

;; TODO: Add hack to allow (mappend (table) '(1 2 3 4))
(def map (f . seqs)
  (if (some [isa _ 'string] seqs) 
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs)) 
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)  
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply + nil (apply map f args)))

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

(def caris (x val) 
  (and (acons x) (is (car x) val)))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic (body ...)
  (atomic-invoke (fn () body ...)))

(mac atlet (args ...)
  (atomic (let args ...)))
  
(mac atwith (args ...)
  (atomic (with args ...)))

(mac atwiths (args ...)
  (atomic (withs args ...)))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

;; do1 - mapped to racket begin0 (performance)

;; TODO: Find a way to push this back into arc.arc
;; expand=, expand=list, and = sucked into core (performance, required)

;; TODO: See if I can eliminate sets here using a different construction
(mac loop (start test update body ...)
  (do start
    ((afn ()
       (if test
           (do body ...
             update
             (self)))))))

;; TODO: Consider using Racket's for construct with in-range
(mac for (v init max body ...)
  (with (v nil i init stop (+ max 1))
    (loop (assign v i) (< v stop) (assign v (+ v 1))
          body ...)))

(mac down (v init min body ...)
  (with (v nil i init stop (- min 1))
    (loop (assign v i) (> v stop) (assign v (- v 1))
          body ...)))

(mac repeat (n body ...)
  (for reps 1 n body ...))

;; TODO: Use Racket's for-each to skip creating the list
;; TODO: Special case strings
(mac each (var expr body ...)
  (with (seq expr
         mapfn (fn (var) body ...))
    (if (alist seq)
         (each1 mapfn seq)
        (isa seq 'table)
         ; TODO: You can do better
         (maptable (fn var body ...) seq)
        (for iter 0 (- (len seq) 1)
          (mapfn (seq iter)))))
  (void))

(def cut (seq start (o end))
  (let end (if (no end)   (len seq)
               (< end 0)  (+ (len seq) end) 
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))

(mac whilet (var test body ...)
  ((afn (var)
     (when var
       body ...
       (self test)))
   test))

(def last (xs)
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

(def keep (test seq) 
  (rem (complement (testify test)) seq))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))

;; caselet - sucked into core (argument pairing, performance)
;; case - sucked into core (argument pairing, performance)

(mac push (x place)
  (atomic
   (= place (cons x place))
   place))

(mac swap (place1 place2)
  (atwiths (p1 place1
            p2 place2)
    (= place1 p2)
    (= place2 p1)))

(mac rotate (a c ...)
  (shift (c ... a) (a c ...)))

(mac shift ((from0 from ...) (to0 to ...))
  (let tmp from0
    (= to from) ...
    (= to0 tmp)))

(mac pop (place)
  (atwiths (val place)
    (do1 (car val)
         (= place (cdr val)))))

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place args ...)
  (atomic
   (= place (adjoin x place args ...))))

(mac pull (test place)
  (atomic
   (= place (rem test place))))

(mac togglemem (x place args ...)
  (atwiths (val place
            tmp x)
    (= place (if (mem tmp val)
                 (rem tmp val)
                 (adjoin tmp val args ...)))))

;; TODO: move atomic within = and lost it here
(mac ++ (place (o i 1))
  (if (isa 'place 'sym)
      (= place (+ place i))
      (atomic
       (= place (+ place i)))))

 ;; TODO: Ditto 
(mac -- (place (o i 1))
  (if (isa 'place 'sym)
      (= place (- place i))
      (atomic
       (= place (- place i)))))

;; TODO: Check this with PG
(mac zap (op place args ...)
  (atomic
   (= place (op place args ...))))

(def pr args
  (each1 disp args)
  (car args))

(def prt args
  (each1 [if _ (disp _)] args)
  (car args))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(mac wipe (arg ...)
  (do (= arg nil) ...))

(mac set (arg ...)
  (do (= arg t) ...))

(mac iflet (var expr then rest ...)
  (let temp expr
    (if temp
        (let var temp then)
        rest ...)))

(mac whenlet (var expr body ...)
  (iflet var expr (do body ...)))

;; aif - sucked into core (hygiene)

(mac awhen (expr body ...)
  (aif expr (do body ...)))

;; aand - sucked into core (hygiene)

(mac accum (accfn body ...)
  (withs (acc nil accfn [push _ acc])
    body ...
    (rev acc)))

(mac drain (expr (o eof nil))
  (with (acc nil done nil)
    (while (no done)
      (let res expr
        (if (is res eof)
            (= done t)
            (push res acc))))
    (rev acc)))

(mac whiler (var expr endval body ...)
  (withs (var nil test (testify endval))
    (while (no (test (= var expr)))
           body ...)))

;; macex - No longer needed, Racket does the expansion for us

(def consif (x y) (if x (cons x y) y))

(def string args
  (apply + "" (map [coerce _ 'string] args)))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (let temp x
    (if (test temp) temp alt)))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)   
                nil
               (f (car seq)) 
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq) 
         start)
        (recstring [if (f (seq _)) _] seq start))))

;; even - Sucked into core (performance)

;; odd - Sucked into core (performance)

(mac after (x ys ...)
  (protect (fn () x) (fn () ys ...)))
  
(mac io-expander (f var name body ...)
  (let var (f name)
    (after (do body ...) (close var))))

(mac w/infile (var name body ...)
  (io-expander infile var name body ...))

(mac w/outfile (var name body ...)
  (io-expander outfile var name body ...))

(mac w/instring (var str body ...)
  (io-expander instring var str body ...))

(mac w/socket (var port body ...)
  (io-expander open-socket var port body ...))

(mac w/outstring (var body ...)
  (let var (outstring) body ...))

(mac w/appendfile (var name body ...)
  (let var (outfile name 'append)
    (after (do body ...) (close var))))

(mac w/stdout (str body ...)
  (call-w/stdout str (fn () body ...)))
  
(mac w/stdin (str body ...)
  (call-w/stdin str (fn () body ...)))

(mac tostring (body ...)
  (w/outstring temp
    (w/stdout temp body ...)
    (inside temp)))

(mac fromstring (str body ...)
  (w/instring temp str
    (w/stdin temp body ...)))

(def readstring1 (s (o eof nil)) (w/instring i s (read i eof)))

(def read ((o x (stdin)) (o eof nil))
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src (o eof nil))
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(let counter 0
  (def writefile (val file)
    (let tmpfile (+ file ".tmp." (atomic (++ counter)))
      (w/outfile o tmpfile (write val o))
      (mvfile tmpfile file))
    val))

(def sym (x) (coerce x 'sym))

(def int (x (o b 10)) (coerce x 'int b))

;; TODO: Check performance - using eval is a hack
(mac rand-choice (expr ...)
  (let choices (list 'expr ...)
    (eval (choices (rand (len choices))))))

(mac n-of (n expr)
  (let acc nil
    (repeat n (push expr acc))
    (rev acc)))

;; TODO: Use OpenSSL for this to make it cross platform
(let str (infile "/dev/urandom")
  (def rand-string (n)
    (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      (with (nc 62 s (newstring n) i 0)
        (while (< i n)
               (let x (readb str)
                 (unless (> x 247)
                   (= (s i) (c (mod x nc)))
                   (++ i))))
        s))))

;; TODO: Is this as inefficient as it looks? for uses the equivalent of list-ref
(mac forlen (var s body ...)
  (for var 0 (- (len s) 1) body ...))

(def best (f seq)
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))
              
(def max args (best > args))
(def min args (best < args))

(def most (f seq) 
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))
  
(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  (zap [insert-sorted test elt _] seq))

(def reinsert-sorted (test elt seq)
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  (zap [reinsert-sorted test elt _] seq))

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))

(mac defmemo (name parms body ...)
  (safeset name (memo (fn parms body ...))))

;; TODO: suck into core (performance)
(def <= args
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

;; TODO: Suck into core (performance)
(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str (stdin)))
  (awhen (readc str)
    (tostring 
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))

(mac summing (sumfn body ...)
  (let c 0
    (let sumfn (fn (arg) (if arg (++ c)))
      body ...)
    c))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def carif (x) (if (atom x) x (car x)))

(def prall (elts (o init "") (o sep ", "))
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))
             
(def prs args     
  (prall args "" #\space))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(def fill-table (table data)
  (each arg (pair data) (with (k (car arg) v (cdr arg))
                            (= (table k) v)))
  table)

(def keys (h) 
  (accum a (each (k v) h (a k))))

(def vals (h) 
  (accum a (each (k v) h (a v))))


(def tablist (h)
  (let z nil
    (maptable (fn (k v) (= z (cons (list k v) z))) h)
    z))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

;; TODO: Find a syntax to bring these back to Arc
;; obj - sucked into core (arg grouping)

(mac w/table (var body ...)
  (let var (table) body ... var))

(def load-table (file (o eof) (o multiple))
  (w/infile i file (read-table i eof multiple)))

(def read-table ((o i (stdin)) (o eof) (o multiple))
  (if multiple
      (w/table h
        (whiler e (read i eof) eof
          (= (h (car e)) (cadr e))))
      (let e (read i eof)
        (if (alist e) (listtab e) e))))

(def load-tables (file)
  (w/infile i file
    (let eof (uniq)
      (drain (read-table i eof) eof))))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h (o o (stdout)))
  (write (tablist h) o))

(def copy (x . args)
  (let x2 (case (type x)
            sym    x
            cons   (copylist x) ; (apply (fn args args) x)
            string (let new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i)))
                     new)
            table  (let new (table)
                     (each (k v) x 
                       (= (new k) v))
                     new)
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2) 
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns (o test >))
  ((sort test ns) (round (/ (len ns) 2))))

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copylist seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))


(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup (afn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (self y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (no r-x?) (scdr r x))
                        (if (cdr x) (self x (cdr x) y t) (scdr x y)))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(mac time (expr)
  (let t1 (msec)
    (do1 expr
         (let t2 (msec)
           (prn "time: " (- t2 t1) " msec.")))))

(mac etime (expr)
  (let t1 (msec)
    (do1 expr
         (let t2 (msec)
           (disp (string "time: " (- t2 t1) " msec.\n") (stderr))))))

(mac ttime (tag expr)
  (let t1 (msec)
    (do1 expr
         (let t2 (msec)
           (disp (string tag " time: " (- t2 t1) " msec.\n")
                 (stderr))))))

(mac jtime (expr)
  (do1 'ok (time expr)))

(mac time10 (expr)
  (time (repeat 10 expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

;; TODO: find a way to move these back out
;; deftem - sucked into functions :(
;; addtem - sucked into functions :(

(def inst (tem . args)
  (let x (eq-table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))


(def templatize (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))       
       (w/infile in file (readall in))))

(def number (n) (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts body ...)
  (safeset name (cache (fn () lasts)
                       (fn () body ...))))

(mac errsafe (expr)
  (on-err (fn (c) nil)
           (fn () expr)))

(def saferead (arg) (errsafe (read arg)))

(def safe-load-table (filename) 
  (or (errsafe (load-table filename))
      (table)))

(def date ((o s (seconds)))
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq) 
  (seq (rand (len seq))))

(mac until (test body ...)
  (while (no test) body ...))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (no fs)       t
           (no (cdr fs)) (apply (car fs) args)
                         (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor (args ...) (no (or args ...))) 

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

(def only (f) 
  (fn args (if (car args) (apply f args))))

(mac conswhen (f x y)
  (with (tf f tx x)
    (if (tf tx) (cons tx y) y)))

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def dupes (xs)
  (with (h (table) acc nil)
    (each x xs
      (if (h x)
          (push x acc)
          (set (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def reduce (f xs)
  (if (cddr xs)
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(let argsym (uniq)

  (def parse-format (str)
    (accum a
      (with (chars nil  i -1)
        (w/instring s str
          (whilet c (readc s)
            (case c 
              #\# (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (a (read s)))
              #\~ (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (readc s)
                      (a (list argsym (++ i))))
                  (push c chars))))
         (when chars
           (a (coerce (rev chars) 'string))))))
  )

(def load (file)
  (w/infile f file
    (let eof (uniq)
      (whiler e (read f eof) eof
        (eval e)))))

(def positive (x)
  (and (number x) (> x 0)))

(def ero args
  (w/stdout (stderr) 
    (each a args 
      (write a)
      (writec #\space))
    (writec #\newline))
  (car args))

(def queue () (list nil nil 0))

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val body ...)
  (with (nc n c 0)
    (each var val
      (when (multiple (++ c) nc)
        (pr ".")
        (flushout))
      body ...)
    (prn)
    (flushout)))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (if x (sym (map downc (coerce x 'string))) 'nil)
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (if x (sym (map upc (coerce x 'string))) 'NIL)
             (err "Can't upcase" x))))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

;; TODO: Generalize this for n lists?
(def zip (s1 s2 f (o eof (uniq)) (o index 0))
  (when (or s1 s2)
    (f (if s1 (car s1) eof)
       (if s2 (car s2) eof)
       eof
       index)
    (zip (cdr s1) (cdr s2) f eof (+ 1 index))))

(def mismatch (s1 s2)
  (catch
    (zip s1 s2 (fn (s1 s2 eof index)
                 (when (isnt s1 s2)
                   (throw index))))))

(def memtable (ks)
  (let h (table)
    (each k ks (set (h k)))
    h))

(= bar* " | ")

(mac w/bars (body ...)
  (let needbars nil
    (let out (tostring body)
      (unless (is out "")
        (if needbars
            (pr bar* out)
            (do (set needbars)
              (pr out))))) ...))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread (body  ...)
  (new-thread (fn () body ...)))

(mac trav (x fs ...)
  ((afn (g)
     (when g
       (fs g) ...))
   x))

(mac or= (place expr)
  (atomic
   (or place (= place expr))))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name rest ...)
  (= (hooks* 'name) (fn rest ...)))

;; TODO: Add caching here
(mac out (expr) (pr (tostring expr)))

(def get (index) [_ index])

(def ensure-dir (path)
  (unless (dir-exists path)
    (make-dir path)))

(let permfile (fn (var)
               (let s (string var)
                 (+ "perm/"
                    (rem ~alphadig s)
                    #\-
                    (string (intersperse #\- (map int (coerce s 'cons)))))))

(mac perm (var (o init) (o load readfile1) (o save writefile) 
               (o file (permfile var)))
  (ensure-var var)
  (= var (or (and (bound var) var)
             (do1 (iflet gf (file-exists file)
                    (load gf)
                    init)
                  (= (permwrite 'var) (fn (val) (save val file)))))))

(mac permat   (var file)
  (perm var nil readfile1 writefile file))

(mac permtable (var (o file (permfile var)))
  (perm var (table) load-table save-table file))

)

(mac evtil (expr test)
  (let v expr
    (while (no (test v))
      (= v expr))
    v))

(def rand-key (h)
  (if (empty h)
      nil
      (let n (rand (len h))
        (catch
          (each (k v) h
            (when (is (-- n) -1)
              (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))

(def percent (n) 
  (round (* 100 n)))

(def tfn args t)
