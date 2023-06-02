; irem nur yildirim
; 2021400042
; compiling: yes
; complete: yes




#lang racket

(provide (all-defined-out))
(require racket/base)
;(define input-file "example1.pi")

;(define (main)
 ; (let ((program (parse input-file)))
  ;  (eval program)))


; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
         ((printf "Hello, world!\n") (get (eval-exprs (parse program-str) empty-state) '-r))
    )
)

; solution starts here
; 1. empty-state (5 points)
(define empty-state (make-immutable-hash))
 ;no parameters
; 2. get (5 points)
(define (get state var) ;get is the func name state and var are parameters
  (let ((val (hash-ref state '-r #f))) (printf "get and state ~s" state)
    (if (eq? val #f) ;;val type ı procedure ise apply?
        var
        val)))


; 3. put (5 points)
;(define (put state var val) (hash-set state var val)) ;state var and val are parameters
(define (put state key value)
  ( let ((new-hash (create-hash (list key) (list value))))
    (add-to-hash state new-hash)))



(define (operator? expr)
  (and (symbol? expr)
       (memv expr '(+ - * / < > sqrt cdr car list length list-ref append))))

(define (type-of x)
  (cond
    ((string? x) 'string)
    ((number? x) 'number)
    ((boolean? x) 'boolean)
    ((pair? x) 'pair)
    ((null? x) 'null)
    ((symbol? x) 'symbol)
    ((procedure? x) 'procedure)
    (else 'unknown)))



(define (last-element lst)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (last-element (cdr lst)))))



(define (eval-expr expr state)
  (cond
    ((number? expr) (put state '-r expr)) ;if expr is a number create a new pair in state
    ((boolean? expr)  (put state '-r expr))
    ((string? expr) (put state '-r expr))
    ((operator? expr)(let ((val (eval expr)))(put state '-r val)))
    ((procedure? expr) (put state '-r (eval expr)))


    
    ((symbol? expr)
     (cond ((hash-has-key? state expr)
            (let ((val (hash-ref state expr)))
              (put state '-r val) 
              ))
           (else (printf "expr is symbol and not in hash en eval  ~s" expr))))


    
    
    ((null? expr) (put state '-r (list)) (printf "null"))
    ;;list expresion
    ((list? expr)
     (let ((operator (car expr)) ;call the according function by checking operator
           (operands (cdr expr)))
       (cond
         ((eq? operator 'quote) (cons state (car operands)))
        ((eq? operator ':=) (let ((var (car operands))
                          (val-expr (cadr operands)))
                       (:= var val-expr state)))


        
         ((eq? operator 'while:) (let ((test-expr (car operands))
                                    (body-exprs (cadr operands))
                                   )
                                (while: test-expr body-exprs state)))
         ((eq? operator 'func)(let ((params (car operands))
                                    (expr (cadr operands))
                                   )
                                (func params expr state)))
         ((eq? operator 'if:) (let ((test-expr (car operands))
                                    (then-exprs (cadr operands))
                                    (else-exprs (caddr operands)))
                                (if: test-expr then-exprs else-exprs state)))
          ((eq? operator 'lambda)(let ((params (car operands))
                                    (expr (cadr operands))
                                   )
                                (func params expr empty-state)))



          ((number? operator)
           (put state '-r operator)) ;; if it is sth like ( 5 10) then return the list in -r key to the map-eval
          ((eq? operator 'eq?)(let ((var1 (map-eval (car operands) state))
                                    (var2 (map-eval (cadr operands) state ))
                                   ) 
                            (put state '-r (equal? var1 var2)))) ;;eq yapınca "circle esit olmuoyr
          
          
          
        ((eq? operator 'printf)
 (let ((result (map-eval (cdr operands) state)))
   (apply printf
          (car operands)
          (last-element (list result)))
   (put state '-r (last-element (list result)))
   state))

          

       ((procedure? operator) (printf "procedure and operator ~s" operator))
  
       ((symbol? operator)
       (cond
         ;; Check if the symbol is an operator
          ((operator? operator)
          (let ((evaluated-operands (map-eval operands state)))
            (case operator
              ((+) (let ((result (apply + evaluated-operands)))
                     (put state '-r result)
                     ))
              ((-) (let ((result (apply - evaluated-operands)))
                     (put state '-r result)
                     ))
              ((*) (let ((result (apply * evaluated-operands)))
                       (put state '-r result)
                       ))
              ((/) (let ((result (apply / evaluated-operands)))
                     (put state '-r result)
                     ))
              ((<) (let ((result (apply < evaluated-operands)))
                     (put state '-r result)
                     ))
              ((>) (let ((result (apply > evaluated-operands)))
                     (put state '-r result)
                     ))
              ((sqrt) (let ((result (apply sqrt evaluated-operands)))
                     (put state '-r result)
                     ))
              ((car) (let ((result (apply car evaluated-operands)))
                     (put state '-r result)
                     ))
              ((cdr) (let ((result (apply cdr evaluated-operands)))
                     (put state '-r result)
                     ))
              ((list) (let ((result (apply list evaluated-operands)))
                     (put state '-r result)
                     ))
              ((length) (let ((result (apply length  evaluated-operands)))
                     (put state '-r result)
                     ))
              ((list-ref) (let ((result (apply list-ref evaluated-operands)))
                     (put state '-r result)
                     ))
               ((append) (let ((result (apply append evaluated-operands)))
                     (put state '-r result)
                     ))
              (else (error 'eval-expr "Unknown operator.")))))

         
         
         ;; Symbol is not an operator, check if it exists in the state
         ((hash-has-key? state operator)
    (let* ((new-state (eval-expr operator state))
           (val (hash-ref new-state '-r)))
      (if (procedure? val)
          (let ((result (apply val operands)))
            (put state '-r result))
          (put state '-r val))))




         
         ;; Symbol is not an operator and not in the state, raise an error
         (else ; Print the evaluated operator
                  (apply operator (map-eval operands state))
                (error 'eval-expr "The symbol is not applicable as a procedure: ~s." operator))))


          
        


           ((list? operator) (eval-expr operator state))


         )))))



; 4. := (15 points)

(define (:= var val-expr state)
  (let* ((state (eval-exprs (list val-expr) state))
         (val (hash-ref state '-r))
         (state (put state var val)))
    state))







; 5. if: (15 points)
(define (if: test-expr then-exprs else-exprs state)
  (let ((result-state (eval-expr test-expr state)))
    (if (hash-ref result-state '-r)
        (let ((new-state (eval-exprs then-exprs state)))
          new-state)
        (let ((new-state (eval-exprs else-exprs state)))
          new-state))))

 ;;if e state koyunca i sıfırlanıyor

; 6. while: (15 points)
(define (while: test-expr body-exprs state)
  (let loop ((state state))
    (let ((result-state (eval-expr test-expr state)))
      (if (hash-ref result-state '-r)
          (begin
            (let ((new-state (eval-exprs body-exprs state)))
              (loop new-state)))
          state))))





 (define (map-eval expr state)
  (cond
    ((null? expr) '())
    ((pair? expr)
     (let ((result-state (eval-expr (car expr) state)))
       (cons (hash-ref result-state '-r) (map-eval (cdr expr) result-state))))
    (else
     (let ((result-state (eval-expr expr state)))
         (list (hash-ref result-state '-r))))))



(define (butlast lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (butlast (cdr lst)))))


; 7. func (15 points)
(define (func params body-exprs state)
  (let ((func-body (lambda args state
                     (let ((new-state (eval-exprs body-exprs (add-to-hash state (create-hash params args)))))
                       (hash-ref new-state '-r)))))
    (put state '-r func-body))state)






  


; 9 eval-exprs (5 points)
(define (eval-exprs exprs state)
  (foldl (lambda (expr acc-state)
           (if (void? expr)
               acc-state
               (let ((new-state (eval-expr expr acc-state)))
                 new-state)))
         state
         exprs))




;(main)
(eval-exprs (parse "example4.pi") empty-state)





