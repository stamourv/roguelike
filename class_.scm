;; utilitary macros for the object system
(define-macro (init-header)
  (eval '(begin
           (define (symbol-append s1 . ss)
             (string->symbol (apply string-append
                                    (symbol->string s1)
                                    (map symbol->string ss))))
           (define (gen-instantiator-name name)
             
             (symbol-append 'make- name '-instance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic constructors (new)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (new class-name . params)
  `(init! (,(gen-instantiator-name class-name)) ,@params))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (update! obj class field f)
  (let ((objval (gensym 'objval)))
   `(let ((,objval ,obj))
      (,(gen-setter-name class field) ,objval
       (,f (,(gen-accessor-name class field) ,objval))))))

(define-macro (set-fields! obj class field-val-list)
  (let ((obj-ptr (gensym 'obj)))
    `(let ((,obj-ptr ,obj))
       ,@(map (lambda (field-val)
                (if (not (and (list? field-val)
                              (= (length field-val) 2)))
                    (error "invalid set-fields! field syntax"))
                (let ((field-name (car field-val))
                      (val        (cadr field-val)))
                  `(,(gen-setter-name class field-name) ,obj-ptr ,val)))
              field-val-list)
       ,obj-ptr)))
(init-header)