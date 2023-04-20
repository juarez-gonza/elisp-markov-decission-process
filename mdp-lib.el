(require 'dash)

(defun bellman-iterate (bellman-iteration num-of-states)
  "Fixed point on bellman equations with initial value set to 0"
  (funcall (-fixfn bellman-iteration)
	   (-repeat num-of-states 0.0)))

(defun zip->reduce-inclusive (f-zip f-reduce &rest seq*)
  (->> seq*
       (apply (-partial #'-zip-with f-zip))
       (-reduce f-reduce)))

(defalias 'sum-of-mul (-partial #'zip->reduce-inclusive #'* #'+))

(defmacro --min-by* (min-form &rest expr*)
  `(--min-by ,min-form (list ,@expr*)))

(defun take-stepped (xs step &optional init)
  (-select-by-indices (-iota (/ (length xs) step) (or init 0) step)
		      xs))

;; action is represented as (:name Any :cost Number [:state-name Probability]*)
(defalias 'action-name #'cadr)
(defalias 'action-cost #'cadddr)
(defalias 'action-state:prob (-partial #'nthcdr 4))

(defalias 'action-state:prob->alist (-partial #'-partition-in-steps 2 2))
;; (action-state:prob->alist '(:u 0.5 :d 0.3 :e 0.2)) translates to '((:u 0.5) (:d 0.3) (:e 0.2))

(defmacro ordered-action-probs (state-names state-action)
  (let ((state:prob (action-state:prob->alist (action-state:prob state-action))))
    `(list ,@(--map (car (alist-get it state:prob '(0.0))) ; default probability if not found in state:prob is 0.0
		    state-names))))
;; (ordered-action-probs (:u :d :e) (:name A :cost 10 :u 0.5 :e 0.2)) translates to (0.5 0.0 0.2)

(defmacro min-action-term (state-names state-action)
  `(+ ,(action-cost state-action)
      (sum-of-mul vs!! (ordered-action-probs ,state-names ,state-action))))
;; (min-action-term (:u :d :e) (:name A :cost 10 :u 0.5 :d 0.3 :e 0.2))
;; translates to (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2)))

(defmacro min-tagged-action-term (state-names state-action)
  `(cons (min-action-term ,state-names ,state-action)
	 ',(action-name state-action)))

(defun tagged-min (tagged-action-a tagged-action-b)
  (--min-by* (> (car it) (car other)) tagged-action-a tagged-action-b))

(defmacro min-action-impl (state-names state-actions min min-action-term)
  (cond ((null state-actions) 0) ; base case 1: base case for states with no action at all
	((null (cdr state-actions)) ; base case 2: base case for states with more than 1 action
	 `(,min-action-term ,state-names ,(car state-actions)))
	(t ; recursive step on state-actions
	 `(,min (,min-action-term ,state-names ,(car state-actions))
		(min-action ,state-names ,(cdr state-actions)
			    ,min ,min-action-term)))))

(defmacro min-action (state-names state-actions &optional min-f min-action-term-f)
  `(min-action-impl ,state-names ,state-actions
		    ,(or min-f 'min)
		    ,(or min-action-term-f 'min-action-term)))
;; (min-action (:u :d :e) ((:name A :cost 10 :u 0.5 :d 0.3 :e 0.2) (:name B :cost 25 :u 0.2 :d 0.7 :e 0.1)))
;; translates to (min (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2))) (+ 25 (sum-of-mul vs!! (list 0.2 0.7 0.1))))

(defmacro mdp-bellman-eq-body (state-names &rest state-actions*)
  (if (null state-actions*) nil
    `(cons (min-action ,state-names ,(car state-actions*))
	   (mdp-bellman-eq-body ,state-names ,@(cdr state-actions*)))))

(defmacro mdp-tagged-bellman-eq-body (state-names &rest state-actions*)
  (if (null state-actions*) nil
    `(cons (min-action ,state-names ,(car state-actions*) tagged-min min-tagged-action-term)
	   (mdp-tagged-bellman-eq-body ,state-names ,@(cdr state-actions*)))))

;; (mdp-bellman-eq-body (:u :d :e)
;;   ((:name A :cost 10 :u 0.5 :d 0.3 :e 0.2) (:name B :cost 25 :u 0.2 :d 0.7 :e 0.1))
;;   ((:name A :cost 10 :u 0.8 :e 0.2) (:name B :cost 25 :d 0.3 :e 0.7))
;;   ())
;; translates to
;; (cons
;;  (min (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2)))
;;       (+ 25 (sum-of-mul vs!! (list 0.2 0.7 0.1))))
;;  (cons
;;   (min (+ 10 (sum-of-mul vs!! (list 0.8 0.0 0.2)))
;;        (+ 25 (sum-of-mul vs!! (list 0.0 0.3 0.7))))
;;   (cons 0 nil)))

;; (mdp-tagged-bellman-eq-body (:u :d :e)
;; 			 ((:name A :cost 10 :u 0.5 :d 0.3 :e 0.2) (:name B :cost 25 :u 0.2 :d 0.7 :e 0.1))
;; 			 ((:name A :cost 10 :u 0.8 :e 0.2) (:name B :cost 25 :d 0.3 :e 0.7))
;; 			 ())
;; translates to
;; (cons
;;  (tagged-min (cons (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2))) 'A)
;; 		  (cons (+ 25 (sum-of-mul vs!! (list 0.2 0.7 0.1))) 'B))
;;  (cons
;;   (tagged-min (cons (+ 10 (sum-of-mul vs!! (list 0.8 0.0 0.2))) 'A)
;; 		   (cons (+ 25 (sum-of-mul vs!! (list 0.0 0.3 0.7))) 'B))
;;   (cons 0 nil)))

(defalias 'mdp-bellman-equations #'car)
(defalias 'mdp-tagged-bellman-equations #'cadr)
(defalias 'mdp-state-names #'cddr)
(defun make-mdp (bellman-equations tagged-bellman-equations state-names)
  (cons bellman-equations (cons tagged-bellman-equations state-names)))

(defmacro define-mdp (name &rest state:actions*)
  (let ((state-names (take-stepped state:actions* 2 0))
	(state-actions (take-stepped state:actions* 2 1)))
    `(setq ,name ; parameter name `vs!!` finishes in !! to mark the name as non-hygienic capturable
	   (make-mdp (lambda (vs!!) (mdp-bellman-eq-body ,state-names ,@state-actions))
		     (lambda (vs!!) (mdp-tagged-bellman-eq-body ,state-names ,@state-actions))
		     ',state-names))))

(defun solve-mdp (mdp)
  ;; iterates with untagged bellman equations (only numeric operations)
  ;; runs a final iteration with tagged minimization in order to get the optimal plan
  (let ((state-names (mdp-state-names mdp)))
    (->> (bellman-iterate (mdp-bellman-equations mdp)
			  (length state-names))
	 (funcall (mdp-tagged-bellman-equations mdp))
	 (funcall (-partial #'-zip state-names)))))

;;; Usage example
;;; Define the MDP with key-valued arguments where :key is the name of the state
;;; and the value is a list of actions indicating action :name and :cost followed by probability of some or all states
;;; States with no actions (usually the final state) can have an empty list as their value (see :e below)

(define-mdp mdp-ex-4
  :u ((:name Antivirus :cost 10 :u 0.5 :d 0.3 :e 0.2)
      (:name Staff :cost 25 :u 0.2 :d 0.7 :e 0.1))
  :d ((:name Antivirus :cost 10 :d 0.75 :e 0.2)
      (:name Staff :cost 25 :d 0.3 :e 0.7))
  :e ())

;;; Solve the MDP
;; (solve-mdp mdp-ex-4)


(provide 'mdp-lib)
