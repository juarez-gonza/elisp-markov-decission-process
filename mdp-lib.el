(require 'dash)
(require 'cl-lib)

;; TODO: update action examples with :name field

(defun bellman-iterate (bellman-iteration num-of-states)
  "Fixed point on bellman equations with initial value set to 0"
  (funcall (-fixfn bellman-iteration)
	   (-repeat num-of-states 0.0)))

(defun zip->reduce-inclusive (f-zip f-reduce &rest seq*)
  (->> seq*
       (apply (-partial #'-zip-with f-zip))
       (-reduce f-reduce)))

(defalias 'sum-of-mul (-partial #'zip->reduce-inclusive #'* #'+))

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
;; (ordered-action-probs (:u :d :e) (:cost 10 :u 0.5 :e 0.2)) translates to (0.5 0.0 0.2)

(defmacro min-action-term (state-names state-action)
  `(+ ,(action-cost state-action)
      (sum-of-mul vs!! (ordered-action-probs ,state-names ,state-action))))
;; (min-action-term (:u :d :e) (:cost 10 :u 0.5 :d 0.3 :e 0.2))
;; translates to (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2)))

(defmacro min-action (state-names state-actions)
  (cond ((null state-actions) 0) ; base case 1: base case for states with no action at all
	((null (cdr state-actions)) ; base case 2: base case for states with more than 1 action
	 `(min-action-term ,state-names ,(car state-actions)))
	(t ; recursive step on state-actions
	 `(min (min-action-term ,state-names ,(car state-actions))
	       (min-action ,state-names ,(cdr state-actions))))))
;; (min-action (:u :d :e) ((:cost 10 :u 0.5 :d 0.3 :e 0.2) (:cost 25 :u 0.2 :d 0.7 :e 0.1)))
;; translates to (min (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2))) (+ 25 (sum-of-mul vs!! (list 0.2 0.7 0.1))))

(defmacro min-action-tagged (state-names state-actions)
  (cond ((null state-actions) 0) ; base case 1: base case for states with no action at all
	((null (cdr state-actions)) ; base case 2: base case for states with more than 1 action
	 `(cons (min-action-term ,state-names ,(car state-actions))
		',(action-name (car state-actions))))
	(t ; recursive step on state-actions
	 `(--min-by (> (car it) (car other))
		    (list (cons (min-action-term ,state-names ,(car state-actions))
				',(action-name (car state-actions)))
			  (min-action-tagged ,state-names ,(cdr state-actions)))))))
;; (min-action-tagged (:u :d :e) ((:name ac-2 :cost 10 :u 0.5 :d 0.3 :e 0.2) (:name ac-1 :cost 25 :u 0.2 :d 0.7 :e 0.1)))

(defmacro define-mdp-impl (minimize-action state-names &rest state-actions*)
  (if (null state-actions*)
      nil
    `(cons (,minimize-action ,state-names ,(car state-actions*))
	   (define-mdp-impl ,minimize-action ,state-names ,@(cdr state-actions*)))))
;; (define-mdp-impl min-action (:u :d :e)
;;   ((:cost 10 :u 0.5 :d 0.3 :e 0.2) (:cost 25 :u 0.2 :d 0.7 :e 0.1))
;;   ((:cost 10 :u 0.8 :e 0.2) (:cost 25 :d 0.3 :e 0.7))
;;   ())
;; translates to
;; (cons
;;  (min (+ 10 (sum-of-mul vs!! (list 0.5 0.3 0.2)))
;;       (+ 25 (sum-of-mul vs!! (list 0.2 0.7 0.1))))
;;  (cons (min (+ 10 (sum-of-mul vs!! (list 0.8 0.0 0.2)))
;; 	    (+ 25 (sum-of-mul vs!! (list 0.0 0.3 0.7))))
;;        (cons 0 nil)))

(defalias 'mdp-bellman-equations #'car)
(defalias 'mdp-tagged-bellman-equations #'cadr)
(defalias 'mdp-state-names #'cddr)
(defun make-mdp (bellman-equations tagged-bellman-equations state-names)
  (cons bellman-equations (cons tagged-bellman-equations state-names)))

(defmacro define-mdp (name &rest state:actions*)
  (cl-symbol-macrolet ((state-names (take-stepped state:actions* 2 0))
		       (state-actions (take-stepped state:actions* 2 1)))
    `(setq ,name ; parameter name `vs!!` finishes in !! to mark the name as non-hygienic capturable
	   (make-mdp
	    (lambda (vs!!) (define-mdp-impl min-action ,state-names ,@state-actions))
	    (lambda (vs!!) (define-mdp-impl min-action-tagged ,state-names ,@state-actions))
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
;;; and the value is a list of actions indicating action :cost followed by probability of some or all states
;;; States with no actions (usually the final state) can have an empty list as their value (see :e below)

(define-mdp mdp-ex-4
  :u ((:name Antivirus :cost 10 :u 0.5 :d 0.3 :e 0.2)
      (:name Staff :cost 25 :u 0.2 :d 0.7 :e 0.1))
  :d ((:name Antivirus :cost 10 :d 0.75 :e 0.2)
      (:name Staff :cost 25 :d 0.3 :e 0.7))
  :e ())

;;; This translates to roughly the following

;; (defun mdp-ex-4-expanded (ude)
;;   (list
;;    (min (+ 10 (sum-of-mul ude '(0.5 0.3 0.2)))
;; 	(+ 25 (sum-of-mul ude '(0.2 0.7 0.1))))
;;    (min (+ 10 (sum-of-mul ude '(0.8 0.0 0.2)))
;; 	(+ 25 (sum-of-mul ude '(0.0 0.3 0.7))))
;;    0))

;;; Solve the MDP
;; (solve-mdp mdp-ex-4)

;;; Same result as the manually specified mdp
;; (bellman-iterate #'mdp-ex-4-expanded 3)
