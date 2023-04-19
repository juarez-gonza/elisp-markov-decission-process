(require 'dash)
(require 'cl-lib)

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

(defun action-dest->alist (&rest action-dest*)
  (-zip-pair (take-stepped action-dest* 2 0)
	     (take-stepped action-dest* 2 1)))
;; (action-dest->alist :u 0.5 :d 0.3 :e 0.2) translates to ((:u . 0.5) (:d . 0.3) (:e . 0.2))

(defmacro ordered-action-probs (state-names state-action)
  (let ((state:prob (apply #'action-dest->alist (cddr state-action))))
    `(list ,@(--map (alist-get it state:prob 0.0)
		    state-names))))
;; (ordered-action-probs (:u :d :e) (:cost 10 :u 0.5 :e 0.2)) translates to (0.5 0.0 0.2)

(defmacro min-action (state-names state-actions)
  (cond
   ((null state-actions) 0) ; base case 1: states with no action at all
   ((null (cdr state-actions)) ; base case 2: states with at least one action
    `(+ ,(cadar state-actions)
	(sum-of-mul vs (ordered-action-probs ,state-names ,(car state-actions)))))
    (t ; recursive step on state-actions
     `(min (+ ,(cadar state-actions)
	      (sum-of-mul vs (ordered-action-probs ,state-names ,(car state-actions))))
	   (min-action ,state-names ,(cdr state-actions))))))
;; (min-action (:u :d :e) ((:cost 10 :u 0.5 :d 0.3 :e 0.2) (:cost 25 :u 0.2 :d 0.7 :e 0.1)))
;; translates to (min (+ 10 (sum-of-mul vs (list 0.5 0.3 0.2))) (+ 25 (sum-of-mul vs (list 0.2 0.7 0.1))))

(defmacro define-mdp-impl (state-names &rest state-actions*)
  (if (null state-actions*)
      nil
    `(cons (min-action ,state-names ,(car state-actions*))
	   (define-mdp-impl ,state-names ,@(cdr state-actions*)))))
;; (define-mdp-impl (:u :d :e)
;;   ((:cost 10 :u 0.5 :d 0.3 :e 0.2) (:cost 25 :u 0.2 :d 0.7 :e 0.1))
;;   ((:cost 10 :u 0.8 :e 0.2) (:cost 25 :d 0.3 :e 0.7))
;;   ())
;; translates to
;; (cons
;;  (min (+ 10 (sum-of-mul vs (list 0.5 0.3 0.2)))
;;       (+ 25 (sum-of-mul vs (list 0.2 0.7 0.1))))
;;  (cons (min (+ 10 (sum-of-mul vs (list 0.8 0.0 0.2)))
;; 	    (+ 25 (sum-of-mul vs (list 0.0 0.3 0.7))))
;;        (cons 0 nil)))

(defmacro define-mdp (name &rest state:actions*)
  (cl-symbol-macrolet ((state-names (take-stepped state:actions* 2 0))
		       (state-actions (take-stepped state:actions* 2 1)))
    `(defun ,name (vs)
	 (define-mdp-impl ,state-names ,@state-actions))))

;;; Usage example
;;; Define the MDP with key-valued arguments where :key is the name of the state
;;; and the value is a list of actions indicating action :cost followed by probability of some or all states
;;; States with no actions (usually the final state) can have an empty list as their value (see :e below)

;; (define-mdp mdp-ex-4
;;   :u ( (:cost 10 :u 0.5 :d 0.3 :e 0.2)
;;       (:cost 25 :u 0.2 :d 0.7 :e 0.1))
;;   :d ( (:cost 10 :u 0.8 :e 0.2)
;;       (:cost 25 :d 0.3 :e 0.7))
;;   :e ())

;;; This translates to roughly the following

;; (defun mdp-ex-4-expanded (ude)
;;   (list
;;    (min (+ 10 (sum-of-mul ude '(0.5 0.3 0.2)))
;; 	(+ 25 (sum-of-mul ude '(0.2 0.7 0.1))))
;;    (min (+ 10 (sum-of-mul ude '(0.8 0.0 0.2)))
;; 	(+ 25 (sum-of-mul ude '(0.0 0.3 0.7))))
;;    0))

;;; Solve the MDP
;; (bellman-iterate #'mdp-ex-4 3)

;;; Same result as the manually specified mdp
;; (bellman-iterate #'mdp-ex-4-expanded 3)
