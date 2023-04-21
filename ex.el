(add-to-list 'load-path "./")
(require 'mdp-lib)

;;; 1: MDP with three states

;; 1.1

(define-mdp mdp-ex-1
  :a ((:name p :cost 1 :b 0.8 :a 0.2)
      (:name q :cost 1 :c 0.1 :a 0.9))
  :b ((:name q :cost 1 :c 0.9 :a 0.1))
  :c ())

;; (solve-mdp mdp-ex-1)
;; ((:a 2.5 . p) (:b 1.25 . q) (:c . 0))

;;; 2: Treating disease

(define-mdp mdp-ex-2
  :d ((:name S :cost 100 :d 0.4 :a 0.1 :c 0.5)
      (:name Q :cost 10 :d 0.7 :c 0.3)
      (:name R :cost 6 :d 0.1 :a 0.6 :c 0.3))
  :a ((:name Q :cost 10 :a 0.4 :c 0.6)
      (:name R :cost 6 :a 0.7 :c 0.3))
  :c ())

;; (solve-mdp mdp-ex-2)
;; ((:d 17.777777777777775 . R) (:a 16.666666666666664 . Q) (:c . 0))

;;; 3: Mars Rover

(define-mdp mdp-ex-3
  :s ((:name Left :cost 4 :r 0.75 :b 0.25)
      (:name Right :cost 7 :r 1.0)
      (:name Descend :cost 2 :b 1.0))
  :b ((:name Ascend :cost 3 :b 0.2 :r 0.8))
  :r ())

;; (solve-mdp mdp-ex-3)
;; ((:s 4.9375 . Left) (:b 3.75 . Ascend) (:r . 0))

;;; 4: Computer Security

(define-mdp mdp-ex-4
  :u ((:name Antivirus :cost 10 :u 0.5 :d 0.3 :e 0.2)
      (:name Staff :cost 25 :u 0.2 :d 0.7 :e 0.1))
  :d ((:name Antivirus :cost 10 :d 0.75 :e 0.2)
      (:name Staff :cost 25 :d 0.3 :e 0.7))
  :e ())

;; (solve-mdp mdp-ex-4)
;; ((:u 41.428571428571416 . Antivirus) (:d 35.71428571428571 . Staff) (:e . 0))
