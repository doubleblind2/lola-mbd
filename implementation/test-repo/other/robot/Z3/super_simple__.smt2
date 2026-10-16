(declare-fun x^-1 () Real)
(declare-fun x^0 () Real)
(declare-fun dx^-1 () Real)
(declare-fun dx^0 () Real)
(declare-fun ASSUMPTION^0 () Bool)

(assert (and (< (+ (* (- 1.0) dx^-1) x^-1) 1.0)
     (< (+ (* (- 2.0) dx^-1) x^-1) 1.0)
     (< (+ dx^-1 (* (- 1.0) x^-1)) 1.0)
     (< (+ (* 2.0 dx^-1) (* (- 1.0) x^-1)) 1.0)))

(assert (= x^0 (+ x^-1 dx^0)))

(assert (let ((a!1 (and (< (+ dx^0 (* (- 1.0) dx^-1)) 1.0)
                (> (+ dx^0 (* (- 1.0) dx^-1)) (- 1.0)))))
  (= ASSUMPTION^0 a!1)))

(assert ASSUMPTION^0)


(maximize x^0)

(check-sat)
(get-objectives)
