(declare-fun x_prev () Real)
(declare-fun x () Real)
(declare-fun dx_prev () Real)
(declare-fun dx () Real)
(declare-fun ASSUMPTION_0 () Bool)

(assert (and (< (+ (* (- 1.0) dx_prev) x_prev) 1.0)
             (< (+ (* (- 2.0) dx_prev) x_prev) 1.0)
             (< (+ dx_prev (* (- 1.0) x_prev)) 1.0)
             (< (+ (* 2.0 dx_prev) (* (- 1.0) x_prev)) 1.0)
             (= x (+ x_prev dx))))

(assert (= ASSUMPTION_0 (and (< (+ dx (* (- 1.0) dx_prev)) 1.0)
                           (> (+ dx (* (- 1.0) dx_prev)) (- 1.0)))))

(assert ASSUMPTION_0)

(minimize x)
(maximize x)

(check-sat)
(get-objectives)
