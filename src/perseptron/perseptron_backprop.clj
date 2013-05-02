;; my percetron inplemantation
;  Yosi
;
(ns yosi.perseptron
	(:use clojure.contrib.math))


;; Error Back Propagation
(def *alfa* (atom 0.1))


(defn error-val [out-list teach-data]
	  (/ (apply + (map (fn [x y] (expt (- x y) 2)) (first out-list) teach-list)) 2))

(defn bp-out [out-data teach-data input-list weight-list]
	  (let [sigma (* (- out-data teach-data) out-data (- 1 out-data))]
		(map (fn [i w] (+ (- (* *alfa* sigma i))) w) input-list weight-list)))

(defn bp-inner [out-data ]
	  )


(defn back-propagate [input-data teach-data]
  (let [out1-list (get-layer-output @w2 @input-data)
        out2-list (get-layer-output @w3 out1-list)]
	(if (< (error-val out2-list teach-data) 0.001)
		nil
	    (let [sigma1 (map (fn [out teach] (* (- out teach) out (- 1 out))) out1-list teach-data)
		      sigma2 


(def teacher (atom 255))

