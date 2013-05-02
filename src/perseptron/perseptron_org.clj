;; my percetron inplemantation
;  Yosi
;
;(ns yosi.perseptron
;	(:use clojure.contrib.math))
(use 'clojure.contrib.math)

;;
;  Constants
(def *e* 2.718281828)

(def *data-input-max* (atom 128))
(def *data-input-min* (atom -128))
(reset! *data-input-max* 128)
(reset! *data-input-min* -128)



;; data structure
;  3層パーセプトロン
;   1層目
(def *input-count* 4)  ;個
;      重みデータ W2
;      2層目の10個に対して対応する1層目からのデータの重み
;   2層目
(def *middle-count* 3) ;個
;      重みデータ W3
;      3層目の 2個に対して対応する2層目からのデータの重み
;   3層目  2個
(def *output-count* 2) ;個


(defn create-random-weight-list [num]
	  (map (fn [x] (rand)) (range num)))

(defn init-weight [inputnum cellnum]
	  (map (fn [x] (create-random-weight-list inputnum)) (range cellnum)))

(def w2 (atom (init-weight (+ *input-count* 1) *middle-count*)))
(def w3 (atom (init-weight (+ *middle-count* 1) *output-count*)))

;; input data !! first is always 256
; dummy
(defn add-constant-input [lst]
	  (cons 1 lst))

(defn normalize-input-data [lst]
	  (let [ data-center-val (/ (+ @*data-input-max* @*data-input-min*) 2) ]
		(map (fn [x] (/ (- x data-center-val) @*data-input-max*)) lst)))

(defn get-input-data [input-list]
	  (add-constant-input (normalize-input-data input-list)))


;; 単セル
;  入力シーケンスと重みを受け取って出力を返す
;  値の範囲は0 - 256
;  値判定はシグモイド

;(defn sigmoid
;	  ([x] (sigmoid x 1))
;	  ([x gain] (/ 1 (+ 1 (expt *e* (- (* gain x)))))))

;; for debug
(defn thouth [n]
	 (/ (floor (* n 10000)) 10000))
(defn sigmoid
	  ([x] (sigmoid x 1))
	  ([x gain] (thouth (/ 1 (+ 1 (expt *e* (- (* gain x))))))))


(defn make-node-output [prev-out weight]
	  (sigmoid (apply + (map * prev-out weight))))

;(make-node-output '[1 0.2 0.3 0.4 0.5] '[0 0.1 0.2 0.3 0.4])

(defn make-layer-output [weight-list prev-out]
	  (add-constant-input
		(map (fn [wt] (make-node-output prev-out wt)) weight-list)))
	   


;;; operator
;   operate 3 layer calc
(defn operate-3 [inp0]
	 (rest (make-layer-output @w3 (make-layer-output @w2 @inp0))))



;; Error Back Propagation
; 学習率
(def *alfa* (atom 0.1))


(defn error-val [out-list teach-data]
	  (/ (apply + (map (fn [x y] (expt (- x y) 2)) out-list teach-data)) 2))

(thouth (error-val '[0.6082 0.7427] '[1 0]))
(def out-data 0.7427)
(def teach-data 0)
(def i 0.5987)

(bp-node @*alfa* -0.0933 '[1 0.5986 0.7068 0.7957] '[0 0.1 0.2 0.3])
(bp-calc-weight-last '[0.6082 0.7427] '[1 0] '[1 0.5986 0.7068 0.7957] '[[0 0.1 0.2 0.3] [0.2 0.3 0.4 0.5]])
(bp-calc-weight-last '(0.6082 0.7427) '(1 0) '(1 0.5986 0.7068 0.7957) '((0 0.1 0.2 0.3) (0.2 0.3 0.4 0.5)))
((0.009336310336800003 0.10558871536760848 0.20659890414605026 0.30742890213499174)
 (0.1858072473483 0.29150421826269235 0.3899685624257785 0.4887068267150423))
 
;; 最終段のシグマのリストを計算する
(defn make-sigma-list-last [out-data-list teach-data-list]
	  (map (fn [out-data teach-data] (* (- out-data teach-data) out-data (- 1 out-data)))
		   out-data-list
		   teach-data-list))

;; 中間段のシグマのリストを計算
; 転置行列
;(defn transpose-seq [list-list]
;	  (apply map list list-list))
(defmacro transpose-seq [list-list]
  `(map list ~@list-list))

;(transpose-seq [[0 0.1 0.2 0.3] [0.2 0.3 0.4 0.5]])
;(transpose-seq '((0 0.1 0.2 0.3)(0.2 0.3 0.4 0.5)))

(defn mul-sum [x-list y-list]
	  (apply + (map (fn [x y] (* x y)) x-list y-list)))

(defn calc-sigma-head [sigma-list weight-list]
	  (map (fn [w] (mul-sum sigma-list w)) (rest (transpose-seq weight-list))))

(defn make-sigma-list [sigma-list weight-list output-list]
	  (map (fn [sgh out] (* sgh out (- 1 out))) (calc-sigma-head sigma-list weight-list) output-list))


;(make-sigma-list '(-0.0933 0.1419) '[[0 0.1 0.2 0.3] [0.2 0.3 0.4 0.5]] '(0.5987 0.7069 0.7958))

	   
;; ウェイト計算(単一ノード)
(defn bp-node [alfa sigma input-list weight-list]
	  (map (fn [i w] (+ (- (* alfa sigma i)) w)) input-list weight-list))

;; ウェイト計算(最終段)
(defn bp-calc-weight-last [out-list teach-list input-list weight-list alfa]
	  (let [sigma-list (make-sigma-list-last out-list teach-list)]
		(map (fn [sigma w-list] (bp-node alfa sigma input-list w-list)) sigma-list weight-list)))

;; ウェイト計算(中間段)
(defn bp-calc-weight [prev-sigma-list output-list input-list weight-list alfa]
	  (let [sigma-list (make-sigma-list prev-sigma-list weight-list output-list)]
		(map (fn [sigma w-list] (bp-node alfa sigma input-list w-list)) sigma-list weight-list)))
						


(defn back-propagate [input-data teach-data alfa]
	  (let [out-list-1 (get-layer-output @w2 @input-data)
		    out-list-0 (get-layer-output @w3 out-list-1)]
		(if (< (error-val out2-list teach-data) 0.001)
			nil
		    (let [sigma-0 (make-sigma-list-last out-list-1 teach-data)
				  sigma-1 (make-sigma-list sigma-0 @w3 out-list-1)
				  w3tmp @w3
				  w2tmp @w2]
			  (do
				  ; update w3
				  (reset! w3 (bp-calc-weight-last out-list-0 teach-data out-list-1 w3tmp alfa))
				  ; update w2
				  (reset! w2 (bp-calc-weight sigma-0 out-list-0 out-list-1 w2tmp alfa))
				  true)))))
				




						  




					 (def teacher (atom 255))

;; scratch
(. System exit 0)

(use 'clojure.contrib.math)

(map (fn [x] (* (rand) x)) (range 10))

(init-weight 10 5)

(apply + (map * [1 2 3] [4 5 5]))
w2
w3
inp0

; test data
(reset! w2 '[[0 0.1 0.2 0.3 0.4] [0.2 0.3 0.4 0.5 0.6] [0.4 0.5 0.6 0.7 0.8]])
(reset! w3 '[[0 0.1 0.2 0.3] [0.2 0.3 0.4 0.5]])

(def inp0 (atom '[1 0.2 0.3 0.4 0.5]))
(reset! inp0 '[1 0.2 0.3 0.4 0.5])

(operate-3 inp0)

(normalize-input-data '(0 128 -128 64 -64))
