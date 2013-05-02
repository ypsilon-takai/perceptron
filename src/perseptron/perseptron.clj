;; my percetron inplemantation
;  Yosi
;
(ns yosi.perseptron
	(:use clojure.contrib.math))
(use 'clojure.contrib.math)

;;
;  Constants
(def *e* 2.718281828)


;; data structure
;  3�w�p�[�Z�v�g����
;   1�w��
(def *input-count* 4)  ;��
;      �d�݃f�[�^ W2
;      2�w�ڂ�10�ɑ΂��đΉ�����1�w�ڂ���̃f�[�^�̏d��
;   2�w��
(def *middle-count* 3) ;��
;      �d�݃f�[�^ W3
;      3�w�ڂ� 2�ɑ΂��đΉ�����2�w�ڂ���̃f�[�^�̏d��
;   3�w��  2��
(def *output-count* 2) ;��

;;; operator
;   operate 3 layer calc
(defn operate-3 [inp0]
	 (rest (make-layer-output @w3 (make-layer-output @w2 @inp0))))



(defn create-random-weight-list [num]
	  (map (fn [x] (rand)) (range num)))

(defn init-weight [inputnum cellnum]
	  (map (fn [x] (create-random-weight-list inputnum)) (range cellnum)))

(def w2 (atom (init-weight (+ *input-count* 1) *middle-count*)))
(def w3 (atom (init-weight (+ *middle-count* 1) *output-count*)))


;; input data
;  ���̓f�[�^�� -1 < < 1 �̊Ԃ̐��ϊ�����

(def *data-input-max* (atom 128))
(def *data-input-min* (atom -128))

(defn add-constant-input [lst]
	  (cons 1 lst))

(defn normalize-input-data [lst]
	  (let [ data-center-val (/ (+ @*data-input-max* @*data-input-min*) 2) ]
		(map (fn [x] (/ (- x data-center-val) @*data-input-max*)) lst)))


; ���͗p�Ƀ��X�g�ɒ萔�f�[�^(���1)��ǉ�����
(defn get-input-data [input-list]
	  (add-constant-input (normalize-input-data input-list)))


;; �P�Z��
;  ���̓V�[�P���X�Əd�݂��󂯎���ďo�͂�Ԃ�
;  �l�͈̔͂�0 - 256
;  �l����̓V�O���C�h

;(defn sigmoid
;	  ([x] (sigmoid x 1))
;	  ([x gain] (/ 1 (+ 1 (expt *e* (- (* gain x)))))))

;; for debug
;(defn thuth [n]
;	 (/ (floor (* n 10000)) 10000))
;(defn sigmoid
;	  ([x] (sigmoid x 1))
;	  ([x gain] (thuth (/ 1 (+ 1 (expt *e* (- (* gain x))))))))

(defn sigmoid
	  ([x] (sigmoid x 1))
	  ([x gain] (/ 1 (+ 1 (expt *e* (- (* gain x)))))))

(defn make-node-output [prev-out weight]
	  (sigmoid (apply + (map * prev-out weight))))

(defn make-layer-output [weight-list prev-out]
	  (add-constant-input
		(map (fn [wt] (make-node-output prev-out wt)) weight-list)))
	   


;; Error Back Propagation
; �w�K��
(def *alfa* (atom 0.1))

; �G���[���v�Z
(defn error-val [out-list teach-data]
	  (/ (apply + (map (fn [x y] (expt (- x y) 2)) out-list teach-data)) 2))


;; �ŏI�i�̃V�O�}�̃��X�g���v�Z����
(defn make-sigma-list-last [out-data-list teach-data-list]
	  (map (fn [out-data teach-data] (* (- out-data teach-data) out-data (- 1 out-data)))
		   out-data-list
		   teach-data-list))

;; ���Ԓi�̃V�O�}�̃��X�g���v�Z
;
; �]�u�s��
;(defn transpose-seq [list-list]
;	  (apply map list list-list))
(defmacro transpose-seq [list-list]
  `(map list ~@list-list))

; �V�O�}�̌v�Z
(defn mul-sum [x-list y-list]
	  (apply + (map (fn [x y] (* x y)) x-list y-list)))

(defn calc-sigma-head [sigma-list weight-list]
	  (map (fn [w] (mul-sum sigma-list w)) (rest (transpose-seq weight-list))))

(defn make-sigma-list [sigma-list weight-list output-list]
	  (map (fn [sgh out] (* sgh out (- 1 out))) (calc-sigma-head sigma-list weight-list) output-list))


	   
;; �E�F�C�g�v�Z(�P��m�[�h)
(defn bp-node [alfa sigma input-list weight-list]
	  (map (fn [i w] (+ (- (* alfa sigma i)) w)) input-list weight-list))

;; �E�F�C�g�v�Z(�ŏI�i)
(defn bp-calc-weight-last [out-list teach-list input-list weight-list alfa]
	  (let [sigma-list (make-sigma-list-last out-list teach-list)]
		(map (fn [sigma w-list] (bp-node alfa sigma input-list w-list)) sigma-list weight-list)))

;; �E�F�C�g�v�Z(���Ԓi)
(defn bp-calc-weight [prev-sigma-list output-list input-list weight-list alfa]
	  (let [sigma-list (make-sigma-list prev-sigma-list weight-list output-list)]
		(map (fn [sigma w-list] (bp-node alfa sigma input-list w-list)) sigma-list weight-list)))
						

;; �덷�t�`�d�ŕύX��̃E�F�C�g�Z�o
(defn back-propagate [input-data teach-data alfa]
	  (let [out-list-1 (get-layer-output @w2 @input-data)
		    out-list-0 (get-layer-output @w3 out-list-1)]
		(if (< (error-val out2-list teach-data) 0.001)
			nil
		    (let [sigma-0 (make-sigma-list-last out-list-1 teach-data)
				  sigma-1 (make-sigma-list sigma-0 @w3 out-list-1)
				  w3tmp @w3
				  w2tmp @w2]
			  [(bp-calc-weight-last out-list-0 teach-data out-list-1 w3tmp alfa)    ; w3
			   (bp-calc-weight sigma-0 out-list-0 out-list-1 w2tmp alfa)]))))            ; w2



(. System exit 0)




