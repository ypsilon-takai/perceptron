;; my percetron inplemantation
;  core functions
;
;  Yosi
;
(ns yosi.perseptron
	(:use clojure.contrib.math))
;(use 'clojure.contrib.math)

;;
;  Constants
(def *e* 2.718281828)


;; �Z��
;  ���̓V�[�P���X�Əd�݂��󂯎���ďo�͂�Ԃ�
;  �l�͈̔͂� -1 <-> 1


;  �l����̓V�O���C�h
(defn sigmoid
	  ([x] (sigmoid x 1))
	  ([x gain] (/ 1 (+ 1 (expt *e* (- (* gain x)))))))

; ���̓��X�g�Əd�݃��X�g���󂯎���ďo�͂�Ԃ�
(defn get-output-one [input weight]
	  (sigmoid (apply + (map * input weight))))

(defn add-constant-input [lst]
	  (cons 1 lst))

; ���̓��X�g���X�g�Əd�݃��X�g���X�g���󂯎���ďo�͂�Ԃ�
(defn get-layer-output [weight-list input-list]
	  (cons
	   (add-constant-input
		  (map (fn [x] (get-output-one (first input-list) x)) weight-list))
	   (input-list)))







;���̓f�[�^��`
;  +-128 �̓��̓f�[�^�̏ꍇ

(def *data-input-max* (atom 128))
(def *data-input-min* (atom -128))


;; data structure
;  3�w�p�[�Z�v�g����
;   1�w��
(def *input-count* 4)  ;��
;   �d�݃f�[�^ W2
;   2�w��
(def *middle-count* 3) ;��
;   �d�݃f�[�^ W3
;   3�w��  2��
(def *output-count* 2) ;��


(defn create-random-weight-list [num]
	  (map (fn [x] (rand)) (range num)))

(defn init-weight [inputnum cellnum]
	  (map (fn [x] (create-random-weight-list inputnum)) (range cellnum)))

(def w2 (atom (init-weight (+ *input-count* 1) *middle-count*)))
(def w3 (atom (init-weight (+ *middle-count* 1) *output-count*)))

;; input data !! first is always 256
; dummy

(defn normalize-input-data [lst]
	  (let [ data-center-val (/ (+ @*data-input-max* @*data-input-min*) 2) ]
		(map (fn [x] (/ (- x data-center-val) @*data-input-max*)) lst)))

(defn get-input-data [input-list]
	  (add-constant-input (normalize-input-data input-list)))

;;; operator
;   operate 3 layer calc
(defn operate-3 [inp0]
	 (rest (get-layer-output @w3 (get-layer-output @w2 @inp0))))



(def inp0 (atom '(1 0.2 0.3 0.4 0.5)))
(reset! inp0 '[1 0.2 0.3 0.4 0.5])

(. System exit 0)
