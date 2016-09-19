(ns clojush.problems.demos.project2
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.random]
        [clojush.interpreter]
        [clojure.math.numeric-tower]
        [clojush.instructions.tag]
        [clojush.ns]
        [clojush.util]
        [clojush.instructions.numbers]))

(use-clojush)

(defn
  integer_fromfloat

[state]
    (if (not (empty? (:float state)))
      (let [item (stack-ref :float 0 state)]
        (->> (pop-item :float state)
             (push-item (truncate item) :integer)))
      state))

(defn
  float_frominteger

[state]
    (if (not (empty? (:integer state)))
      (let [item (stack-ref :integer 0 state)]
        (->> (pop-item :integer state)
             (push-item (*' 1.0 item) :float)))
      state))

(defn getlengthF
  [state cnt]
    (if (empty? (:float state))
      cnt
      (getlengthF (pop-item :float state) (+ 1 cnt))))

(defn checkboolean
  [state cnt iftrue]
  (if (empty? (:boolean state))
      (if (and (= 0 cnt) iftrue)
        0
        (* (+ 1 cnt) (+ 1 (getlengthF state 0))))
      (if (= true (top-item :boolean state))
        (checkboolean (pop-item :boolean state) cnt true)
        (checkboolean (pop-item :boolean state) (+ cnt 1) false))))

(defn getlengthInt
  [state cnt]
    (if (empty? (:integer state))
      cnt
      (getlengthInt (pop-item :integer state) (+ 1 cnt))))



;0121
;0101
;0101
;0001

(def argmap
  {:error-function (fn [program]
                     (doall
                      (for [input (range 1)]
                       (let [state (->> (make-push-state)

                                       (push-item (float 1) :float)
                                       (push-item (float 0) :float)
                                       (push-item (float 0) :float)
                                       (push-item (float 0) :float)

                                       (push-item (float 1) :float)
                                       (push-item (float 0) :float)
                                       (push-item (float 1) :float)
                                       (push-item (float 0) :float)

                                       (push-item (float 1) :float)
                                       (push-item (float 0) :float)
                                       (push-item (float 1) :float)
                                       (push-item (float 0) :float)

                                       (push-item (float 1) :float)
                                       (push-item (float 2) :float)
                                       (push-item (float 1) :float)
                                       (push-item 0 :integer)

                                       (run-push program))]
                        
                           (abs (checkboolean state 0 false))))))
   :atom-generators (list 
                          'up_move
                          'down_move
                          'left_move
                          'right_move)
   :parent-selection :tournament 
   :genetic-operator-probabilities {:alternation 0.5
                                    :uniform-mutation 0.5}
   })

;(define-registered
;  integer_func
;  ^{:stack-types [:integer]}
;  (fn [state]
;    (if (not (empty? (:integer state)))
;      (let [item (stack-ref :integer 0 state)]
;        (->> (pop-item :integer state)
;             (push-item (+ item 1) :integer)))
;     (push-item 1 :integer state))))

;Integer is first stack
;Down move 4 from float to integer

(define-registered
  down_move
  ^{:stack-types [:integer :float :boolean]}
  (fn [state]
    (let [changed_state (integer_fromfloat
                          (integer_fromfloat
                            (integer_fromfloat state)))]
      (if (empty? (:float changed_state))
        state
        (let [top-float (top-item :float changed_state)
              ]
          (if (= top-float 1.0)
            (push-item  false :boolean
              (integer_fromfloat changed_state))
            (if (= top-float 2.0)
              (push-item  true :boolean
                (integer_fromfloat changed_state))
              (integer_fromfloat changed_state))))))))

;Up move 4 from integer to float
(define-registered
  up_move
  ^{:stack-types [:integer :float :boolean]}
  (fn [state]
    (let [changed_state (float_frominteger
                          (float_frominteger
                            (float_frominteger 
                              (float_frominteger state))))]
      (if (empty? (:integer changed_state))
        state
        (let [top-int (top-item :integer changed_state)
              ]
          (if (= 1 top-int)
            (push-item  false  :boolean changed_state)
            (if (= 2 top-int)
              (push-item  true :boolean changed_state)
              changed_state)))))))

;Left move 1 from integer to float
(define-registered
  left_move
  ^{:stack-types [:integer :float :boolean]}
  (fn [state]
    (if (or (empty? (:integer state)) (== (getlengthInt state 0) 1))
        state
        (let [top-int (top-item :integer (pop-item :integer state))
              ]
          (if (= 1 top-int)
                (push-item false :boolean (float_frominteger state))
                (if (= 2 top-int)
                  (push-item true :boolean (float_frominteger state))
            (float_frominteger state)))))))

;Right move 1 from float to integer
(define-registered
  right_move
  ^{:stack-types [:integer :float :boolean]}
  (fn [state]
    (if (empty? (:float state))
        state
        (let [top-float (top-item :float state)
              ]
          (if (= 1.0 top-float)
                (push-item false :boolean (integer_fromfloat state))
                (if (= 2.0 top-float)
                  (push-item true :boolean (integer_fromfloat state))
                  (integer_fromfloat state)))))))

(pushgp argmap)