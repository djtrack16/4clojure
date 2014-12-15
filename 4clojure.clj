;; SELECTED SOLUTIONS TO 4CLOJURE.COM PROBLEMS

;; Format:
;; Problem Description
;; Problem (if necessary)
;; Solution


;;15. Write a function that doubles a number
#(* % 2)

;;16. Write a function that returns a personalized greeting
#(str "Hello, "  %  "!")

;;17. Learn how to apply the map function
(map #(+ % 5) '(1 2 3))
'(6 7 8)

;;18. Learn how to apply filter
(filter #(> % 5) '(3 4 5 6 7))
'(6 7)

;;19. Write a function which returns the last element in sequence
(comp first reverse) 
;; or ;; 
(last %)

;;20. Write a function which returns the last element in sequence
(comp second reverse)

;;21. Return the nth element from a sequence (without using nth)
(fn [coll n] (first (drop n coll)))

;;22. Return total number of elements in a sequence (without count)
#(alength (to-array %))

;;23 reverse a sequence (without using reverse)
#(reduce conj () %)

;;24 find the sum of a sequence of numbers
#(reduce + %)

;;25 return only odd numbers from sequence
filter odd?

;;26. first X fibonacci numbers
; gets nth fibonacci number
(def fib
	(memoize
		(fn [n]
			(if (< n 2)
				1
			(+ (fib (dec (dec n)))
			   (fib (dec n))
			))
		)
	)
)
; returns first X fib numbers
(take x (map fib (iterate inc 0)))

;;27 returns true if sequence is palindrome
#(= (seq %) (reverse (seq %)))

;;28 implement flatten
#(remove coll? (tree-seq coll? seq %))

;;29 returns a string containing only capitalized letters
#(.replaceAll % "[^A-Z]" "")

;;30 removes consecutive duplicates from a sequence
#(mapcat set (partition-by identity %))

;;31 packs consecutive duplicates into sub-lists
#(partition-by identity %)

;;32 duplicates each element of a sequence
#(interleave % %)

;;33 replicates each element of a sequence a variable number of times
(fn [n coll] (mapcat #(repeat n %) coll))

;;34 implements range function
(fn [a b] (take (- b a) (iterate inc a)))

;;39 implement interleave
#(mapcat list % %)

;;40 implement interpose
(fn [value coll] (butlast (interleave coll (repeat value))))

;;41 drop every nth item from a sequence
(def rm [n v]
	;;(let [p (partition-all n v)]
	;;	(let [rem (last p)]
	;;		(let [ans (mapcat butlast p)]
	;;			(if (zero? (mod (count v) n))
	;;				ans
	;;			(concat ans rem))

	;;	))
	;;)
	;; above also works, but below is shorter
	(mapcat 
		(partial take (dec n))
		(partition-all n v)
	)
)

;;42 find the factorial 
#(reduce * (range 1 (inc %)))

;;43, reverse interleave process into x number of subsequences
(fn [v x]
	(let [sz (/ (count v) x)]
		(partition-all sz 
			(apply interleave (partition-all x v))
		)
	)
)
