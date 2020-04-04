;; MOST SOLUTIONS TO 4CLOJURE.COM PROBLEMS

*;; get all permutations of a list/string (allow for duplicates to make it more interesting)
*;; check if preorder and postorder traversal are valid4cl
*;; given inorder and preorder build a BST
*;; is valid bst?
;; transitive dependencies (from codekata.com site, did this q in python)
;; change infix expr to postfix expr
;; evaluate postfix expr using stack

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
last

;;20. Write a function which returns the last element in sequence
butlast

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
(fn fib [n]
	(if (< n 2)
		1 
		(+
			(fib (dec (dec n)))
			(fib (dec n)))))))

(def memo-fib (memoize fib))

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
(fn [value coll]
	(butlast
		(interleave
			coll
			(repeat value))))

;;41 drop every nth item from a sequence
(fn [n v]
  (->>
  	(partition-all (dec n) n v)
  	(mapcat identity)))

;;42 find the factorial 
#(reduce * (range 1 (inc %)))

;;43, reverse interleave process into x number of subsequences
(fn [v x]
	(let [sz (/ (count v) x)]
		(partition-all sz 
			(apply interleave (partition-all x v)))))

;; 44. Rotate Sequence in either direction (-n for opposite direction)
#(let [k (mod % (count %2))]
		(concat (drop k %2) (take k %2)))


;;45, iterate can produce infinite lazy sequence
(= __ (take 5 (iterate #(+ 3 %) 1)))
(1 4 7 10 13)

;;46 flip the order of arguments of an input function
(fn [f]
	(fn [x y & z]
		(apply f y x z)))

;; 49 split a sequence into two parts at index k 
(fn [a b] [(take a b) (drop a b)])

;; 50 split a seq of items with differing types into set of homogeneous sub-sequences
#(vals (group-by type %))

;; 51 sophisticated destructuring
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))

;; 53 find longest increasing subsequence in vector of integers.
;; length > 2 to qualify



;; 54. Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
;; cannot use "partition" or "partition-all"

(fn split [n coll]
	(if (>= (count coll) n)
		(cons
			(take n coll)
			(split n (drop n coll)))))

;; 55. return a map containing the number of occurrences of each distinct item in a sequence
;; i.e. write 'frequencies'
(fn [coll]
	(let [f (group-by identity coll)]
		(zipmap (keys f)
				(map count (vals f)))))

;;56 remove duplicates from a seq, preserving order of elements
;; i.e. write 'distinct'
(fn [coll]
	(if (= (type coll) clojure.lang.LazySeq)
		coll 
  		(keys (group-by identity coll))))

;;57 create function compositions
;; The parameter list should take an arbitary # of functions, and creates a function that applies them right to left
(fn [x y & z]
	(let [fns (map fn (x y & z))]
		(-> reverse fns)))

;;58 Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and
;; create a function applies them from right-to-left.
;; re-write "comp"
(fn [& funcs]
	(reduce
		(fn [f g]
			(fn [args]
				(f (apply g args))))
		funcs))

;;59 Take a set of functions and return a new function that takes a variable number of
;; arguments and returns a sequence containing the result of applying each function
;; left-to-right to the argument list.
;; i.e. rewrite "juxt"
(fn [& funcs]
	(fn [& args]
		(for [f funcs]
			(apply f args))))

;;60 Write a function which behaves like reduce, but returns each intermediate value of the
;; reduction. Your function must accept either two or three arguments, and the return sequence
;; must be lazy.

(fn _reductions
	([f s]
		(lazy-seq
			(_reductions
				f
				(first s)
				(rest s))))
	([f val s]
		(cons
			val
			(lazy-seq
				(when (seq s)
					(_reductions
						f
						(f val (first s))
						(rest s)))))))

;; 61. Given a vector of keys and a vector of values, construct a map from them
	;; i.e. write zipmap
#(apply
	merge
	(map hash-map %1 %2))

;; 62. Given a side-effect free function f and an initial value x,
;; write a function which returns an infinite lazy sequence of x,
;; (f x), (f (f x)), (f (f (f x))), etc.
;; i.e. impl 'iterate'
(fn [f y]
	(cons y 
		(lazy-seq
			(recur f (f y)))))

;; 63. Given a function f and a sequence s, write a function which returns a map.
;; The keys should be the values of f applied to each item in s.
;; The value at each key should be a vector of corresponding items in the order they appear in s.
;; i.e. implement 'group-by'
(fn [f coll]
	(zipmap
		(distinct (map f coll))
		(partition-by f (sort-by f coll))))

;; 65. Clojure has many sequence types, which act in subtly different ways. The core functions
;; typically convert them into a uniform "sequence" type and work with them that way, but it
;; can be important to understand the behavioral and performance differences so that you know
;; which kind is appropriate for your application. Write a function which takes a collection
;; and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list? - the point
;; is to poke at them and understand their behavior.
(fn [s]
 (let [b (empty s)]
  (cond
  	(= b {})  :map
	(= b #{}) :set
	(= b '()) (if (reversible? s) :vector :list))))
 

;; 66 find the greatest common divisor of two numbers
(fn [a b]
	(if (zero? b)
		a
		(recur b (mod a b))))

;; 67 first x primes
;; let's generate lazy-seq of sieve of erastothenes, then take x

(fn [n]
	(take n
		((fn sieve [s]
			(cons (first s)
				(lazy-seq
					(sieve
						(remove 
							#(zero? (mod % (first s)))
							(rest s))))))
		(iterate inc 2))))


;;68 grok loop and recur concepts
(= __
  (loop [x 5
		 result []]
	(if (> x 0)
	  (recur (dec x) (conj result (+ 2 x)))
	  result)))
;; answer [7 6 5 4 3]

;;69 Write a function which takes a function f and a variable number of maps.
;; Your function should return a map that consists of the rest of the maps conj-ed
;; onto the first. If a key occurs in more than one map, the mapping(s) from the
;; latter (left-to-right) should be combined with the mapping in the result by calling
;; (f val-in-result val-in-latter)
(fn [f & maps]
	(let[merge-kvs
			(fn [m [k v]]
				(if (contains? m k)
					(assoc m k (f (get m k) v))
					(assoc m k v)))
		merge-all
			(fn [m1 m2]
				(reduce merge-kvs m1 m2))]
	(reduce merge-all maps)))


;; 70. Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.
(fn [s] (sort-by #(.toLowerCase %)
  (clojure.string/split
   (clojure.string/replace s #".$" "") #" ")))


;; 71. The -> macro threads an expression x through a variable number of forms. Fill in blank:
(= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__))
   5)
;; answer: last


;; 72. The ->> macro threads an expression x through a variable number of forms.
;;First, x is inserted as the last item in the first form, making a list of it if it is not a list already.
;;Then the first form is inserted as the last item in the second form, making a list of that form if necessary. This process continues for all the forms.
(= (__ (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (__))
   11)
;; answer 'reduce +'

;;74 Given a string of comma separated integers, write a function which returns
;; a new comma separated string that only contains the numbers which are perfect squares.
(fn [nums]
	(apply
		str
		(interpose
			","
			(filter
				(fn [x]
					(let [n (read-string x)]
						(let [div (Math/sqrt n)]
							(== (* div div) n))))
				(clojure.string/split nums #",")))))

;;75 Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1. Euler's totient function
;; f(x) is defined as the number of positive integers less than x which are coprime to x. The
;; special case f(1) equals 1. Write a function which calculates Euler's totient function.
;; note: all prime numbers less than x will be a subset of this function.
(fn [n]
	(count
		(filter
			#((fn [x n]
				(if (zero? n)
					(= x 1)
					(recur n (mod x n))))
			% n)
			(range n))))


;76 The trampoline function takes a function f and a variable number of parameters. Trampoline
;; calls f with any parameters that were supplied. If f returns a function, trampoline calls that
;; function with no arguments. This is repeated, until the return value is not a function, and then
;; trampoline returns that non-function value. This is useful for implementing mutually recursive
;; algorithms in a way that won't consume the stack.
(= 11 (letfn
	 [(foo [x y] #(bar (conj x y) y))
	  (bar [x y] (if (> (last x) 10)
				   x
				   #(foo x (+ 2 y))))]
	 (trampoline foo [] 1)))

;77 Write a function which finds all the anagrams in a vector of words. A word x is an anagram of
;; word y if all the letters in x can be rearranged in a different order to form y. Your function
;; should return a set of sets, where each sub-set is a group of words which are anagrams of each
;; other. Each sub-set should have at least two words. Words without any anagrams should not be
;; included in the result.
(fn [words]
	(->>
		words
		(group-by #(sort %))
		(map set #(vals %))
		(filter #(> (count %) 1))
		(map set)
		set))

;;78 re-implement "trampoline" command (hint. type "source trampoline" in the repl)
(fn t
	[f & args]
	(if (fn? f)
		(t (apply f args))
		f))


;;80 A number is "perfect" if the sum of its divisors equal the number itself.
;; 6 is a perfect number because 1+2+3=6. Write a function which returns true
;; for perfect numbers and false otherwise.
(fn [n]
	(=
		(reduce
			+
			(filter
				#(zero? (mod n %))
				(range 1 n)))
		n))

;; 81. Find the intersection of two sets, without using clojure.set/intersection
#(set (filter %1 %2))

;;82 ;; incomplete/wrong!, should use levenshtein distance instead of hamming distance
(fn wordChainExists [words]
	(filter empty?
	(for [a words b words]
		(let [hammingDistance #(count
			(filter
				(fn [[x y]]
					(not= x y))
				(partition 2 (interleave %1 %2))))]
		(when (= 1 (hammingDistance a b)) [a b])))))

;; 83 given a variable number of booleans, return true if some params are true
;; but not all of them; otherwise, return false
#(= (count (into #{} %&)) 2)

;; 84 transitive closure of a binary relation
(fn transitive [s]
  (#(if (= % s) s (transitive %))
  	(set
  		(for [[w x] s [y z] s]
		  [w (if (= x y) z x)]))))




;; 85. get powerset of any set, size is 2**n
#(reduce
	(fn [subsets elem]
		(into subsets
			(for [x subsets]
				(conj x elem))))
	#{#{}}
	%)

;; 86 Happy numbers are positive integers that follow a particular formula: take each individual
;; digit, square it, and then sum the squares to get a new number. Repeat with the new number
;; and eventually, you might get to a number whose squared sum is 1. This is a happy number. An
;; unhappy number (or sad number) is one that loops endlessly. Write a function that determines
;; if a number is happy or not.

(fn happy? [n]
	(loop [n n path #{}]
		(if (= 1 n)
			true
			(let [sum
				(reduce
					+
					(map
						#(let [x (read-string (str %))] (* x x))
						(str n)))]
				(if (path sum) false (recur sum (conj path sum)))))))

;;88 symmetric difference of two sets
(fn [a b]
	(let [d clojure.set/difference]
		(clojure.set/union
			(d a b)
			(d b a))))

;; 89 Starting with a graph you must write a function that returns true if it is possible to make
;; a tour of the graph in which every edge is visited exactly once.
;; The graph is represented by a vector of tuples, where each tuple represents a single edge.
;; The rules are:
;; You can start at any node.
;; You must visit each edge exactly once.
;; All edges are undirected.
(fn _ [graph]
 (if (= 1 (count graph))
  true
 (reduce
  #(%1 %2)
  (for [[a b] graph [c d] graph]
   (if
	(and
	 (not= a b)
	 (not= c d)
	 (#{c d} b)
	 (not= [a b] [c d]))
	(_ (remove {[a b]} graph))
	false)))))

;;90 cartesian product
(fn [a b]
	(for [x a y b]
		[x y]))

;; 92 Roman numerals are easy to recognize, but not everyone knows all the rules
;; necessary to work with them. Write a function to parse a Roman-numeral string
;; and return the number it represents. You can assume that the input will be
;; well-formed, in upper-case, and follow the subtractive principle. You don't
;; need to handle any numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.
(fn [numeral]
	(let [values {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]))

;;93 Write a function which flattens any nested combination of sequential things
;; (lists, vectors, etc.), but maintains the lowest level sequential items.
; The result should be a sequence of sequences with only one level of nesting.
(fn p_fl [v]
	(if (every? sequential? v)
		(mapcat p_fl v)
		[v]))

;; 95 check whether or not the given sequence represents a binary tree
;; each node must have value, left child and right child (even if they are null)
;; example: '(:a (:b nil nil) nil) returns true
(fn b_tree? [t]
	(or (nil? t) 
		(and (coll? t)
			(= (count t) 3)
			(every? b_tree? (rest t)))))


;; 96 Let us define a binary tree as "symmetric"
;; if the left half of the tree is the mirror image
;; of the right half of the tree. Write a predicate
;; to determine whether or not a given binary tree is symmetric.
#(= %
		((fn mirror? [[self l r]]
			(if self
		'(self (mirror? r) (mirror? l))))
		%))

;; 97 compute the nth row of pascal's triangle
;; we also compute all rows (because we may need them for another problem)
#(letfn
	[(f [n] (reduce
						*
						(range 1 (inc n))))
	 (nCr [n r] (quot
	 							(f n)
	 							(* (f r) (f (- n r)))))]
		(for [r (range %)]
			(nCr (dec %) r)))

;; 98. A function f defined on a domain D induces an equivalence relation on D, as follows:
;; a is equivalent to b with respect to f if and only if (f a) is equal to (f b). Write a
;; function with arguments f and D that computes the equivalence classes of D with respect to f.
(fn [f D]
	(set
		(map
			set
			(vals (group-by f D)))))


;; 99 multiply two numbers and return sequence of their digits
(fn [x y]
	(loop [res (* x y) prod []]
		(if (zero? res)
			(vec prod)
			(recur (quot res 10) (concat [(mod res 10)] prod)))))

;; 100.Write a function which calculates the least common multiple.
;; Your function should accept a variable number of positive integers or ratios.
(fn [& nums]
	(let [gcd (fn [a b]
		(if (zero? b)
			a
			(recur b (mod a b))))]
		(reduce
			(fn [a b]
				(quot (* a b)
					(gcd a b)))
			nums)))

;; 101. Levenshtein Distance
(fn )

;; 102. When working with java, you often need to create an object with fieldsLikeThis,
;; but you'd rather work with a hashmap that has :keys-like-this until it's time to
;; convert. Write a function which takes lower-case hyphen-separated strings and converts
;; them to camel-case strings.
#(let [parts (clojure.string/split % #"-")]
(->>
	(rest parts)
	(map clojure.string/capitalize)
	(cons (first parts))
	(clojure.string/join)))


;;103. Given a sequence S consisting of n elements generate all k-combinations of S,
;; i. e. generate all possible sets consisting of k distinct elements taken from S.
;; The number of k-combinations for a sequence is equal to the binomial coefficient.
;; ans: simply filter the powerset (4clojure #85) and take only sets of size K elements
(fn [k s]
	(set (filter
			#(= k (count %))
			(reduce
				(fn [subsets elem]
					(into subsets
						(for [x subsets]
							(conj x elem))))
				#{#{}}
				s))))


;; 104. Given an integer, return the corresponding roman numeral in uppercase
;; Use the subtractive principle
(fn [n]
	(let [rnum {1 \I, 5 \V, 10 \X, 50 \L, 100 \C, 500 \D, 1000 \M}]
		(loop [n n pos 1 numeral []]
			(if (zero? n)
				(clojure.string/join numeral)
				(recur (quot n 10) )))))

;105. Given an input sequence of keywords and numbers, create a map such that each key in the map
;is a keyword, and the value is a sequence of all the numbers (if any) between it and the next
;keyword in the sequence.
(fn [kvs]
	(loop [[k & v] kvs pairs {}]
		(if (empty? k)
			pairs
			(let [[values leftover] (split-with number? v)]
				(recur leftover (assoc pairs k values))))))


;; 107. Lexical scope and first-class functions are two of the most basic
;; building blocks of a functional language like Clojure. When you combine
;; the two together, you get something very powerful called lexical closures.
;; With these, you can exercise a great deal of control over the lifetime of 
;; your local bindings, saving their values for use later, long after the code
;; you're running now has finished. It can be hard to follow in the abstract,
;; so let's build a simple closure. Given a positive integer n, return a function
;; (f x) which computes x^n. Observe that the effect of this is to preserve the
;; value of n for use outside the scope in which it is defined.
(fn [n]
	(fn [x]
		(reduce
			*
			(repeat n x))))

;; 108. Given any number of sequences, each sorted from smallest to largest, find
;; the smallest single number which appears in all of the sequences. The sequences
;; may be infinite, so be careful to search lazily.
(fn [& xs]
	((fn [s]
		(let [mins (map first s)
								maxmin (apply max mins)]
			(if (apply = mins)
				maxmin
				(recur
					(map #(if (= (first %) maxmin) % (rest %)) s)))))
		xs))

;; 110. Write a function that returns a lazy sequence of "pronunciations" of a sequence of numbers.
;; A pronunciation of each element in the sequence consists of the number of repeating identical
;; numbers and the number itself. For example, [1 1] is pronounced as [2 1] ("two ones"), which
;; in turn is pronounced as [1 2 1 1] ("one two, one one"). Your function should accept an initial
;; sequence of numbers, and return an infinite lazy sequence of pronunciations, each element being
;; a pronunciation of the previous element.
(fn p [s]
	(let[pronounce (juxt count first)
		init (mapcat pronounce (partition-by identity s))]
		(cons init
			(lazy-seq (p init)))))

;; 112. Create a function which takes an integer and a nested collection of integers as arguments.
;; Analyze the elements of the input collection and return a sequence which maintains the nested
;; structure, and which includes all elements starting from the head whose sum is less than or
;; equal to the input integer.
(fn )

;;114. take-while is great for filtering sequences, but it limited: you can only
;; examine a single item of the sequence at a time. What if you need to keep track
;; of some state as you go over the sequence? Write a function which accepts an
;; integer n, a predicate p, and a sequence. It should return a lazy sequence of
;; items in the list up to, but not including, the nth item that satisfies the predicate.
(fn [n p s]
	(take
		(.indexOf
			%
			(nth
				(filter p s)
				(dec n)))
		s))

;;115 A balanced number is one whose component digits have the same sum on the left
;; and right halves of the number. Write a function which accepts an integer n, and
;; returns true iff n is balanced.
(fn [s]
	(let [digits (map (comp read-string str) (seq (str s)))
		  half (/ (count digits) 2)]
		(let [left (reduce + (take half digits))
			  right (reduce + (take half (reverse digits)))]
			  (= left right))))

;; 116. A balanced prime is a prime number which is also the mean of the primes directly before
;; and after it in the sequence of valid primes. Create a function which takes an integer n, and
;; returns true iff it is a balanced prime.
(fn [n]
	(let[prime (fn [n] (every? #(pos? (mod n %)) (range 2 (inc (Math/sqrt n)))))
		lo (last (filter prime (range n)))
		hi (first (filter prime (range (inc n) Integer/MAX_VALUE)))]
			(and (< 3 n) (prime n) (= n (/ (+ lo hi) 2)))))


;;118. Re-implement map
(fn mp [f s]
	(when (seq s)
		(cons
			(f (first s))
			(lazy-seq (mp f (rest s))))))



;; 120. Return the count of how many elements are smaller than the sum of their squared component digits.
;; For example: 10 is larger than 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
(fn [coll]
	(count
		(remove
			zero?
			(for [n coll]
				(let[ss (map #(* % %)
					(map #(Integer/parseInt %) (map str (str n))))]
					(if (< n (reduce + ss)) n 0 ))))))

;; 121. Given a mathematical formula in prefix notation, return a function that calculates the value of the
;; formula. The formula can contain nested calculations using the four basic mathematical operators, numeric
;; constants, and symbols representing variables. The returned function has to accept a single parameter
;; containing the map of variable names to their values.
;;incomplete
(fn ev [expr]
	(fn _ [values]
		(let [[f x & y :as expr]]
			(cond
				(fn? f) (f (_ x) (_ y))
				(values f) (values f)
				:else f))))

;;122. Convert a binary number in string form to its numerical value
(fn [binary]
	(reduce +
		(map-indexed
			(fn [exp bit]
				(if (zero? bit)
					0
					(reduce * (repeat exp 2))))
			(map #(-(int %) 48) (reverse (vec binary))))))

;;128 Given a string of two characters, one the suit and one the rank, generate a map
;; with the format {:suit suit :rank rank}
(fn [[suit rank]]
	{:suit
		({
			\H :heart
			\D :diamond
			\S :spades
			\C :club
		} suit)
	:rank
		((zipmap "23456789TJQKA" (range)) rank)
  	}
)

;; 130. Tree reparenting. Every node of a tree is connected to each of its children as well as
;; its parent. One can imagine grabbing one node of a tree and dragging it up to the root position,
;; leaving all connections intact. For example, below on the left is a binary tree. By pulling the
;; "c" node up to the root, we obtain the tree on the right. Note it is no longer binary as "c" had
;; three connections total -- two children and one parent. Each node is represented as a vector, which
;; always has at least one element giving the name of the node as a symbol. Subsequent items in the
;; vector represent the children of the node. Because the children are ordered it's important that
;; the tree you return keeps the children of each node in order and that the old parent node, if any,
;; is appended on the right. Your function will be given two args -- the name of the node that should
;; become the new root, and the tree to transform. 

(fn [r t]
	())

;; 131. Given a variable number of sets of integers, create a function which returns true
;; iff all of the sets have a non-empty subset with an equivalent summation.
;; subset sum problem is NP-COMPLETE/HARD, which is slightly inconvenient
;; algo: get powerset of each set, sum each subset in each powerset,
;; then return true if intersection of all of these sums is non-empty
;; taken powerset solution from p. 85
(fn [& sets]
	(let[powerset (fn [v] (loop [v v subsets #{#{}}]
					(if (empty? v)
						(disj subsets #{})
						(recur (rest v) (into subsets (map #(conj % (first v)) subsets))))))
		eachSum (fn [subs] (map #(reduce + %) subs))
		allSums (map #(set (eachSum (powerset %))) sets)] 
	(not (empty? (reduce clojure.set/intersection allSums)))))

;; 132. Write a function that takes a two-argument predicate, a value, and a collection;
;; and returns a new collection where the value is inserted between every two items that
;; satisfy the predicate.
(fn [f value coll]
  (mapcat
   (fn [[a b]]
	 (cons a (when (and a b (f a b)) [value])))
   (partition-all 2 1 coll)))

;; 134 given a key and map, return true iff map contains key and corresponding value is nil.
(fn [k dict] 
	(and
		(contains? dict k)
		(nil? (dict k))))

;;135 emulate infix calculator, don't worry about precedence and operators
(fn infix
	([x] x)
	([x op y & z] (apply infix (op x y) z)))

;;137. Write a function which returns a sequence of digits of a non-negative number (first argument)
;; in numerical system with an arbitrary base (second argument). Digits should be represented with
;; their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15] in base 16. 
(fn [n b]
	(if (zero? n)
		[0]
		(loop [n n digits []]
			(if (zero? n)
				(vec digits)
				(recur (quot n b) (concat [(mod n b)] digits))))))

;; 141. In trick-taking card games such as bridge, spades, or hearts, cards are played in groups known
;; as "tricks" - each player plays a single card, in order; the first player is said to "lead" to the
;; trick. After all players have played, one card is said to have "won" the trick. How the winner is
;; determined will vary by game, but generally the winner is the highest card played in the suit that
;; was led. Sometimes (again varying by game), a particular suit will be designated "trump", meaning
;; that its cards are more powerful than any others: if there is a trump suit, and any trumps are played,
;; then the highest trump wins regardless of what was led.
;; Your goal is to devise a function that can determine which of a number of cards has won a trick. You
;; should accept a trump suit, and return a function winner. Winner will be called on a sequence of cards,
;; and should return the one which wins the trick. Cards will be represented in the format returned by
;; Problem 128, Recognize Playing Cards: a hash-map of :suit and a numeric :rank. Cards with a larger rank
;; are stronger.
(fn [suit]
	(fn [cards]
		(let [trump #(filter (fn [c] (= % (c :suit)) cards))]
		(if suit
			(->> suit trump )
			))))
;; 143 dot product
(fn [x y]
	(apply
		+
		(map * x y)))

;; 144. Write an oscillating iterate: a function that takes an initial value and a variable number of
;; functions. It should return a lazy sequence of the functions applied to the value in order, restarting
;; from the first function after it hits the end.
(fn [x & funcs]
	(reductions
		(fn [a b]
			(b a))
		x
		(cycle funcs)))	


;; 145. the for macro
(= __ (for [x (iterate #(+ 4 %) 0)
			:let [z (inc x)]
			:while (< z 40)]
		z))

__ = (range 1 40 4)

;;146 Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
;; it is excellent for transforming all sorts of sequences. If you don't want a sequence as your
;; final output (say you want a map), you are often still best-off using for, because you can
;; produce a sequence and feed it into a map, for example. For this problem, your goal is to "flatten"
;; a map of hashmaps. Each key in your output map should be the "path"1 that you would have to take
;; in the original map to get to a value, so for example {1 {2 3}} should result in {[1 2] 3}.
;; You only need to flatten one level of maps: if one of the values is a map, just leave it alone.
#(into
	{}
	(for [[k v] m [k2 v2] v]
		[[k k2] v2]))

;; 147. given a random input vector of numbers, generate lazy infinite sequence,
;; as if the vector was a row in pascal's triangle

(fn pascal [nums]
	(cons nums
		(lazy-seq
			(pascal
				(vec(map + (conj nums 0) (cons 0 nums)))))))

;; 148 Write a function which calculates the sum of all natural numbers under n (first argument) which are
;; evenly divisible by at least one of a and b (second and third argument). Numbers a and b are guaranteed
;; to be coprimes.
(fn [n a b]
	(let [sum #(quot (* % (inc %)) 2)
		  mul #(* (sum (quot (dec n) %)) %)]
		  (-
		  	(+
		  		(mul a)
		  		(mul b))
		  	(mul (* a b)))))

;; 150. A palindromic number is a number that is the same when written forwards or backwards (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument, and returns an increasing lazy sequence of all
;; palindromic numbers that are not less than n. The most simple solution will exceed the time limit!


;;153. Pairwise disjoint sets. Given a set of sets, create a function which returns true
;; if no two of those sets have any elements in common and false otherwise.
#(->> % (apply concat) (apply distinct?))

;; 157 Transform a sequence into a sequence of pairs
;; containing the original elements along with their index.
#(map-indexed (fn [idx item] [item idx]) %)

;; 158. Write a function that accepts a curried function of unknown arity n.
;; Return an equivalent function of n arguments. 
(fn [f]
  #(reduce
  	(fn [f x] (f x))
  	f
  	%&))

;; 166 For any orderable data type it's possible to derive all of the basic
;; comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation
;; (any operator but = or ≠ will work). Write a function that takes three
;; arguments, a less than operator for the data and two items to compare.
(fn [f x y]
  (cond
  	(f x y) :lt
  	(f y x) :gt
  	:else :eq))

;; 171. Write a function that takes a sequence of integers and returns a sequence of
;; "intervals". Each interval is a a vector of two integers, start and end, such that
;; all integers between start and end (inclusive) are contained in the input sequence.
(fn [xs]
  (let [[x & xs] (sort v)]
	(if x
	  (reverse
	   (reduce
		(fn [[[a b] & r :as intervals] nxt]
		  (if (<= a nxt (+ 1 b))
			(conj r [a nxt])
			(conj intervals [nxt nxt])))
		[[x x]]
		xs))
	  v)))


;;173
(= 3
  (let [[__] [+ (range 3)]] (apply __))
  (let [[[__] b] [[+ 1] 2]] (__ b))
  (let [[__] [inc 2]] (__)))
;; ans
f x


		   
;;177 When parsing a snippet of code it's often a good idea to do a sanity check to see
;; if all the brackets match up. Write a function that takes in a string and returns truthy
;; if all square [ ] round ( ) and curly { } brackets are properly paired and legally nested,
;; or returns falsey otherwise.
(fn balanced? [input]
	(let[open #{\( \[ \{}
		close #{\) \] \}}
		match (zipmap close open)]
		(->> input
			(filter (clojure.set/union open close))
			(reduce
				(fn [b paren]
					(cond
						(open paren) (conj b paren)
						(= (match paren) (last b)) (pop b)
						:else (conj b paren)))
				[])
			(empty?))))

;; 195 In a family of languages like Lisp, having balanced parentheses is a defining feature of
;; the language. Luckily, Lisp has almost no syntax, except for these "delimiters" -- and that
;; hardly qualifies as "syntax", at least in any useful computer programming sense. It is not a
;; difficult exercise to find all the combinations of well-formed parentheses if we only have N
;; pairs to work with. For instance, if we only have 2 pairs, we only have two possible combinations:
;; "()()" and "(())". Any other combination of length 4 is ill-formed. Can you see why?
;; Generate all possible combinations of well-formed parentheses of length 2n (n pairs of parentheses).
;; For this problem, we only consider '(' and ')', but the answer is similar if you work with only {}
;; or only [].
;; There is an interesting pattern in the numbers!

(fn [n]
	(set
		(map
			#(apply str %)
			((fn parens [o c]
				(lazy-seq
					(concat
						(when (and (zero? o) (zero? c))
							[""])
						(when (pos? o)
							(map #(cons "(" %) (parens (dec o) (inc c))))
				 		(when (pos? c)
				 			(map #(cons ")" %) (parens o (dec c)))))))
			n 0))))

