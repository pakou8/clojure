(ns maths "pakou maths tools")

(defn sinc [x]
	(if (== x 0)
		1
		(/ (Math/sin x) x)))

(defn factorial [n]
	(loop [result 1 iter n]
		(if (< iter 1)
			result
			(recur (* iter result) (dec iter)))))

(defn permutation [n k]
	(if (> k n)
		0
		(loop [result 1 iter n]
			(if (<= iter (- n k))
				result
				(recur (* iter result) (dec iter))))))

(defn combination [n k]
	(if (> k n)
		0
		(/ (permutation n k) (factorial k))))

(defn mean [samples]
	(loop[sum 0 count 0 rsamples samples]
		(if (empty? rsamples)
			(/ sum count)
			(recur (+ sum (first rsamples)) (inc count) (rest rsamples)))))

(defn variance [samples]
	(let [fsq (fn [x] (* x x))
		samplesSq (map fsq samples)
		samplesMean (mean samples)]
		(- (mean samplesSq) (* samplesMean samplesMean))))

(defn deviation [samples]
	(Math/sqrt (variance samples)))

(defn derivative [function x epsilon]
	(/ (- (function (+ x epsilon)) (function x))
		epsilon))

(defn derivative2 [function x epsilon]
	(derivative
		(fn [y] (derivative function y epsilon))
		x
		epsilon))

(defn integral [function a b steps]
	(loop [sum 0 a0 a count steps]
		(if (< count 1)
			sum
			(let [a1 (+ a0 (/ (- b a0) count))
				middle (+ a0 (* (- a1 a0) 0.5))]
				(recur (+ sum (* (- a1 a0) (function middle)))
						a1
						(dec count))))))
						
(defn newton [function x steps epsilon]
	(loop [x1 x count steps]
		(if (< count 1)
			x1
			(recur (- x1 (/ (function x1) (derivative function x1 epsilon)))
				(dec count)))))

(defn- plot-lines [graphics xlist ylist]
	(loop [xl xlist yl ylist]
		(let [xr (rest xl) yr (rest yl)]
			(if (not (empty? xr))
				(do (.drawLine graphics (first xl) (first yl) (first xr) (first yr))
					(recur (rest xr) (rest yr)))))))

(defn- plot-line-strips [graphics xlist ylist]
	(loop [xl xlist yl ylist]
		(let [xr (rest xl) yr (rest yl)]
			(if (not (empty? xr))
				(do (.drawLine graphics (first xl) (first yl) (first xr) (first yr))
					(recur xr yr))))))

(defn plot [function xmin xmax ymin ymax steps name]
	(let [xcoord (range xmin xmax (/ (- xmax xmin) steps))
		ycoord (map function xcoord)
		xaxis (list xmin xmax 0 0)
		yaxis (list 0 0 ymin ymax)
		frame (new javax.swing.JFrame name)
		panel (proxy [javax.swing.JPanel] []
			(paintComponent [graphics]
				(let [width (.getWidth (.getSize this))
					height (.getHeight (.getSize this))
					fx (fn [x] (* (/ (- x xmin) (- xmax xmin)) width))
					fy (fn [x] (* (/ (- ymax x) (- ymax ymin)) height))
					xline (map fx xaxis)
					yline (map fy yaxis)
					xstrip (map fx xcoord)
					ystrip (map fy ycoord)]
					(do (.setColor graphics java.awt.Color/BLACK)
						(plot-lines graphics xline yline)
						(.setColor graphics java.awt.Color/RED)
						(plot-line-strips graphics xstrip ystrip)))))]
		(do (.setContentPane frame panel)
			(.setSize frame 400 400)
			(.setVisible frame true)
			frame)))