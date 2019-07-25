package rpca

import scala.math.{sin, Pi}

object Example {
	def main(args: Array[String])={
		val start = System.currentTimeMillis()/1000
		val f = 7
		val T = 10
		val sinusoidal = Range(1, T*f+1).map(x => sin(x*2*Pi/f)).toArray
		sinusoidal(57) = 100
		sinusoidal(58) = 100
		sinusoidal(59) = 100
		val X = Rpca.reshape(sinusoidal, f, T, "F")
//		val lpenalty = 1
//		val spenalty = 30 / sqrt(max(f, T))//val spenalty = 1.4 / sqrt(max(X.size, X.head.size))
//		val pca = new Rpca(maxIter = 5000)
//		val r = pca.fit(X, Spenalty=spenalty, debug=true)//, lpenalty, spenalty)
		val pca = new Rpca
		val r = pca.fit(X)
//		r.map{X => println("*"*100) ;Rpca.prettyPrint(X)}
		println(s"Time elapsed: ${System.currentTimeMillis()/1000.0 - start}")
	}
}
