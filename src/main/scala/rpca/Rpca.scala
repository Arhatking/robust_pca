package rpca

import org.apache.commons.math3.stat.StatUtils.{mean, variance}
import org.ojalgo.array.Array1D
import org.ojalgo.matrix.decomposition.SingularValue
import org.ojalgo.matrix.store.{MatrixStore, PrimitiveDenseStore}
import math.{abs, max, pow, sqrt}

class Rpca(tolcoef:Double=1e-8, maxIter:Double=1000, scaled:Boolean=false, forcediff:Boolean=false, autodiff:Boolean=false) {
	def fit(Xo:Array[Array[Double]], Lpenalty:Double= -1, Spenalty:Double= -1, debug:Boolean=false) = {
		val start = System.currentTimeMillis()/1000
		val (m, n) = (Xo.length, Xo.head.length)
		val vo = if(!scaled || forcediff || autodiff) Rpca.flatten(Xo, "F") else null

		val v1 = if(forcediff){
			val dickeyFullerTest = new AugmentedDickeyFuller(vo)
			dickeyFullerTest.getZeroPaddedDiff
		}
		else if (autodiff){
			val dickeyFullerTest = new AugmentedDickeyFuller(vo)
			if (dickeyFullerTest.isNeedsDiff) dickeyFullerTest.getZeroPaddedDiff else vo
		}
		else vo

		val (v2, mi, si) = if (!scaled) {// se std == 0 entao nao precisa de nada disso
			val mi = mean(v1)
			val si = sqrt(variance(v1))
			(v1.map(x => (x - mi) / si), mi, si)
		} else (v1, 0.0, 1.0)

		val X2 = if (vo != null) Rpca.matFact.rows(Rpca.reshape(v2, m, n, "F"): _*) else Rpca.matFact.rows(Xo: _*)
		val lpenalty = if (Lpenalty != -1) Lpenalty else 1
		val spenalty = if (Spenalty != -1) Spenalty else 1.4 / sqrt(max(m, n))
		val xlse = Rpca.decompose(X2, tolcoef, maxIter, lpenalty, spenalty, m, n, debug)
		if (debug) println(s"Tempo de execucao: ${System.currentTimeMillis()/1000.0 - start}s")
		if (!scaled) xlse.slice(0,2).map(Rpca.unscale(_, mi, si)) ++ xlse.slice(2,4).map(_.multiply(si)) else xlse
	}
}

object Rpca{
	private def decompose(X:PrimitiveDenseStore, tolcoef:Double, maxIter:Double, lpenalty:Double, spenalty:Double, m:Int, n:Int, debug:Boolean)={
		var objPrev = 0.5 * squaredNorm(X)
		val tol = tolcoef * objPrev
		var diff = 2 * tol
		var mu = m * n / (4 * lpNorm1(X))
		var iter = 0
		var converged = false

		var L: MatrixStore[java.lang.Double] = matFact.makeZero(m, n)
		var S: MatrixStore[java.lang.Double] = null
		var E: MatrixStore[java.lang.Double] = null
		while (diff > tol && iter < maxIter) {
			val sr = Rpca.getS(X, L, mu, spenalty)
			S = sr._1
			val lr = Rpca.getL(X, S, mu, lpenalty)
			L = lr._1
			val er = Rpca.getE(X, L, S)
			E = er._1
			val obj = Rpca.getObjective(lr._2, sr._2, er._2)
			//println(s"Objective function: $objPrev on previous iteration " + iter )
			//println(s"Objective function: $obj on iteration " + (iter - 1))
			diff = abs(objPrev - obj)
			objPrev = obj
			mu = Rpca.getDynamicMu(E)
			iter += 1
			if (diff < tol) converged = true
		}
		if (debug) println(s"Convergencia: $converged, Iteracoes: $iter")
		Array(X, L, S, E)
	}

	private val matFact = PrimitiveDenseStore.FACTORY

	private def lpNorm1(X: MatrixStore[java.lang.Double]) = X.toRawCopy1D.reduce(abs(_) + abs(_))

	private def squaredNorm(X: MatrixStore[java.lang.Double]) = pow(X.norm, 2)

	private def getS(X:PrimitiveDenseStore, L:MatrixStore[java.lang.Double], mu:Double, spenalty:Double)= {
		val spenalty2 = spenalty * mu
		val S = Rpca.softThreshold(X subtract L, spenalty2)
		(S, spenalty2 * Rpca.lpNorm1(S))
	}

	private def getL(X:PrimitiveDenseStore, S:MatrixStore[java.lang.Double], mu:Double, lpenalty:Double) = {
		val lpenalty2 = lpenalty * mu
		val P = X subtract S
		val udv = SingularValue.make(P)
		udv decompose P
		val ds = Rpca.softThresholdDiagonal(udv.getSingularValues, lpenalty2)
		val penalizedD = Rpca.matFact.rows(reshape(ds, order="D"):_*)
		val L = udv.getQ1 multiply penalizedD multiply udv.getQ2.transpose
		(L, ds.sum * lpenalty2)
	}

	private def getE(X:PrimitiveDenseStore, L:MatrixStore[java.lang.Double], S:MatrixStore[java.lang.Double]) = {
		val E = X subtract L subtract S
		(E, Rpca.squaredNorm(E))
	}


	private def softThreshold(A: MatrixStore[java.lang.Double], penalty:Double) = {
		//val soft = Y.toRawCopy2D.map(_.map(x => signum(x) * max(abs(x) - penalty, 0)))
		//val soft = Y.toRawCopy2D.map(_.map(x => if (x != 0) signum(x) * max(abs(x) - penalty, 0) else 0))
		val soft = A.toRawCopy2D.map(_.map{x =>
			val penalized = abs(x) - penalty
			if (penalized < 0) 0
			else if (x > 0 ) penalized
			else -penalized
		})
		Rpca.matFact.rows(soft:_*)
	}

	private def softThresholdDiagonal(u:Array1D[java.lang.Double], penalty:Double) = {
		u.toRawCopy1D.map{x =>
			val penalized = x - penalty
			if (penalized > 0) penalized else 0
		}
	}

	private def getObjective(nuclearNorm:Double, l1Norm:Double, l2Norm:Double) = 0.5 * l2Norm + nuclearNorm + l1Norm

	private def getDynamicMu(E:MatrixStore[java.lang.Double]) = {
		val (m, n) = (E.countRows, E.countColumns)
		val esd = sqrt(variance(E.toRawCopy1D))
		val mu = esd * sqrt(2 * max(m,n))
		max(mu, .01)
	}

	def reshape(x:Array[Double], r:Int=0, c:Int=0, order:String="C") = {
		val vec = x.iterator
		order match {
			case "C" =>
				val A = Array.ofDim[Double](r, c)
				for (i <- 0 until r; j <- 0 until c) A(i)(j) = vec.next
				A
			case "F" =>
				val A = Array.ofDim[Double](r, c)
				for (j <- 0 until c; i <- 0 until r) A(i)(j) = vec.next
				A
			case "D" =>
				val m = x.length
				val A = Array.ofDim[Double](m, m)
				for(i <- 0 until m) A(i)(i) = vec.next
				A
		}
	}

	def flatten(X:Array[Array[Double]], order:String) = {
		order match {
			case "C" => X.flatten
			case "F" => X.transpose.flatten
		}
	}

	private def unscale(A:MatrixStore[java.lang.Double], mi:Double, si:Double) = {
		val unsc = A.toRawCopy2D.map(_.map(x => x * si + mi))
		matFact.rows(unsc:_*)
	}

	def prettyPrint(A:MatrixStore[java.lang.Double]) = A.toRawCopy2D.foreach(x => println(x.mkString(",\t")))
	def prettyPrint(A:Array[Array[Double]]) = A.foreach(x => println(x.mkString(",\t")))
}
