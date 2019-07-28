class Chronometer(lap:Int){
	val window = if(lap > 0) lap*1000L else 1000L
	val minute = 60000L //para comecar sempre do segundo 0 quando conta acima de 60s
	var stop = 0L
	reset()

	def reset()={
		stop = System.currentTimeMillis + window
		stop = stop - (if (window >= minute) stop % minute else 0)
	}

	def overlap = {
		if (System.currentTimeMillis < stop) false
		else{
			while(System.currentTimeMillis >= stop) stop += window
			true
		}
	}
}
