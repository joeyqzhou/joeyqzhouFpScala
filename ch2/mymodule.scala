object MyModule{

	//mark
	def partiall[A,B,C](a:A,f:(A,B)=>C):B=>C ={
		b =>f(a,b)
	}

	def uncurry[A,B,C](f: A => B => C): (A, B) => C =
		(a,b) => f(a)(b)
	def binarySearch[A](as:Array[A],key:A,gt:(A,A)=>Boolean):Int={
		@annotation.tailrec
		def go(low:Int,high:Int):Int={
			if(low>high) -1
			else{
				val mid2 = (low+high)/2
				if( as(mid2)==key ) mid2
				else if( gt(as(mid2),key) ) go(low,mid2-1)
				else go(mid2+1,high)
			}
		}
		go(0,as.length-1)
	}

	def isSorted[A](as:Array[A],sort:(A,A)=>Boolean):Boolean={
		@annotation.tailrec
		val len = as.length
		def go(curIdx:Int):Boolean={
			if(curIdx >= len -1 ) true
			else if( ! sort(as(curIdx),as(curIdx+1)) ) false
			else go(curIdx+1)
		}
		go(0)
	}


	//@annotation.tailrec
	def fib(n:Int):Int={
	/*	def go(n:Int,acc:Int):Int={
			if(n<=0) acc
			else go(n-1,go(n-1,0)+go(n-2,0))
		}*/
		if(n<=0) 0
		else if(n==1) 1
		else fib(n-1)+fib(n-2) 
	}
	def factorial(n:Int):Int={
		if(n<=1) 1
		else n*factorial(n-1)
	}
	def factorial2(n:Int):Int={
		@annotation.tailrec
		def go(n:Int, acc:Int):Int={
			if (n<=1) acc
			else go(n-1,n*acc)
		}
		go(n,1)
	}
	
	def main(args:Array[String]){
		def testIsSorted():Unit={
			val as1 = Array(1,2,3,5,7,8,13)
			val as2 = Array(1,2,3,5,15.0,8,13)
			println(as1.mkString(","))
			println("isSorted" + isSorted[Int](as1,(a:Int,b:Int)=>a<b))
			println("isSorted" + isSorted[Double](as2,(a:Double,b:Double)=>a<b))
		}
		def testBinarySearch():Unit={
			val as = Array(1,2,3,5,7,8,13)
			println("as: " + as.mkString(","))
			println("8: " + binarySearch[Int](as,8,(a:Int,b:Int)=>a>b))
			println("13: " + binarySearch[Int](as,13,(a:Int,b:Int)=>a>b))
			println("4: " + binarySearch[Int](as,4,(a:Int,b:Int)=>a>b))
			
		}
		testIsSorted()
		testBinarySearch()
		println("Hello world")
		println(factorial(5))
		println(factorial2(5))
		println(fib(7))
		println(fib(1))
		println(fib(0))
	}
}
