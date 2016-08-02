trait Either[+E,+A] {
	
	def map[B](f:A=>B):Either[E,B] = {
		this match {
			case Right(a) => Right(f(a))
			case Left(e) => Left(e)
		}	
	}

	//why github write Left on top
	def flatMap[EE>:E,B](f:A=>Either[EE,B]):Either[EE,B]={
		this match {
			case Right(a) => f(a)
			case Left(e) => Left(e)
		}
	}

	def orElse[EE>:E,B>:A] (b:Either[EE,B]):Either[EE,B]={
		this match{
			case Left(_) => b
			case Right(a) => Right(a)
		}
	}

	 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C]  ={
		this flatMap ( aa=> b map (bb=>f(aa,bb)) )
		//what is the difference
		//this flatMap ( aa=> (bb=>f(aa,b)) )
		// the above one bb is the type B, secnod one b is type Either not B
	 }
}

def mean(xs:Seq[Double]):Either[String,Double]={
	if(xs.Empty) Left("some error")
	else Right(xs.sum / xs.length)
}


