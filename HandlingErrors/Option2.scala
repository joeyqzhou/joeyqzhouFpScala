sealed trait Option[+A]
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
	def map[B](f:A=>B):Option[B]= this match {
		case None => None
		case Some(a) => Some(f(a))
	}

	def getOrElse[B>:A](default:=>B):B= this match {
		case  None=> default
		case Some(a) => a
	}
	def flatMap1[B](f:A=>Option[B]):Option[B] = map[f] getOrElse None

	def flatMap[B](f:A=>Option[B]):Option[B] = this match{
		case None => None
		case Some(a) => f(a)
	}

	def orElse[B>:A](ob: => Option[B]):Option[B] = this map (Some(_)) getOrElse ob

	def orElse_1[B>:A](ob: => Option[B]):Option[B] = this match {
    case Some(a) if f(a) => this
    case _ => None
  	}

	def filter(f:A=>Boolean):Option[A] = this match {
		case Some(a) if f(a) => this
		case _ => None
	}
	def filter1(f:A=>Boolean):Option[A] = this flatMap( a=> if(f(a)) Some(a) else None)
	
	def mean(sq:Seq[Double]):Option[Double] = {
		if(sq.Empty) None
		else Some(sq.sum/sq.length)
	}

	def variance(sq:Seq[Double]):Option[Double] = {
		val t_mean = mean(sq)
		t_mean match {
			case None => None
			case Some(a) => mean( sq.map(x=>math.pow(x-t_mean,2)) )
		}
	}


	def map2[A,B,C](a: Option[A],b:Option[B])(f:(A,B) => C):Option[C] ={
		a flatMap ( ta=> b map( tb=>f(ta,tb )  ) )
	}

	def bothMatch_2(pat1:String,pat2:String,s:String):Option[Boolean]={
		map2( mkMatcher(pat1)(s), mkMatcher(pat2)(s) ) ( (a,b)=>a && b )
	}

	def sequence[A](a:List[Option[A]]):Option[List[A]]={
		leftRold(a,None)
	}

}
