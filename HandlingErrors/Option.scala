import scala.{Option => _, Either => _, _}
sealed trait Option[+A]{

	def map[B](f:A=>B):Option[B]= this match {
		case None => None
		case Some(a) => Some( f(a) )
	}
	def flatMap[B](f:A=>Option[B]):Option[B] = this match{
		case None => None
		case Some(a) => f(a)
	}
	//B >: A means B must be supertype of A
	// default: => B: means the argument will not be evaluated until it is needed by the function
	def getOrElse[B >: A] (default: => B): B = this match{
		case None => default
		case Some(a) => a
	}
	def flatMap_1[B](f:A=>Option[B]):Option[B] = map(f) getOrElse None
	def orElse[B >: A](ob: => Option[B]) :Option[B] = this map (Some(_)) getOrElse ob
	def orElse_1[B >: A](ob: => Option[B]) :Option[B] = this match {
		case None => ob
		case _ => this
	}
	def filter(f: A => Boolean):Option[A] = this match{
		case None => None
		case Some(a) => if (f(a)) this else None
	}
	def filter_1(f: A => Boolean):Option[A] = flatMap(a=>if (f(a)) Some(a) else None)


}
case class Some[+A](get:A) extends Option[A]
case object None extends Option[Nothing]


case class Employee(name:String, department:String)

val employeesByName: Map[String,Employee] =
	List(Employee("Alice","RD"),Employee("Bob","Accounting")).
	map( e=> (e.name,e) ).toMap

val dept: Option[String] =employeesByName.get("Joe").map(_.dept)
							.filter(_!="Accounting").getOrElse("Defaulte Dept")

def mean(xs:Seq[Double]):Option[Double] = {
	if (xs.isEmpty)  None
	else xs.sum/xs.length
}

def variance(xs: Seq[Double]):Option[Double] = {
	mean(xs) flatMap ( m=> mean(xs.map( x => math.pow(x-m,2) ) ) ) 
}



