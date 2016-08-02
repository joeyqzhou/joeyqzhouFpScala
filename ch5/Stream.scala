
import Stream._

trait Stream[+A] {
	//def uncons: Option[(A,Stream[A])]
	//def isEmpty: Boolean = uncons.isEmpty

	
 	def toList:List[A] = this match {
		case Cons(h,t) => h()::t().toList
		case _ => List()
	}

	def takeWhile(p: A=>Boolean):Stream[A] ={
		this match{
			case Cons(h,t) if p(h()) => cons(h(),t().takeWhile(p))
			case Cons(h,_) if p(h()) => cons(h(),Empty )
			case _ => Empty
		}
	}
	def take(n:Int): Stream[A] = {	
		this match{
			case Cons(h,t) if n>1 => cons(h(),t().take(n-1))
			case Cons(h,_) if n==1 => cons(h(),Empty)
			case _ => Empty	
		}
	}

	def foldRight[B](z: => B)(f:(A, =>B) => B):B={
		this match{
			case Cons(h,t) => f( h(), t().foldRight(z)(f) )
			case _ => z
		}
	}

	def exists(p: A=>Boolean): Boolean={
		foldRight(false)((a,b)=> p(a) || b)
	}

	def forAll(p: A=>Boolean): Boolean={
		foldRight(true)((a,b)=> p(a) && b)
	}
	
	def takeWhile1(p: A=>Boolean):Stream[A] = {
		foldRight(empty[A])((a,b)=> if(p(a)) cons(a,b) else empty)
	}

	def map[B](f: A=>B):Stream[B] =
		foldRight(empty[B])( (a,b)=> cons( f(a), b ) )

	def filter(f: A=>Boolean):Stream[A]=
		foldRight(empty[A])( (a,b) => if (f(a)) cons(a,b) else b )
	
	// error: covariant type A occurs in contravariant position in type A of value elem	
	/*def append_one(elem:A):Stream[A]=
		foldRight(cons(elem,empty[A]))( (h,t)=> cons(h,t) ) */

	def append[B>:A](s:Stream[B]):Stream[B]=
		foldRight(s)((h,t)=>cons(h,t))
		

	def flatMap[B](f: A=>Stream[B]):Stream[B] =
		foldRight(empty[B])( (h,t)=> f(h) append t )
	
	def mapViaUnfold[B](f:A=>B):Stream[B] = 
		Stream.unfold(this){
			case Cons(h,t) => Some(f(h()),t())
			case _ => None
		}

	def takeViaUnfold(n:Int):Stream[A] =
		unfold(this,n){
			case (Cons(h,t),n) if(n>0) => Some(h(),(t(),n-1))
			case _ => None
		}

	def takeWhileViaUnfold(f:A=>Boolean):Stream[A] =
		unfold(this){
			case Cons(h,t) if(f(h())) => Some(h(),t())
			case _ => None
		}
	
	def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = {
		unfold(this,s2){	
			case (Cons(h1,t1),Cons(h2,t2)) => Some( (f(h1(),h2()), (t1(),t2()) ) ) 
			case _ => None
		}
	}

	def zip[B](s2:Stream[B]):Stream[(A,B)] = {
		zipWith(s2)( (_,_) )
	}

	def zipAll[B](s2:Stream[B]):Stream[(Option[A],Option[B])] = {
		zipWithAll(s2)( (_,_) )
	}


  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) , (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) , (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) , (t1() -> t2()))
    }
	//def hasSubSequence(s:Stream[A]):Boolean = {
	//}

	def startsWith[A](s:Stream[A]):Boolean ={
		this.zipAll(s).takeWhile(!_._2.isEmpty).forAll{
			case(h,h2) => h == h2	
		}
	}

	def tails:Stream[Stream[A]] = {
		unfold(this){
			case Cons(h,t) => Some(( cons(h(),t())  ,t()))
			case _ => None
		}
	}

	override def toString:String ={
		this.foldRight("")( (a,str)=> a + "_" + str  )
	}
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def empty[A]:Stream[A] = Empty
 	//@
	override def toString:String = "aaa"	
	def cons[A](hd: => A,tl: =>Stream[A]):Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons( ()=> head, ()=> tail)
	}

	def apply[A] (as: A*): Stream[A] = {
		if (as.isEmpty) Empty
		else cons(as.head, apply(as.tail: _*))
	}

	def constant[A](a:A):Stream[A] = {
		lazy val const:Stream[A] = Stream.cons(a,const)
		const
	}

	def from(n:Int): Stream[Int] = cons(n,from(n+1))
	
	def fibs:Stream[Int] = {
		def go(f0:Int,f1:Int):Stream[Int]=
			cons(f0,go(f1,f0+f1))
		go(0,1)
	}


	//z:initial state, (A,S):A:next value, S:next state
	def unfold[A,S](z:S)(f:S=>Option[(A,S)]):Stream[A] = {
		f(z) match {
			case Some((h,t)) => cons(h, unfold(t)(f))
			case None  => empty
		}	
	}

	def fibs1:Stream[Int] = unfold((0,1)){ case(f0,f1)=>Some(f0,(f1,f0+f1)) }
	def from1(n:Int):Stream[Int] = unfold(n)( s=>Some(s,s+1))
	//some[+A](get:A) extends Option[A]	
	def constant1[A](elem:A):Stream[A] = unfold(cons(elem,empty))( s=>Some(elem,cons(elem,s) ) )
	def constant2[A](elem:A):Stream[A] = unfold(elem)(_=>Some(elem,elem))
	



	def test(){
		println("Hello FP")
		println( Stream(1,2,3).toList)
		println( Stream(1,2,3).tails.toList)
		lazy val ones:Stream[Int] = Stream.cons(1,ones)
		println(ones.take(10).toList)
		println(ones.exists(_ %2 !=0))
		//println(ones.takeWhile( _==1 ))
		//println(ones.takeWhileViaUnfold( _==1 ))
		println(ones.forAll(_ != 1) )
		println(constant(3).take(5).toList)
		println(constant1(3).take(5).toList)
		println(constant2(3).take(5).toList)
		println( from(98).take(20).toList )
		println( from1(98).take(20).toList )
		println(fibs.take(30).toList )
		println(fibs1.take(20).toList)
		println(constant(3).map(a=>a*2).take(10).toList)
		println(constant(3).mapViaUnfold(a=>a*2).takeViaUnfold(10).toList)	
	}
}

Stream.test()
