//package fpinscala.datastructures

sealed trait List[+A]
//trait is an abstract interface that may optionally contain implementationsof some methods
//sealed means that all implementations of our trait must be declared in this file

case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]
//just as functions can be polymorphic
//data types can be as well



object List{
	def sum(ints:List[Int]): Int = ints match{
		case Nil => 0
		case Cons(x,xs) => x + sum(xs)
	}
	def sum2(l:List[Int]):Int = foldRight(l,0)(_ + _) //type inference
	def product2(l:List[Int]):Double = foldRight(l,1.0)(_ * _) //type inference
	
	def product(ds:List[Double]): Double = ds match{
        case Nil => 1.0
        case Cons(x,xs) => x * product(xs)
	}
    
	def foldRight[A,B](l:List[A], z:B)(f:(A,B)=>B):B={
		l match{
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs,z)(f))
		}
	}
	//def reverse[A](l:List[A]):List[A] = l match {
	//	case Nil=> List[A]()
	//	case Cons(h,t) => Cons(reverse(t),Cons(List[A](),h))
	//}
	def reverse[A](l:List[A]):List[A] = {
		foldLeft(l,List[A]())( (b,a)=>Cons(a,b) )
	}


    def apply[A](as:A*):List[A] = {
		if ( as.isEmpty ) Nil
		else Cons(as.head, apply(as.tail:_*))
	}
   
	def tail[A](l:List[A]):List[A] = {
		l match{
		case Nil => sys.error("empty List")
		case Cons(_,t) => t}
	} 
	def drop[A](l:List[A],n:Int):List[A] = {
		n match{
			case 0 => l
			case _ => {
					l match{
						case Nil => sys.error("List is smaller than n")
						case Cons(_,t) => drop(t,n-1)}
					}
		}
	}
	
	def top[A](l:List[A]):A = {
		l match{
		case Nil => sys.error("Empty List")
		case Cons(t,_) => t}
	}
	def dropWhile[A](l:List[A],f:A=>Boolean):List[A] = {
      l match{
		case Cons(h,t) if f(h) => dropWhile(t,f)
		case _ =>l
		}
	}
	def setHead[A](l:List[A],newHead:A):List[A] = {
		l match {
		case Nil => sys.error("List is empty")
		case Cons(h,t) => Cons(newHead,t)}
	}

    def init[A](l: List[A]):List[A] = {
		l match{
			case Cons(h,Nil) => Nil
			case Cons(h,t) => Cons(h,init(t))
			case _ => Nil
		}
	} 
	def length[A](l:List[A]):Int={
		foldRight(l,0)( (a,b)=> 1 + b)
	}

	def foldLeft[A,B](l:List[A],z:B)(f:(B,A)=>B):B={
		l match{
			case Nil => z
			case Cons(h,t) => foldLeft(t,f(z,h))(f)
		}
	}
	def sum3(l:List[Int]):Int = foldLeft(l,0)(_ + _)
	def product3(l:List[Double]):Double = foldLeft(l,1.0)(_ * _)
    def length2[A](l:List[A]):Int = foldLeft(l,0) ( (a,b) => ( a + 1) )
    def addOne(l:List[Int]):List[Int]={
		l match{
			case Nil=>Nil
			case Cons(h,t)=>Cons(h+1,addOne(t))
		}
	}
	def addOne1(l:List[Int]):List[Int]={
		foldRight(l,Nil:List[Int])((a,b)=>Cons(a+1,b))
	}

	def allToString[A](l:List[A]):List[String]={
		foldRight(l,Nil:List[String])( (h,t)=>Cons( h.toString, t ) )
	}

	def map[A,B](l:List[A])(f:A=>B):List[B]={
		foldRight(l,Nil:List[B])( (h,t)=>Cons(f(h),t) )
	}
	
	def filter[A](l:List[A])(f:A=>Boolean):List[A]={
		foldRight(l,Nil:List[A]) ( (h,t)=> if (f(h)) Cons(h,t) else t )
	}
    def concatenate[A](hl:List[A],tl:List[A]):List[A]={
		 foldRight(hl,tl) ( (h,t) => Cons(h,t) )
	}
	def flatMap[A,B](l:List[A])(f:A=>List[B]):List[B]={
		foldRight(l,Nil:List[B])( (h,t)=> concatenate(f(h),t) ) //????How to Construct( List[B], List[B])
	}

	def filter2[A](l:List[A])(f:A=>Boolean):List[A]={
		flatMap(l)(e=> if( f(e) ) Cons(e,Nil) else Nil:List[A])
	}
	def addPairWise[A](la:List[A],lb:List[A])(f:(A,A)=>A):List[A]={
		(la,lb) match{
			case (la,Nil) => la
			case (Nil,lb) => lb
			case (Cons(ha,ta),Cons(hb,tb)) => Cons( f(ha,hb),addPairWise(ta,tb)(f) )	
		}
	}
	//不能区分连续还是不连续
	def hasSubsequence[A](l:List[A], sub:List[A]):Boolean = {
		(l,sub) match{
			case (l,Nil) => true
			case (Nil,sub) => false
			case (Cons(lh,lt),Cons(subh,subt)) => {
					if (lh==subh) 
						hasSubsequence(lt,subt) || hasSubsequence(lt,Cons(subh,subt))
					else
						hasSubsequence(lt,sub)  
				}
		}
	}
	//override def toString():String
    def sth() {
	
	println("Hello world")
    val example = Cons(-2, Cons(1, Cons(2, Cons(3,Nil) ) ) )
	val example4 = List(1,2,3)
	val example3 = Cons(4.0,Cons(5.0,Nil) )
	val example2 = List(1.0,2.0,3.0)
	val test1 = List(-2,2,3)
	val test2 = List(-1,3,2)
	println( hasSubsequence(example,example4) )
	println( hasSubsequence(example,test1 ) )
	println( hasSubsequence(example,test2) )
	println( hasSubsequence(example,List(2,3)) )
	println( example )
	println( example4 )
	println ("add pair wise: ", addPairWise(example,example4)(_+_) )
	println ( concatenate(example3,example2) )
	println( reverse(example) )
	println( "flatMap:" + flatMap(example) (a=> List(a,a-1) ) )
	println( "filter: " +  filter(example)( a => a%2 ==0 ))
	println( "filter: " +  filter2(example)( a => a%2 ==0 ))
	println( map(example)(a=>a * 2) )
	println( dropWhile(example, (x:Int)=> x<1.5  )  )
	println( setHead( example,10:Int) )
	val total = sum( example )
	val prod = product(example2)
	println( List(1,2,3) match{ case _ => 42} )
	val b = List(1,2,3) match{ 
		case Cons(_,t) => t
		case Nil => 0} 
	println(b)
    println(total)
	println("total2:" + sum2(example))
	println(prod)
	 println("product2:" + product2(example))
	println(example.toString)
	println(example.tail)
	println( drop(example,1) )
	println( drop(example,2) )
    println( drop(example,3) )
	println( init(example) )
	println( foldRight(List(1,2,3),Nil:List[Int])(Cons(_,_)) )
	println( length(example) )
	println( addOne(example) )
	println( addOne1(example) )
	println( length(example2) )
	println( length2(example2))
	//println( reverse(example2) )

	println( allToString(example) )
	println( allToString(example2) )
	}
}
List.sth()


