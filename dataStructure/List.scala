sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail:List[A]) extends List[A]

object List{

	def apply[A](as: A*):List[A]={
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}
	def tail[A](l:List[A]):List[A]={
		l match{
			case  Nil => Nil
			case Cons(_,t) => t
		}
	}
	def drop[A](l:List[A],n:Int):List[A]={
		if(n<=0) l
		else drop(tail(l),n-1)
	}

	def dropWhile[A](l: List[A])(f:A=>Boolean): List[A] = {
		l match{
			case Cons(h,t) => if(f(h)) dropWhile(t)(f) else l
			case _ => Nil
		}
	}

	def setHead[A](l:List[A],newHead:A):List[A] = {
		l match{
			case Cons(h,t) =>Cons(newHead,t)
			case _ => Nil
		}
	}
	
	def foldRight[A,B](l:List[A],z:B)(f:(A,B)=>B):B = {
		l match{
			case Nil => z
			case Cons(h,t) => f(h, foldRight(t,z)(f) )
		}
	}

	def sum(l:List[Int]):Int={
		foldRight[Int,Int](l,0)((a,b)=>a+b)
	}
	
	def product(l:List[Double]):Double={
		foldRight(l,1.0)(_ * _)
	}
	
	def length[A](l: List[A]):Int = {
		foldRight(l,0)( (_,acc)=> acc + 1)
	}

	@annotation.tailrec
	def foldLeft[A,B](l:List[A],z:B)(f:(B,A)=>B):B = {
		l match{
			case Nil => z
			case Cons(h,t) => foldLeft(t,f(z,h))(f)
		}		 
	}
	
	def append2[A](l:List[A],elem:List[A]):List[A]={
		foldRight(l,elem)(Cons(_,_))
	}
	def append[A](l:List[A],elem:A):List[A]={
		foldRight(l,Cons(elem,Nil))(Cons(_,_))
	}

	def sum2(l:List[Int]):Int={
		foldLeft(l,0)(_ +_)
	}
	def product2(l:List[Double]):Double={
		foldLeft(l:List[Double],1.0) (_ * _)
	}
	def length2[A](l:List[A]):Int={
		foldLeft(l,0)( (acc,_) => acc+1)
	}

	def reverse[A](l:List[A]):List[A]={
		foldLeft(l,List[A]())( (b,a) => Cons(a,b) )
	}	
	
	def concat[A](l: List[List[A]]): List[A] ={
		foldLeft(l,Nil:List[A])(append2)
	}

	def add1(l:List[Int]):List[Int] = {
		foldRight(l,Nil:List[Int])((a,b)=>Cons(a+1,b))
	}

	def d2str(l:List[Double]):List[String] = {
		foldRight(l,Nil:List[String])((a,b)=>Cons(a.toString,b) )
	}

	def map[A,B](l:List[A])(f:A=>B):List[B] = {
		foldRight(l,Nil:List[B])( (a,b) => Cons(f(a),b ) )
	}
	
	def flatMap[A](l:List[A])(f:A=>List[A]):List[A] = {
		foldRight(l,Nil:List[A])( (a,b)=> append2(f(a),b) )
	}
	
	def filter[A](l:List[A])(f:A=>Boolean):List[A] = {
		foldRight(l,Nil:List[A])( (a,b) => {if(f(a)) Cons(a,b) else b} )
	}
	
	def filter2[A](l:List[A])(f:A=>Boolean):List[A] = {
		flatMap(l)( a=> if (f(a)) Cons(a,Nil) else Nil )
	}

	def operatePairLists[A](la:List[A],lb:List[A])(f:(A,A)=>A) :List[A] = {
		(la,lb) match {
			case (_,Nil) => Nil
			case (Nil,_) => Nil
			case (Cons(h1,t1),Cons(h2,t2)) => Cons( f(h1,h2),operatePairLists(t1,t2)(f) )
		}		
	}
	def test(){
		val e1 = List(1,2,3,4)
		val e3 = List(4,5,6,7)
		val e2 = List(1.0,2.0,3,4)
		println("Hello")
		println(e1)
		println(tail(e1))	
		println(drop(e1,2))
		println(drop(e1,4))
		println(dropWhile(e1)(a=>a<3))
		println(setHead(e1,10))
		println(sum2(e1))
		println(product2(e2))
		println(length2(e2))
		println(reverse(e2))
		println(append(e2,3.0))
		println(add1(e1))
		println(d2str(e2))
		println( map(e1)(a=>a+10) )
		println( filter(e1)( a => (a%2==1)) )
		println( filter2(e1)( a => (a%2==1)) )
		println( flatMap(e1)( a => List(a,a) ))
		println( operatePairLists(e1,e3)( (a,b)=>a+b ) )
		println( operatePairLists(e2,e2)( (a,b)=>a*a ) )
	}	
}

List.test()

