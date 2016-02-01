sealed trait Tree[+A]
case object Nil extends Tree[Nothing]
case class Leaf[A] (value:A) extends Tree[A]
case class Branch[A] (left:Tree[A],right:Tree[A]) extends Tree[A]

object Tree{
val Infi = 99999
def size[A](tree:Tree[A]): Int = tree match {
		case Nil => 0
		case Leaf(aLeaf) => 1
		case Branch(left,right) => size(left) + size(right) + 1
	}

def maximum(tree:Tree[Int]):Int = tree match{
	case Nil => -Infi
	case Leaf(n) => n
	case Branch(left,right) => maximum(left) max maximum(right)
}

def depth[A](tree:Tree[A]):Int = tree match{
	case Nil => 0
	case Leaf(_) => 1
	case Branch(left,right) => 1 + ( depth(left) max depth(right) )
}


def filter[A](tree:Tree[A])(f:A=>Boolean):Tree[A] = tree match{
	case Nil => Nil
	case Leaf(n) =>	if (f(n)) Leaf(n) else Nil
	case Branch(left,right) => Branch( filter(left)(f),filter(right)(f) )
}


def map[A,B](tree:Tree[A])(f:A=>B):Tree[B] = tree match{
	case Nil => Nil
	case Leaf(n) => Leaf(f(n))
	case Branch(left,right) => Branch( map(left)(f), map(right)(f) )
}

def fold[A,B](tree:Tree[A],z:B)(f:A=>B)(g:(B,B)=>B):B = tree match{
	case Nil => z
	case Leaf(n) => f(n)
	case Branch(left,right) => g( fold(left,z)(f)(g),fold(right,z)(f)(g) )
}

def maximumViaFold(tree:Tree[Int]):Int = fold(tree,-Infi)( a=>a ) ( (a,b)=> (a max b) )
def depthViaFold[A](tree:Tree[A]):Int = fold(tree, 0) ( _ => 1) ( (a,b) => 1 + ( a max b) )


def run(){
	val ta =  Branch( Branch(Leaf(1),Leaf(2)),Leaf(3) )  	
	println( size(ta) )
	println( maximum(ta) )
	println( maximumViaFold(ta) )
	println(depth(ta) )
	println( depthViaFold(ta) )
	println( filter(ta)( a => a>2 ) )
	println( map(ta)( a => a * 2.0) )
}

}

Tree.run
