case class Player(name:String, score:Double)
def printWiner(p:Player):Unit = println(p.name + ":" + p.score)
def winner(p1:Player, p2:Player): Player = 
	if(p1.score > p2.score) p1 else p2

val players = List(Player("a",1.0),Player("b",3.0),Player("c",0.0))
printWiner(players.reduceLeft(winner))
