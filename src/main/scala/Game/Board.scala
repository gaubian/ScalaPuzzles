/*
 * Generic datatypes for games.
 */
package Game

/* Represents the difficulty level of a board. */
object Difficulty extends Enumeration
{
	val Easy, Medium, Hard = Value
	/* The default method ‘toString’ is fine. */
}

/* Represents the status of a game. */
object Status extends Enumeration
{
	val Playing, Mercy, Lost, Won = Value
}

/* Trait of a game board. */
trait Board
{
	val h : Int
	val w : Int
	var status : Status.Value
	def hasEnded : Boolean
	def restart () : Unit
}
