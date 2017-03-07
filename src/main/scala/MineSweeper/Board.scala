/*
 * The MineSweeper game.
 *
 * This file implements the game itself as a class ’Board’, without bothering
 * with display.
 */
package MineSweeper

import Game.{Difficulty,Status}

/* The variant type ‘Cell’ is the type of MineSweeper’s cells. */
abstract class Cell
{
	var revealed = false
	var flag     = false
}
case class Empty (count:Int) extends Cell
case class Mine  ()          extends Cell

/* Stores the parameters for instanciating a new board. It is useful as it can
 * check their validity and enable easy switching between several game profiles. */
class Settings (val name:String, val h:Int, val w:Int, val mines:Int, val difficulty:Difficulty.Value)
{
	assert(0 < h && 0 < w && 0 <= mines && mines <= h*w)
	override def toString =
		name + " (" + h + "×" + w + ", " + mines + " mines)"
}

class Board (settings:Settings, seed:Int = 0) extends Game.Board
{
	/* accessors for the settings: */
	val h = settings.h
	val w = settings.w
	val mines = settings.mines
	/* the game grid: */
	val grid = Array.tabulate[Cell](h, w)((y,x) => Empty(0))
	/* number of flags used: */
	var flags = 0
	val visited = Array.tabulate[Boolean](h,w)((y,x) => false)
	/* game status: */
	var status = Status.Playing
	/* coordinates of the last revealed cell (useful for the ‘mercy’ mode): */
	var (lastY,lastX) = (0,0)
	/* Randomly generate the grid. */
	private val rand = if (seed == 0) new util.Random() else new util.Random(seed)
	var minesPos : List[(Int,Int)] = Nil
	/* okRule is true when the current grid is solvable with the
	current rule  */
	var okRule = false
	while (!okRule)
	{
		for (y <- 0 until h; x <- 0 until w)
		{
			grid(y)(x) = Empty(0)
			flags = 0
			visited(y)(x) = false
			lastY = 0
			lastX = 0
		}
		minesPos = Nil
		for (_ <- 1 to mines) {
			var (y,x) = (0,0)
			do {
				y = rand.nextInt(h)
				x = rand.nextInt(w)
			} while (grid(y)(x) == Mine())
			minesPos ::= (y,x)
			grid(y)(x) = Mine()
			incr(y-1, x-1)
			incr(y-1, x  )
			incr(y-1, x+1)
			incr(y  , x-1)
			incr(y  , x+1)
			incr(y+1, x-1)
			incr(y+1, x  )
			incr(y+1, x+1)
		}
		firstPick()
		settings.difficulty match {
		  case Difficulty.Easy   => okRule = applyEasyRule()
		  case Difficulty.Medium => okRule = applyMediumRule()
		  case Difficulty.Hard   => okRule = applyHardRule()
		}
	}
	restart ()
	private def incr (y:Int, x:Int) =
		if (0 <= y && y < h && 0 <= x && x < w)
			grid(y)(x) match {
			  case Empty(n) => grid(y)(x) = Empty(n+1)
			  case _        => ()
			}
	/* Tests for the game status. */
	def isPlaying =
		status == Status.Playing
	def hasEnded =
		status == Status.Lost || status == Status.Won
	/* The available game actions. */
	/* Is true when the current game can be solved using the easy 
	rules*/
	def applyEasyRule () =
	{
		/* la variable idoine : */
		var idoine = true
		val posFst = Array.tabulate[Boolean](h,w)((y,x) => true)
		val posSnd = Array.tabulate[Boolean](h,w)((y,x) => true)
		while (idoine)
		{
			idoine = false
			for (y <- 0 until h; x <- 0 until w)
			{
				if (grid(y)(x).revealed)
				{
					if (posFst(y)(x) && ruleEasyOne(y,x))
					{
						idoine = true
						posFst(y)(x) = false
					}
					if (posSnd(y)(x) && ruleEasyTwo(y,x))
					{
						idoine = true
						posSnd(y)(x) = false
					}
				}
			}
		}
		isFinished ()
	}
	/* Is true when the current game can be solved using the easy 
	rules*/
	def applyMediumRule () =
	{
		/* le retour de la variable idoine : */
		var idoine = true
		var firstInsuffisant = !(applyEasyRule ())
		while (firstInsuffisant && idoine)
		{
			idoine = false
			for (i <- 0 until h; j <- 0 until w; k <- 0 until h; l <- 0 until w)
			{
				idoine = commonNeighbour(i,j,k,l) || idoine
			}
			applyEasyRule ()
		}
		firstInsuffisant && isFinished ()
	}
	/* Is true when the current game can be solved using the easy 
	rules*/
	def applyHardRule () =
	{
		!applyEasyRule () && !applyMediumRule ()
	}
	/* Is true when the current game can be solved using the first 
	easy rule */
	def ruleEasyOne (y:Int,x:Int) =
	{
		var res = 0
		for (i <- y-1 to y+1; j <- x-1 to x+1)
		{
			if (i > (-1) && j > (-1) && i < h && j < w && !(grid(i)(j).revealed))
				res = res + 1
		}
		grid(y)(x) match {
		  case Empty(n) =>
			if (n==res)
			{
				for (i <- y-1 to y+1; j <- x-1 to x+1)
					if (i > (-1) && j > (-1) && i < h && j < w && !(grid(i)(j).flag))
						mark(i,j)
				true
			}
			else
				false
		  case _ => false
		}
	}
	/* Is true when the current game can be solved using the second 
	easy rule */
	def ruleEasyTwo (y:Int,x:Int) =
	{
		var res = 0
		for (i <- y-1 to y+1; j <- x-1 to x+1)
		{
			if (i >= 0 && j >= 0 && i < h && j < w && grid(i)(j).flag)
				res = res +1
		}
		grid(y)(x) match {
		  case Empty(n) =>
			if (n==res)
			{
				for (i <- y-1 to y+1; j <- x-1 to x+1)
					if (i >= 0 && j >= 0 && i < h && j < w && !(grid(i)(j).flag) && !(grid(i)(j).flag))
						reveal(i,j)
				true
			}
			else
				false
		  case _ => false
		}

	}
	/* Gives the best first pick */
	def firstPick () =
	{
		var maxi = 0
		var maxiYX = (0,0)
		for (y <- 0 until h; x <- 0 until w)
		{
			val valueATM = travel(y,x)
			if (valueATM > maxi)
			{
				maxi = valueATM
				maxiYX = (y,x)
			}

		}
		val (a,b) = maxiYX
		reveal(a,b)
	}
	/* Useful for travel */
	def travel (y:Int, x:Int) : Int =
	{
		if ((y < 0) || (x < 0) || (x > h-1) || (y > w-1) || (visited(y)(x)))
			0
		else {
			visited(y)(x) = true
			grid(y)(x) match {
			  case Empty (0) =>
				1 + travel(y-1,x) + travel(y+1,x) + travel(y,x-1) + travel(y,x+1)
			  case _ =>
				0
			}
		}
	}
	/* Useful for the medium rule */
	def commonNeighbour(i:Int,j:Int,k:Int,l:Int) =
	{
		(grid(i)(j),grid(k)(l)) match {
		  case (Empty(m),Empty(n)) =>
			var smthChanged = false
			var inter : List[(Int,Int)] = Nil
			var v1Lessv2 : List[(Int,Int)] = Nil
			for (y <- i-1 to i+1; x <- j-1 to j+1)
			{
				if ((y>(-1)) && (x>(-1)) && (y<h) && (x<w) && !(grid(y)(x).revealed))
				{
					if (((y-k).abs < 2) && ((x-l).abs < 2))
						inter ::= (y,x)
					else
						v1Lessv2 ::= (y,x)
				}
			}
			if ((inter.length > 0) && (v1Lessv2.length == m-n))
			{
				for(x <- v1Lessv2)
				{
					val (a,b) = x
					if (!(grid(a)(b).flag))
					{
						smthChanged = true
						mark(a,b)
					}
				}
				for (y <- i-1 to i+1; x <- j-1 to j+1)
				{
					if ((y>(-1)) && (x>(-1)) && (y<h) && (x<w) &&
						!(grid(y)(x).revealed) && !(((y-i).abs < 2) && ((x-j).abs < 2)))
					{
						smthChanged = true
						reveal(y,x)
					}
				}
			}
			smthChanged
		  case _ => false
		}
	}
	/* True iff current grid is finished (regardless of actual 
	game state) ss*/
	def isFinished () =
	{
		var rest = mines
		for (y <- 0 until h;x <- 0 until w)
		{
			if (grid(y)(x).flag)
				rest = rest - 1
		}
		(rest == 0)
	}
	def mark (y:Int, x:Int) = if (isPlaying)
		if (!grid(y)(x).revealed)
			if (grid(y)(x).flag) {
				grid(y)(x).flag = false
				flags -= 1
			}
			else if (flags < mines) {
				grid(y)(x).flag = true
				flags += 1
			}
	def reveal (y:Int, x:Int) = if (isPlaying)
		if (!grid(y)(x).flag) {
			revealZone(y, x)
			if (grid(y)(x) == Mine()) {
				status = Status.Mercy
			}
		}
	private def revealZone (y0:Int, x0:Int) = {
		val stack = new collection.mutable.Stack[(Int,Int)]
		def explore (y:Int, x:Int) =
			if (0 <= y && y < h && 0 <= x && x < w && !grid(y)(x).revealed) {
				if (grid(y)(x).flag)
					mark(y, x)
				grid(y)(x).revealed = true
				lastY = y
				lastX = x
				if (grid(y)(x) == Empty(0))
					stack.push((y,x))
			}
		explore(y0, x0)
		while (!stack.isEmpty) {
			val (y,x) = stack.pop()
			explore(y-1, x-1)
			explore(y-1, x  )
			explore(y-1, x+1)
			explore(y  , x-1)
			explore(y  , x+1)
			explore(y+1, x-1)
			explore(y+1, x  )
			explore(y+1, x+1)
		}
	}
	def cancelLastRevealed () = if (status == Status.Mercy) {
		grid(lastY)(lastX).revealed = false
		status = Status.Playing
	}
	def defuse () = if (isPlaying)
		if (minesPos forall {case (y,x) => grid(y)(x).flag})
			status = Status.Won
		else
			status = Status.Lost
	def restart () = if (!hasEnded) {
		for (y <- 0 until h; x <- 0 until w) {
			grid(y)(x).flag = false
			grid(y)(x).revealed = false
		}
		flags = 0
		status = Status.Playing
	}
}
