/*
 * The Flood game.
 *
 * This file implements the game itself as a class ’Board’, without
 * bothering with display.
 */
package Flood

import Game.Status

/* Each cell stores its color and whether it is controlled. */
class Cell (var color:Int, var controlled:Boolean)

/* Stores the parameters for instanciating a new game. It is useful as it can
 * check their validity and enable easy switching between several game profiles. */
class Settings (val name:String, val h:Int, val w:Int, val numColors:Int)
{
	assert(h > 0)
	assert(w > 0)
	assert(numColors > 0)
	assert(numColors < 11)
	override def toString =
		name + " (" + h + "×" + w + ", " + numColors + " colors)"
}

class Board (settings:Settings, seed:Int = 0) extends Game.Board
{
	/* accessors for the settings: */
	val h = settings.h
	val w = settings.w
	val numColors = settings.numColors
	/* randomly generate the grid. */
	private val rand = if (seed == 0) new util.Random() else new util.Random(seed)
	/* the game grid: */
	val grid = Array.tabulate[Cell](h, w)((y,x) => new Cell(rand.nextInt(numColors),false))
	/* A copy of the initial colors (useful for restarting). */
	private val initialColors = Array.tabulate(h, w)((y,x) => grid(y)(x).color)
	/* number of moves done: */
	var moveCount = 0
	var maxMoveCount = h*w
	/* game status: */
	var status = Status.Playing
	/* list which enumerates cells we control: */
	private var controlledCells : List[(Int,Int)] = (0,0) :: Nil
	/* stack which enumerates cells at the frontier: */
	private val frontier = new collection.mutable.Stack[(Int,Int)]
	/* an intermediate stack that will be useful: */
	private val work = new collection.mutable.Stack[(Int,Int)]
	/* Initialization. */
	restart ()
	/* Here, we make an algorithm play using the strategy of “the descending
	 * diagonal”, which actually works well. */
	while (status == Status.Playing && !frontier.isEmpty)
	{
		val (y,x) = frontier.pop()
		val c = grid(y)(x).color
		if (seeDirection(y+1, x, c) == 2)
		{
			frontier.push((y,x))
			playMove(y+1, x)
		}
		else if (seeDirection(y, x+1, c) == 2)
		{
			frontier.push((y,x))
			playMove(y, x+1)
		}
		else if (seeDirection(y, x-1, c) == 2)
		{
			frontier.push((y,x))
			playMove(y, x-1)
		}
		else if (seeDirection(y-1, x, c) == 2)
		{
			frontier.push((y,x))
			playMove(y-1, x)
		}
	}
	/* We define the score obtained by the algorithm as the upper limit. */
	maxMoveCount = moveCount
	restart ()
	/* Tests for the game status. */
	def isPlaying =
		status == Status.Playing
	def hasEnded =
		status == Status.Won || status == Status.Lost
	/* Color the controlled zone with color c. */
	private def colorZone (c:Int) =
	{
		for ((y,x) <- controlledCells)
			grid(y)(x).color = c
	}
	/* Empty stack ‘frontier’ into ‘work’. */
	private def switch () =
	{
		while (!frontier.isEmpty)
			work.push(frontier.pop())
	}
	/* Expand the controlled zone. */
	private def expand () =
	{
		switch ()
		while (!work.isEmpty)
		{
			val (y,x) = work.pop()
			expandCell(y,x)
		}
	}
	/* Takes a look at grid(y)(x) and return:
		— 2 if it is not controlled and has the given color,
		— 1 if it is not controlled and has not the given color,
		— 0 otherwise.
	*/
	private def seeDirection (y:Int, x:Int, c:Int) =
	{
		if (y>=0 && x>=0 && y<h && x<w && !grid(y)(x).controlled)
		{
			if (grid(y)(x).color == c)
				1
			else
				2
		}
		else
			0
	}
	/* Add a cell to the controlled zone. */
	private def addToControlled (y:Int, x:Int) = {
		controlledCells ::= (y,x)
		grid(y)(x).controlled = true
		work.push((y,x))
	}
	/* Expand controlled zone from the cell (i,j) (which will likely be on the
	 * border. */
	private def expandCell (y:Int, x:Int) =
	{
		val left  = seeDirection(y-1, x, grid(y)(x).color)
		val up    = seeDirection(y, x-1, grid(y)(x).color)
		val right = seeDirection(y+1, x, grid(y)(x).color)
		val down  = seeDirection(y, x+1, grid(y)(x).color)
		var flag = false
		if (left > 0)
		{
			if (left == 1)
				addToControlled(y-1, x)
			else
				flag = true
		}
		if (up > 0)
		{
			if (up == 1)
				addToControlled(y, x-1)
			else
				flag = true
		}
		if (right > 0)
		{
			if (right == 1)
				addToControlled(y+1, x)
			else
				flag = true
		}
		if (down > 0)
		{
			if (down == 1)
				addToControlled(y, x+1)
			else
				flag = true
		}
		if (flag)
			frontier.push((y,x))
	}
	/* The available game action. */
	def playMove (y:Int, x:Int) = if (isPlaying)
	{
		val c = grid(y)(x).color
		if (c != grid(0)(0).color)
		{
			colorZone(c)
			expand()
			moveCount = moveCount + 1
			if (controlledCells.length == h*w)
				status = Status.Won
			else if (moveCount == maxMoveCount)
				status = Status.Lost
		}
	}
	/* Restart the game. */
	def restart () =
	{
		for (y <- 0 until h; x <- 0 until w)
			grid(y)(x) = new Cell(initialColors(y)(x), false)
		while (!frontier.isEmpty)
			frontier.pop()
		controlledCells = Nil
		addToControlled(0, 0)
		moveCount = 0
		status = Status.Playing
		expand()
	}
}
