/*
 * The Flip game.
 *
 * This file implements the game itself as a class ’Board’, without
 * bothering with display.
 */
package Flip

import Game.Status

/* Stores the parameters for instanciating a new game. It is useful as it can
 * check their validity and enable easy switching between several game profiles. */
class Settings (val name:String, val h:Int, val w:Int)
{
	assert (h > 0)
	assert (w > 0)
	override def toString =
		name + " (" + h + "×" + w + ")"
}

class Board (settings:Settings, seed:Int = 0) extends Game.Board
{
	/* accessors for the settings: */
	val h = settings.h
	val w = settings.w
	/* the game grid (the goal is to have only true): */
	val grid = Array.tabulate[Boolean](h, w)((y,x) => false)
	/* the list of moves done when generating the board (useful for restarting). */
	private var initialMoves : List[(Int,Int)] = Nil
	/* game status: */
	var status = Status.Playing
	private var mayWin = false
	private var nbFalse = h*w
	/* number of moves done: */
	var moveCount = 0
	/* history of moves: */
	private val history = new collection.mutable.Stack[(Int,Int)]
	/* Randomly generate the grid. */
	private val rand = if (seed == 0) new util.Random() else new util.Random(seed)
	for (y <- 0 until h; x <- 0 until w)
	{
		if (rand.nextBoolean())
			initialMoves ::= (y,x)
	}
	init()
	/* Tests for the game status. */
	def isPlaying =
		status == Status.Playing
	def hasEnded =
		status == Status.Won
	/* Initialize the board with the ‘initialMoves’. */
	private def init () =
	{
		for ((y,x) <- initialMoves)
			playMove(y, x)
		while (!history.isEmpty)
			history.pop()
		moveCount = 0
		mayWin = true
	}
	/* Toggle a cell. */
	private def flip (y:Int, x:Int) =
	{
		if (grid(y)(x))
			nbFalse = nbFalse + 1
		else
			nbFalse = nbFalse - 1
		grid(y)(x) = !grid(y)(x)
	}
	/* The available game action. */
	def playMove (y:Int, x:Int) = if (isPlaying)
	{
		history.push((y,x))
		flip(y, x)
		if (y > 0)
			flip(y-1, x)
		if (y < h-1)
			flip(y+1, x)
		if (x > 0)
			flip(y, x-1)
		if (x < w-1)
			flip(y, x+1)
		moveCount = moveCount + 1
		if (mayWin && nbFalse == 0)
			status = Status.Won
	}
	/* Cancel the last move. */
	def cancelLastMove () =
		if (!history.isEmpty)
		{
			val (y,x) = history.pop()
			playMove(y, x)
			history.pop()
			moveCount = moveCount - 2
		}
	/* Restart the game. */
	def restart () = if (!hasEnded)
	{
		status = Status.Playing
		mayWin = false
		nbFalse = h*w
		for (y <- 0 until h; x <- 0 until w)
			grid(y)(x) = false
		init()
	}
}
