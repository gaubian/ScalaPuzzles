/*
 * The Unruly game.
 *
 * This file implements the game itself as a class ’Board’, without
 * bothering with display.
 */
package Unruly

import Game.{Difficulty,Status}

/* Each cell stores its color (0 = None, 1 = Black, 2 = White) and whether it is
 * initially shown. */
class Cell (var color:Int, var initial:Boolean)

/* Stores the parameters for instanciating a new game. It is useful as it can
 * check their validity and enable easy switching between several game profiles. */
class Settings (val name:String, val h:Int, val w:Int, val difficulty:Difficulty.Value)
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
	/* the game grid: */
	val grid = Array.tabulate[Cell](h, w)((y,x) => new Cell(0, true))
	/* number of cells of each color in each line and column: */
	private val blackY = Array.tabulate[Int](h)(y => 0)
	private val blackX = Array.tabulate[Int](w)(x => 0)
	private val whiteY = Array.tabulate[Int](h)(y => 0)
	private val whiteX = Array.tabulate[Int](w)(x => 0)
	/* number of remaining void cells: */
	private var voidC = 0
	/* Randomly generate the grid with the backtracking algorithm. */
	private val rand = if (seed == 0) new util.Random() else new util.Random(seed)
	/* The selector, to randomly chose our variable */
	private val selector = Array.tabulate[(Int,Int)](h*w)(i => (i % h, i / h))
	/* We shuffle the selector */
	shuffleSelector()
	initialize(0,0)
	/* the intended solution (that is, the initial colors): */
	private val solution = Array.tabulate(h, w)((y,x) => grid(y)(x).color)
	/* once we've got the solved grid, we erase some cell */
	settings.difficulty match {
	  case Difficulty.Easy   => eraseEasy()
	  case Difficulty.Medium => eraseMedium()
	  case Difficulty.Hard   => eraseHard()
	}
	/* copies of everything we have initialized (for restarting): */
	private val voidCopy = voidC
	private val blackYCopy = Array.tabulate[Int](h)(y => blackY(y))
	private val blackXCopy = Array.tabulate[Int](w)(x => blackX(x))
	private val whiteYCopy = Array.tabulate[Int](h)(y => whiteY(y))
	private val whiteXCopy = Array.tabulate[Int](w)(x => whiteX(x))
	/* game status: */
	var status = Status.Playing
	/* Tests for the game status. */
	def isPlaying =
		status == Status.Playing
	def hasEnded =
		status == Status.Won || status == Status.Lost
	/* erase some cells: easy */
	private def eraseEasy () =
	{
		for ((y,x) <- selector)
		{
			eraseCellDir(y, x, -1, 0)
			eraseCellDir(y, x, 1, 0)
			eraseCellDir(y, x, 0, -1)
			eraseCellDir(y, x, 0, 1)
			grid(y)(x).initial = (grid(y)(x).color > 0)
		}
	}
	/* erase some cells: medium */
	private def eraseMedium () =
	{
		eraseEasy()
		for ((y,x) <- selector)
		{
			eraseCellLine(y, x, 1, 0)
			eraseCellLine(y, x, 0, 1)
			grid(y)(x).initial = (grid(y)(x).color > 0)
		}
	}
	/* erase some cells: hard */
	private def eraseHard () : Unit =
	{
		var numberToErase = 5
		eraseMedium()
		for ((y,x) <- selector)
		{
			if (grid(y)(x).color > 0)
			{
				color(y, x, 0)
				voidC = voidC + 1
				grid(y)(x).initial = false
				if (numberToErase == 0)
					return
				else numberToErase = numberToErase - 1
			}
		}
	}
	/* erase cell (i,j) if the two followif case are the same color
	on direction (di,dj) */
	private def eraseCellDir (i:Int,j:Int,di:Int,dj:Int) =
	{
		if (grid(i)(j).color > 0 && i+2*di >= 0 && i+2*di < h &&
		j+2*dj >= 0 && j+2*dj < w && grid(i+di)(j+dj).color ==
		grid(i+2*di)(j+2*dj).color && grid(i+di)(j+dj).color > 0)
			{
			color(i,j,0)
			voidC = voidC + 1
			}
	}
	/* erase cell (i,j) if grid(i+di,j+dj)=grid(i-di,j-dj) > 0, with
	di and dj positive */
	private def eraseCellLine (i:Int,j:Int,di:Int,dj:Int) =
	{
		if (grid(i)(j).color > 0 && i-di >= 0 && j-dj >= 0
		&& i+di < h && j+dj < w && grid(i+di)(j+dj).color ==
		grid(i-di)(j-dj).color && grid(i+di)(j+dj).color > 0)
			{
			color(i,j,0)
			voidC = voidC + 1
			}

	}
	/* shuffle the selector, using Fisher-Yates */
	private def shuffleSelector () =
	{
		for (i <- 0 until h*w)
		{
			val choice = i + (rand.nextInt(h*w-i))
			val stored = selector(i)
			selector(i) = selector(choice)
			selector(choice) = stored
		}
	}
	/* Color a cell with a given color. */
	private def color (y:Int, x:Int, c:Int) =
	{
		if (grid(y)(x).color == 1)
		{
			blackY(y) = blackY(y) - 1
			blackX(x) = blackX(x) - 1
		}
		else if (grid(y)(x).color == 2)
		{
			whiteY(y) = whiteY(y) - 1
			whiteX(x) = whiteX(x) - 1
		}
		if (c == 1)
		{
			blackY(y) = blackY(y) + 1
			blackX(x) = blackX(x) + 1
		}
		else if (c == 2)
		{
			whiteY(y) = whiteY(y) + 1
			whiteX(x) = whiteX(x) + 1
		}
		grid(y)(x).color = c
	}
	/* Check if the grid is correct assuming last move was (y,x). */
	private def verif (y:Int, x:Int) : Boolean =
	{
		if (grid(y)(x).color == 1 && (blackY(y)>w/2 || blackX(x)>h/2))
			return false
		if (grid(y)(x).color == 2 && (whiteY(y)>w/2 || whiteX(x)>h/2))
			return false
		verifLine(y, x, -1, 0) && verifLine(y,   x, 1, 0) && verifLine(y, x,   0, -1) &&
		verifLine(y, x,  0, 1) && verifLine(y-1, x, 1, 0) && verifLine(y, x-1, 0,  1)
	}
	/* Check that the three cells between (startY,startX) and
	 * (startY+2*vectY,startX+2*vectX) are not of the same color. */
	private def verifLine (startY:Int,startX:Int,vectY:Int,vectX:Int) : Boolean=
	{
		(startY<0 || startX<0 || startY+2*vectY<0 || startX+2*vectX<0 ||
		 startY>=h || startX>=w || startY+2*vectY>=h || startX+2*vectX>=w) ||
		(grid(startY)(startX).color != grid(startY+vectY)(startX+vectX).color) ||
		(grid(startY+2*vectY)(startX+2*vectX).color != grid(startY+vectY)(startX+vectX).color)

	}
	/* Initialize the grid by backtracking. */
	private def initialize (y:Int, x:Int) : Boolean =
	{
		if (y==h)
			true
		else {
			var followY = y
			var followX = 0
			if (x==w-1)
				followY = y+1
			else
				followX	= x+1
			if (rand.nextBoolean())
			{
				color(y, x, 1)
				if (verif(y, x))
				{
					if (initialize(followY, followX))
						return true
				}
				color(y, x, 2)
				if (verif(y, x))
				{
					if (initialize(followY, followX))
						return true
				}
				color(y, x, 0)
				false
			}
			else
			{
				color(y, x, 2)
				if (verif(y, x))
				{
					if (initialize(followY, followX))
						return true
				}
				color(y, x, 1)
				if (verif(y, x))
				{
					if (initialize(followY, followX))
						return true
				}
				color(y, x, 0)
				false
			}
		}
	}
	/* The available game action. */
	def playMove (y:Int, x:Int, c:Int) = if (isPlaying)
	{
		if (!grid(y)(x).initial)
		{
			val a = grid(y)(x).color
			color(y, x, c)
			if (c==0 || verif(y, x))
			{
				if (a==0 && c!=0)
					voidC = voidC - 1
				else if (a!=0 && c==0)
					voidC = voidC + 1
			}
			else
				color(y, x, a)
			if (voidC==0)
				status = Status.Won
		}
	}
	/* Give up. */
	def resign ()
	{
		for (y <- 0 until h; x <- 0 until w)
			grid(y)(x).color = solution(y)(x)
		status = Status.Lost
	}
	/* Restarts the game */
	def restart ()
	{
		voidC = voidCopy
		Array.copy(blackYCopy, 0, blackY, 0, h)
		Array.copy(blackXCopy, 0, blackX, 0, h)
		Array.copy(whiteYCopy, 0, whiteY, 0, w)
		Array.copy(whiteXCopy, 0, whiteX, 0, w)
		for (y <- 0 until h; x <- 0 until w)
			grid(y)(x).color = if (grid(y)(x).initial) solution(y)(x) else 0
		status = Status.Playing
	}
}
