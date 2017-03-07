/*
 * The Flood game.
 *
 * This file contains the interface-related stuff. It defines a class ‘BoardUI’
 * that displays a board, as well as the help popup window.
 */
package Flood

import swing._
import java.awt.{Dimension,Color}
import Util.CellUI

/* This object gathers every useful resource used by the game interface, and
 * also defines a method which produces a Swing component representing a cell. */
object Resource
{
	def cellComponent (c:Cell) =
		new Label {
			background = cellBackground(c)
			opaque = true
			preferredSize = new Dimension(cellSize,cellSize)
			minimumSize   = preferredSize
			maximumSize   = preferredSize
		}
	def cellBackground(c:Cell) =
		colors(c.color)
	val cellSize = 40
	val colors = Array(
		new Color(102,102,255),    // blue
		new Color(204,153,255),    // pale purple
		new Color(0,204,102),      // petroleum green
		new Color(255,153,151),    // pink
		new Color(255,255,153),    // beige
		new Color(102,255,178),    // light blue
		new Color(186,21,255),     // purple
		new Color(255,255,255),    // white
		new Color(224,224,224),    // grey
		new Color(204,255,204)     // pale green
	)
}

class BoardUI (board:Board) extends Game.BoardUI(board)
{
	/* moves counter: */
	val movesCounter = new Label
	counters.contents += movesCounter
	updateCounters()
	/* Handle the events and map them to game actions. */
	gridUI.reactions += {
	  case event.KeyPressed(_, event.Key.Space, _,_) =>
		playMove(gridUI.ysel, gridUI.xsel)
	  case e @ event.MouseClicked(CellUI(y,x), _,_,_,_) if e.peer.getButton == 1 =>
		playMove(y, x)
	}
	/* Provide the actual display of cells. */
	def getCellComponent (y:Int, x:Int) =
		Resource.cellComponent(board.grid(y)(x))
	/* This method binds user events received by the ‘GridUI’ object to game
	   actions performed by the ‘Board’ object. */
	def playMove (y:Int, x:Int) = {
		board.playMove(y, x)
		gridUI.refreshAll()
		updateStatus()
	}
	/* There is no “Mercy” mode for Flood. */
	def mercyMode () = ()
	/* When in game, the multibutton launches a new board. */
	def endGame () =
		publish(Game.NewBoardRequest())
	def updateCounters () =
		movesCounter.text = "%d/%d moves" format (board.moveCount,board.maxMoveCount)
}
