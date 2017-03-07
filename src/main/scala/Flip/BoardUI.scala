/*
 * The MineSweeper game.
 *
 * This file contains the interface-related stuff. It defines a class ‘BoardUI’
 * that displays a board, as well as the help popup window.
 */
package Flip

import swing._
import Util.CellUI

/* This object gathers every useful resource used by the game interface, and
 * also defines a method which produces a Swing component representing a cell. */
object Resource
{
	def cellComponent (b:Boolean) =
		new Label {
			icon = cellImage(b)
			opaque = true
		}
	def cellImage (b:Boolean) =
		if (b)
			imgTrue
		else
			imgFalse
	//val cellSize = 40
	val imgTrue  = Util.Resource.img("/Flip/cellTrue.png")
	val imgFalse = Util.Resource.img("/Flip/cellFalse.png")
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
	/* There is no “Mercy” mode for Flip. */
	def mercyMode () = ()
	/* When in game, the multibutton cancels the last move. */
	def endGame () = {
		//publish(Game.NewBoardRequest())
		board.cancelLastMove()
		gridUI.refreshAll()
		updateStatus()
	}
	def updateCounters () =
		movesCounter.text = "%d moves" format board.moveCount
}
