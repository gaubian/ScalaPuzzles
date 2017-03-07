/*
 * The MineSweeper game.
 *
 * This file contains the interface-related stuff. It defines a class ‘BoardUI’
 * that displays a board, as well as the help popup window.
 */
package MineSweeper

import swing._
import java.awt.Color
import Util.CellUI

/* This object gathers every useful resource used by the game interface, and
 * also defines some handy methods. */
object Resource
{
	def cellComponent (c:Cell, gameHasEnded:Boolean = false) =
		new Label {
			icon = cellImage(c, gameHasEnded)
			background = cellBackground(c, gameHasEnded)
			opaque = true
		}
	def cellImage (c:Cell, gameHasEnded:Boolean) =
		if (c.revealed || gameHasEnded)
			c match {
			  case Empty(n) => imgNumber(n)
			  case Mine()   => imgMine
			}
		else if (c.flag)
			imgFlag
		else
			imgHidden
	def cellBackground(c:Cell, gameHasEnded:Boolean) =
		if (c == Mine() && c.revealed)
				colorRevealedMine
		else if (c == Mine() && gameHasEnded) {
			if (c.flag)
				colorRightFlag
			else
				colorMissedMine
		}
		else if (c.flag && gameHasEnded)
			colorWrongFlag
		else if (c.revealed)
			colorRevealed
		else
			colorHidden
	//val cellSize = 40
	val imgHidden = Util.Resource.img("/MineSweeper/cell_hidden.png")
	val imgFlag   = Util.Resource.img("/MineSweeper/cell_flag.png")
	val imgMine   = Util.Resource.img("/MineSweeper/cell_mine.png")
	val imgNumber = Array.tabulate(9)(i => Util.Resource.img("/MineSweeper/cell_empty_%d.png" format i))
	val colorHidden       = Color.gray
	val colorRevealed     = Color.white
	val colorRightFlag    = Color.green
	val colorWrongFlag    = Color.yellow
	val colorMissedMine   = Color.orange
	val colorRevealedMine = Color.red
}

class BoardUI (board:Board) extends Game.BoardUI(board)
{
	/* Flags and mines counters. */
	val flagsCounter = new Label
	val minesCounter = new Label
	counters.contents += flagsCounter
	counters.contents += minesCounter
	updateCounters()
	/* Handle the events and map them to game actions. */
	gridUI.reactions += {
	  case event.KeyPressed(_, event.Key.Space, _,_) =>
		reveal(gridUI.ysel, gridUI.xsel)
	  case event.KeyPressed(_, event.Key.Alt, _,_) =>
		mark(gridUI.ysel, gridUI.xsel)
	  case e @ event.MouseClicked(CellUI(y,x), _,_,_,_) if e.peer.getButton == 1 =>
		reveal(y, x)
	  case e @ event.MouseClicked(CellUI(y,x), _,_,_,_) if e.peer.getButton == 3 =>
		mark(y, x)
	}
	/* Provide the actual display of cells. */
	def getCellComponent (y:Int, x:Int) =
		Resource.cellComponent(board.grid(y)(x), board.hasEnded)
	/* These methods bind user events received by the ‘GridUI’ object to game
	   actions performed by the ‘Board’ object. */
	def reveal (y:Int, x:Int) =
		/* if the cell was flagged, just unflag it, otherwise reveal it: */
		if (board.grid(y)(x).flag)
			mark(y, x)
		else {
			board.reveal(y, x)
			if (board.grid(y)(x) == Empty(0) || board.hasEnded)
				gridUI.refreshAll()
			else
				gridUI.refresh(y, x)
			updateStatus()
		}
	def mark (y:Int, x:Int) = {
		board.mark(y, x)
		gridUI.refresh(y, x)
		updateStatus()
	}
	/* In “Mercy” mode, the multibutton cancels the last move. */
	def mercyMode () = {
		board.cancelLastRevealed()
		gridUI.refresh(board.lastY, board.lastX)
		updateStatus()
	}
	/* When in game, the multibutton ends the game and shows the solution. */
	def endGame () = {
		board.defuse()
		gridUI.refreshAll()
		timer.stop()
		updateStatus()
	}
	/* Refresh the counters displayed. */
	def updateCounters () = {
		flagsCounter.text = "%d flags" format board.flags
		minesCounter.text = "%d mines" format board.mines
	}
}
