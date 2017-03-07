/*
 * The Unruly game.
 *
 * This file contains the interface-related stuff. It defines a class ‘BoardUI’
 * that displays a board, as well as the help popup window.
 */
package Unruly

import swing._
import Util.CellUI

/* This object gathers every useful resource used by the game interface, and
 * also defines a method which produces a Swing component representing a cell. */
object Resource
{
	def cellComponent (c:Cell) =
		new Label {
			icon = cellImage(c)
			opaque = true
		}
	def cellImage (c:Cell) =
		(c.color,c.initial) match
		{
			case (1,true)  => imgCell1Init
			case (1,false) => imgCell1
			case (2,true)  => imgCell2Init
			case (2,false) => imgCell2
			case (_,_)     => imgCellHidden
		}
	//val cellSize = 40
	val imgCellHidden = Util.Resource.img("/Unruly/cell_hidden.png")
	val imgCell1Init  = Util.Resource.img("/Unruly/cell1Init.png")
	val imgCell1      = Util.Resource.img("/Unruly/cell1.png")
	val imgCell2Init  = Util.Resource.img("/Unruly/cell2Init.png")
	val imgCell2      = Util.Resource.img("/Unruly/cell2.png")
}

class BoardUI (board:Board) extends Game.BoardUI(board)
{
	/* Handle the events and map them to game actions. */
	gridUI.reactions += {
	  case event.KeyPressed(_, event.Key.Numpad0, _,_) =>
		playMove(gridUI.ysel, gridUI.xsel, 0)
	  case event.KeyPressed(_, event.Key.Numpad1, _,_) =>
		playMove(gridUI.ysel, gridUI.xsel, 1)
	  case event.KeyPressed(_, event.Key.Numpad2, _,_) =>
		playMove(gridUI.ysel, gridUI.xsel, 2)
	  case e @ event.MouseClicked(CellUI(y,x), _,_,_,_) if e.peer.getButton == 1 =>
		playMove(y, x, 1)
	  case e @ event.MouseClicked(CellUI(y,x), _,_,_,_) if e.peer.getButton == 3 =>
		playMove(y, x, 2)
	}
	/* Provide the actual display of cells. */
	def getCellComponent (y:Int, x:Int) =
		Resource.cellComponent(board.grid(y)(x))
	/* This method binds user events received by the ‘GridUI’ object to game
	   actions performed by the ‘Board’ object. */
	def playMove (y:Int, x:Int, color:Int) = {
		if (color != 0 && color == board.grid(y)(x).color)
			board.playMove(y, x, 0)
		else
			board.playMove(y, x, color)
		gridUI.refresh(y, x)
		updateStatus()
	}
	/* There is no “Mercy” mode for Unruly. */
	def mercyMode () = ()
	/* When in game, the multibutton resigns and shows the (expected) solution. */
	def endGame () = {
		board.resign()
		gridUI.refreshAll()
		timer.stop()
		updateStatus()
	}
	def updateCounters () = ()
}
