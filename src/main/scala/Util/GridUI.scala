/*
 * A ‘GridUI’ Swing component than can be used to display grids.
 */
package Util

import swing._
import java.awt.Color

object CellUI
{
	val normalBorder   = Swing.LineBorder(Color.gray,  2)
	val selectedBorder = Swing.LineBorder(Color.black, 2)
}

/* ‘CellUI’ is a wrapper around the cell component. Il permits retrieving cell
 * coordinates in reactions and in-place modification of cell contents. */
case class CellUI (y:Int, x:Int) extends BoxPanel(Orientation.NoOrientation)
{
	private var c : Component = new Label("")
	def set (c2:Component) = {
		contents -= c
		c = c2
		contents += c
		revalidate()
		repaint()
	}
	def selected (b:Boolean) =
		border = if (b) CellUI.selectedBorder else CellUI.normalBorder
	selected(false)
}

/* To use the abstract class ‘GridUI’, one must inherit from it and provide the
 * ‘getCellComponent’ method which, when given coordinates, produces a component
 * that actually represent the associated cell.
 * Since the ‘GridUI’ listens to its keyboard and to its ‘CellUI’ mouse events,
 * those events can be exploited by the concrete implementation.
 * That implementation can also make use of the coordinates of the selected cell
 * to adapt its actions.
 */
abstract class GridUI (val h:Int, val w:Int) extends GridPanel(h, w)
{
	/* a matrix intended for direct access to CellUI components: */
	private val grid = Array.ofDim[CellUI](h, w)
	/* coordinates of the selected cell: */
	var (ysel,xsel) = (0,0)
	/* Create the ‘CellUI’ wrappers and listen to their mouse events. */
	for (y <- 0 until h; x <- 0 until w) {
		grid(y)(x) = CellUI(y, x)
		listenTo(grid(y)(x).mouse.clicks)
		contents += grid(y)(x)
	}
	refreshAll()
	/* A cell can be selected with keyboard arrow keys or left-clicking. */
	select(0, 0)
	focusable = true
	listenTo(this.keys)
	reactions += {
	  case event.KeyPressed(_, event.Key.Left,  _, _) =>
		moveSelection(dx = -1)
	  case event.KeyPressed(_, event.Key.Right, _, _) =>
		moveSelection(dx = +1)
	  case event.KeyPressed(_, event.Key.Up,    _, _) =>
		moveSelection(dy = -1)
	  case event.KeyPressed(_, event.Key.Down,  _, _) =>
		moveSelection(dy = +1)
	  case e @ event.MouseClicked(CellUI(y,x), _, _, _, _) if e.peer.getButton == 1 =>
		select(y, x)
	}
	def selection =
		(ysel,xsel)
	def select (y:Int, x:Int) = {
		grid(ysel)(xsel).selected(false)
		grid(y)(x).selected(true)
		ysel = y
		xsel = x
	}
	def moveSelection (dy:Int = 0, dx:Int = 0) =
		select(y = (h+ysel+dy) % h, x = (w+xsel+dx) % w)
	def getCellComponent (y:Int, x:Int) : Component
	def refresh (y:Int, x:Int) =
		grid(y)(x).set(getCellComponent(y,x))
	def refreshAll () =
		for (y <- 0 until h; x <- 0 until w)
			refresh(y, x)
}
