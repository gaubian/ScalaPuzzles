/*
 * A ‘BoardUI’ Swing component that displays a game board with additional
 * elements such as status information.
 */
package Game

import swing._
import Util.{GridUI,Timer}

/* This object gathers every useful resource used by the board interface. */
object Resource
{
	def statusImage (status:Status.Value) = status match {
	  case Status.Playing => imgStatusPlaying
	  case Status.Mercy   => imgStatusMercy
	  case Status.Lost    => imgStatusLost
	  case Status.Won     => imgStatusWon
	}
	val imgStatusPlaying = Util.Resource.img("/status_playing.png")
	val imgStatusMercy   = Util.Resource.img("/status_mercy.png")
	val imgStatusLost    = Util.Resource.img("/status_lost.png")
	val imgStatusWon     = Util.Resource.img("/status_won.png")
	val fontTimer = Util.Resource.font("/LiquidCrystal.otf").deriveFont(20f)
}

abstract class BoardUI (board:Board) extends BorderPanel
{
	/* the board’s grid: */
	val gridUI = new GridUI(board.h, board.w) {
		def getCellComponent (y:Int, x:Int) =
			BoardUI.this.getCellComponent(y, x)
	}
	/* an empty component to be optionally filled by concrete games: */
	val counters = new BoxPanel(Orientation.Vertical)
	/* the big “multi-button”, which displays different smileys and performs
	 * different tasks depending on the game status: */
	val multiButton = new Button(Action("") {
		multiButtonDo()
		//gridUI.requestFocus()
	})
	/* the “Restart” button: */
	val restartButton = new Button(Action("Restart") {
		restart()
		//gridUI.requestFocus()
	})
	/* the timer (with a cool LCD style): */
	var t0 = 0L    // the reference time for the timer
	val timerLabel = new Label {
		font = Resource.fontTimer
		border = Swing.EmptyBorder(0, 1, 0, 1)
	}
	val timer = Timer(1000) {
		val s = (System.currentTimeMillis - t0) / 1000
		timerLabel.text = "%02d:%02d".format(s/60, s%60)
	}
	/* the status bar above the grid, holding all those components: */
	val gameControls = new BoxPanel(Orientation.Horizontal) {
		contents += counters
		contents += Swing.HGlue
		contents += multiButton
		contents += Swing.HGlue
		contents += new BoxPanel(Orientation.Vertical) {
			contents += new BoxPanel(Orientation.Horizontal) {
				contents += restartButton
			}
			contents += new BoxPanel(Orientation.Horizontal) {
				contents += timerLabel
			}
		}
	}
	updateStatus(countersInitialized=false)
	initTimer()
	layout(gameControls) = BorderPanel.Position.North
	layout(gridUI)       = BorderPanel.Position.Center
	/* Disable focusing the buttons so that the grid cannot lose focus. */
	multiButton  .focusable = false
	restartButton.focusable = false
	/* Start from the center of the grid (better-looking). */
	gridUI.select(board.h/2, board.w/2)
	/* Map the multibutton’s action to the “Enter” key. */
	gridUI.reactions += {
	  case event.KeyPressed(_, event.Key.Enter, _,_) =>
		multiButtonDo()
	}
	/* Reset the timer and start it. */
	def initTimer () = {
		timer.stop()
		timerLabel.text = "00:00"
		t0 = System.currentTimeMillis
		timer.start()
	}
	/* Refresh the status bar. */
	def updateStatus (countersInitialized:Boolean=true) : Unit = {
		if (countersInitialized)
			updateCounters()
		multiButton.icon = Resource.statusImage(board.status)
		//restartButton.enabled = !board.hasEnded
		if (board.hasEnded)
			timer.stop()
	}
	/* Perform the “multi”-button action according to the current game status. */
	def multiButtonDo () =
		board.status match {
		  case Status.Playing => endGame()
		  case Status.Mercy   => mercyMode()
		  case Status.Lost    => publish(NewBoardRequest())
		  case Status.Won     => publish(NewBoardRequest())
		}
	/* Restart the game with the same board. */
	def restart () = {
		board.restart()
		gridUI.refreshAll()
		initTimer()
		updateStatus()
	}
	/* Abstract method that provides a Swing component for a given cell. */
	def getCellComponent (y:Int, x:Int) : Component
	/* Abstract method called to update the counters in the status bar. */
	def updateCounters () : Unit
	/* Abstract method performing the multibutton’s action when in “Mercy” mode. */
	def mercyMode () : Unit
	/* Abstract method performing the multibutton’s action when playing (usually
	   ending the game, maybe showing the solution). */
	def endGame () : Unit
}
