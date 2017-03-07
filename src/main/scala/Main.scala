/*
 * The main program.
 *
 * The class Main is the toplevel Swing application. It allows the user to
 * choose and switch between available games.
 */
package Main

import swing._
import javax.swing.{UIManager,UnsupportedLookAndFeelException}
import Util.ResponsiveTabbedPane

final object Main extends SimpleSwingApplication {
	/* Set the look&feel (before doing anything else). */
	try {
		UIManager.setLookAndFeel("com.sun.java.swing.plaf.gtk.GTKLookAndFeel")
	}
	catch {
	 case _ : UnsupportedLookAndFeelException =>
		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
	}
	/* Enable antialiasing. */
	System.setProperty("awt.useSystemAAFontSettings", "lcd")
	System.setProperty("swing.aatext", "true")
	/* list of available games —this is not quite flexible as we have to
	 * hardcode the games, but unfortunately, because of Scala’s lazy
	 * initialization of singleton objects, they cannot simply register
	 * themselves since they have to be referenced from here: */
	val games = List(
		MineSweeper.UI,
		Unruly.UI,
		Flip.UI,
		Flood.UI
	)
	val ui : MainFrame = new MainFrame {
		title = "Scala puzzles"
		val tabs = new ResponsiveTabbedPane {
			def tabSelected (previous:Int, current:Int) = {
				/* when selecting a tab, launch a board (unless this is the home
				 * tab…): */
				if (current != 0)
					games(current-1).newBoard()
				adjustSize()
			}
		}
		val homePage = new Label {
			icon = Util.Resource.img("/welcome.png")
		}
		tabs.add(new TabbedPane.Page("Home", homePage))
		for (game <- games)
			tabs.add(new TabbedPane.Page(game.name, game.ui))
		contents = tabs
		//peer.setLocationRelativeTo(null)
		centerOnScreen()
	}
	/* The toplevel interface. */
	def top = ui
	/* Adjust the size of the main window to its contents. */
	def adjustSize () =
		ui.pack()
}
