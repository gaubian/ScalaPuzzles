/*
 * A generic interface for games.
 *
 * A ‘UI’ Swing component provides menus to launch boards with custom parameters
 * or predefined settings, and contains the ‘BoardUI’ component of the current
 * board.
 * A concrete game interface must inherit from it and define values that provide
 * the game’s useful data.
 */
package Game

import swing._
import java.awt.Dimension
import Util.{IntField,PopupWindow}

/* A (integral) parameter the ‘UI’ object uses to generate its “custom settings”
 * popup. */
class Parameter (
  val name:String,
  val default:Int,
  val checker:(Int,ParameterSet)=>Boolean,
  val label:String )

/* Some handy predefined checker functions. */
object Checkers
{
	val positive = (n:Int, _:ParameterSet) => n > 0
}

/* A set of parameters that is to be given back to checker functions. */
trait ParameterSet
{
	def get (name:String) : Int
	def getDifficulty : Difficulty.Value
}

/* How it is really implemented. */
private class MyParameterSet extends ParameterSet
{
	val map = new collection.mutable.HashMap[String,Int]
	var difficulty = Difficulty.Easy
	def get (name:String) =
		map(name)
	def set (name:String, value:Int) =
		map(name) = value
	def getDifficulty =
		difficulty
	def setDifficulty (value:Difficulty.Value) =
		difficulty = value
}

/* This trait gathers useful data about a game for displaying a help popup. */
trait Help
{
	val description : String
	val canLose : Boolean
	val mercyMode : Option[String]
	val inGameAction : String
	val actions : List[(String,String)]
}

/* This event is thrown when the user asked for a new board from the current
 * board interface (as the ‘BoardUI’ object cannot replace itself). */
case class NewBoardRequest () extends event.Event

/* A popup window that provides a form for a list of parameters. */
class SettingsPopup (parameters:List[Parameter], defaultDifficulty:Option[Difficulty.Value]) extends PopupWindow[(ParameterSet,String)]
{
	/* the current set of custom parameters: */
	private val paramValues = new MyParameterSet
	/* the buttons controlling the popup: */
	val okButton = new Button(Action("OK") {
		popupDone((paramValues,seedField.text))
	})
	val cancelButton = new Button(Action("Cancel") {
		popupCancel()
	})
	val buttonsLine = new BoxPanel(Orientation.Horizontal) {
		contents += okButton
		contents += Swing.HGlue
		contents += cancelButton
	}
	/* the form lines, one tuple (label,field) per line (there is one line per
	 * parameter plus one for the difficulty (optional) plus one for the seed): */
	val seedField = new TextField
	var lines : List[(Label,Component)] = List( (new Label("seed: "), seedField) )
	defaultDifficulty match {
	  case None             => ()
	  case Some(difficulty) =>
		val difficultyCombobox = new ComboBox(List(Difficulty.Easy, Difficulty.Medium, Difficulty.Hard))
		lines ::= (new Label("difficulty: "), difficultyCombobox)
		difficultyCombobox.selection.item = difficulty
		difficultyCombobox.inputVerifier = (_:Component) => {
			paramValues.setDifficulty(difficultyCombobox.selection.item)
			true
		}
	}
	/* We do not add directly the lines to the contents of the window;
	 * instead we compute them in reverse order and we store them in a list,
	 * because we need to set the action of each text field to select the
	 * following text field (except for the seed field, which validates the
	 * popup). */
	seedField.action = okButton.action
	var nextField = seedField
	for (param <- parameters.reverseIterator) {
		paramValues.set(param.name, param.default)
		val field = new IntField(3, param.default)
		/* set the action of the text field to select the following one (we
		 * need ‘next’ because of the delayed evaluation of the action): */
		val next = nextField
		field.action = Action("") { next.requestFocus }
		/* add a checker to the field, that checks the validity of the input
		 * and update the values stored in the parameter set: */
		field.check = (value:Int) => {
			val b = param.checker(value, paramValues)
			if (b)
				paramValues.set(param.name, value)
			okButton.enabled = b
			b
		}
		/* add the line: */
		lines ::= (new Label(param.label + ": "), field)
		nextField = field
	}
	/* create the form with the above lines (we use a GridBagPanel to align the
	 * fields and to stretch them through the remaining horizontal space): */
	val form = new GridBagPanel {
		def labelConstraints (i:Int) = new Constraints {
			gridy = i
			gridx = 0
			anchor = GridBagPanel.Anchor.LineEnd
		}
		def fieldConstraints (i:Int) = new Constraints {
			gridy = i
			gridx = 1
			ipadx = 25
			weighty = 1.0
			weightx = 1.0
			fill = GridBagPanel.Fill.Horizontal
		}
		for (((label,field), i) <- lines.iterator.zipWithIndex) {
			layout(label) = labelConstraints(i)
			layout(field) = fieldConstraints(i)
		}
	}
	/* at least, the popup’s contents: */
	title = "Custom game"
	contents = new BorderPanel {
		layout(form) = BorderPanel.Position.Center
		layout(buttonsLine) = BorderPanel.Position.South
	}
}

/* A popup that displays the help information (a description of the game and a
 * list of game controls) given. */
class HelpPopup (gameName:String, help:Help) extends PopupWindow
{
	/* A handy method to create descriptions for game actions related to the
	 * multibutton (and thus assigned to one game state). */
	private def buttonAction (status:Status.Value, descr:String) = {
		val ctrlLabel = new BoxPanel(Orientation.Horizontal) {
			contents += Swing.HGlue
			contents += new Button("") {
				icon = Resource.statusImage(status)
				focusable = false
			}
			contents += new Label("or <Enter>:   ")
		}
		val descrLabel = new Label("<html>" + descr) {
			horizontalAlignment = Alignment.Left
		}
		(ctrlLabel, descrLabel)
	}
	val inGameButton = buttonAction(Status.Playing, help.inGameAction)
	val lostButton   = buttonAction(Status.Lost, "you lost — play again")
	val wonButton    = buttonAction(Status.Won, "you won — play again")
	/* build the list of descriptions involving the multibutton: */
	var buttonActions = List(wonButton)
	if (help.canLose)
		buttonActions ::= lostButton
	help.mercyMode match {
	  case None        => ()
	  case Some(descr) =>
		buttonActions ::= buttonAction(Status.Mercy, descr)
	}
	buttonActions ::= inGameButton
	/* other actions: */
	val actions = help.actions :+ ("arrow keys", "select")
	/* now, create the popup: */
	modal = false
	title = "Help — %s" format gameName
	contents = new BoxPanel(Orientation.Vertical) {
		/* ideally, we would like a window with a fixed width and an automatic
		 * height, but that seems unfeasible with Swing… */
		minimumSize   = new Dimension(400, 0)
		preferredSize = new Dimension(400, 550)
		maximumSize   = new Dimension(400, Int.MaxValue)
		/* a caption with the name and a description of the game: */
		contents += new Label {
			text = "<html>" +
				"<h1>" + gameName + "</h1>" +
				"<p align=justify>" + help.description + "</p>" +
				"<h2>controls</h2></html>"
			xLayoutAlignment = 0.5 // instead of 0.0
		}
		/* the available game actions: */
		contents += new GridPanel(actions.length + buttonActions.length, 2) {
			for ((ctrl,descr) <- actions) {
				contents += new Label(ctrl + ":   ")    {horizontalAlignment = Alignment.Right}
				contents += new Label("<html>" + descr) {horizontalAlignment = Alignment.Left}
			}
			for ((ctrlLabel,descrLabel) <- buttonActions) {
				contents += ctrlLabel
				contents += descrLabel
			}
		}
		/* we can not adapt the height of the window to its contents, but at
		 * least we pack this contents so it is not stretched in a weird manner
		 * (don’t ask why, but only one VGlue is not enough): */
		for (_ <- 1 to 20)
			contents += Swing.VGlue
	}
}

abstract class UI
{
	/* the last game settings used: */
	private var settings = defaultSettings
	/* the popup that asks the user for custom game settings: */
	val customGamePopup = new SettingsPopup(parameters, defaultDifficulty)
	/* the popup that explains the controls: */
	val helpPopup = new HelpPopup(name, help)
	/* the game menu: */
	val menu = new MenuBar {
		contents += new Menu("New") {
			mnemonic = event.Key.N
			contents += new MenuItem(Action(defaultSettings.toString) {
				settings = defaultSettings
				newBoard()
			}) { mnemonic = event.Key.D }
			contents += new Separator
			for (predefSettings <- predefinedSettings) {
				contents += new MenuItem(Action(predefSettings.toString) {
					settings = predefSettings
					newBoard()
				})
			}
			contents += new Separator
			contents += new MenuItem(Action("Custom") {
				customGamePopup.show() match {
				  case None        => ()
				  case Some((p,s)) => settings = makeSettings(p); newBoard(s)
				}
			}) { mnemonic = event.Key.C }
		}
		contents += new MenuItem(Action("Again") {
			newBoard()
		}) { mnemonic = event.Key.A }
		contents += new MenuItem(Action("Help") {
			helpPopup.show()
		}) { mnemonic = event.Key.H }
	}
	/* the toplevel component: */
	val ui = new BorderPanel {
		layout(menu) = BorderPanel.Position.North
	}
	/* Handle new board requests fired by the ‘BoardUI’ component. */
	ui.reactions += {
	  case NewBoardRequest() => newBoard()
	}
	/* This method launches a new board with the settings stored in ‘settings’
	 * and optionally a seed string. */
	def newBoard (seedStr:String = "") : Unit = {
		val seed  = if (seedStr == "") 0 else seedStr.hashCode
		val board = makeBoard(settings, seed)
		ui.layout(board) = BorderPanel.Position.Center
		Main.Main.adjustSize()
		ui.listenTo(board)
		board.gridUI.requestFocus()
	}
	/* The abstract values, to be defined by a concrete game, that provide the
	 * game’s name, set of parameters and description. */
	def name : String
	type Settings
	def defaultSettings : Settings
	def predefinedSettings : List[Settings]
	def parameters : List[Parameter]
	def defaultDifficulty : Option[Difficulty.Value]
	def makeSettings (params:ParameterSet) : Settings
	def makeBoard (settings:Settings, seed:Int) : BoardUI
	def help : Help
}
