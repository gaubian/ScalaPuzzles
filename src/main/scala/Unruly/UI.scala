/*
 * The Unruly game.
 *
 * This file provides the game interface as an object ‘UI’.
 */
package Unruly

object Help extends Game.Help
{
	val description =
		"Set every cell to either red or blue, such as:<ol>" +
		"<li>there is the same number of cells of both colours in each row " +
		"and in each column,</li>" +
		"<li>and there is not three consecutive cells with the same colour " +
		"(either horizontally or vertically).</li></ol>"
	val canLose = false
	val mercyMode = None
	val inGameAction = "resign and show a solution"
	val actions = List(
		("left click or <1>/<0>", "toggle cell in blue"),
		("right click or <2>/<0>", "toggle cell in red")
	)
}

object UI extends Game.UI
{
	def name = "Unruly"
	type Settings = Unruly.Settings
	def defaultSettings = new Settings("Default", h=10, w=10, Game.Difficulty.Medium)
	def predefinedSettings = List(
		new Settings("Easy",   h=8,  w=8,  Game.Difficulty.Easy),
		new Settings("Medium", h=10, w=10, Game.Difficulty.Medium),
		new Settings("Hard",   h=14, w=14, Game.Difficulty.Hard)
	)
	def makeSettings (params:Game.ParameterSet) = {
		val h = params.get("h")
		val w = params.get("w")
		new Settings("Custom", h, w, params.getDifficulty)
	}
	/* The dimensions of the Unruly board must be even. */
	def evenChecker = (value:Int, params:Game.ParameterSet) =>
		value > 0 && value % 2 == 0
	def parameters = List(
		new Game.Parameter("h", defaultSettings.h, evenChecker, "height"),
		new Game.Parameter("w", defaultSettings.w, evenChecker, "width")
	)
	def defaultDifficulty = Some(defaultSettings.difficulty)
	def makeBoard (settings:Settings, seed:Int) =
		new BoardUI(new Board(settings, seed))
	def help = Unruly.Help
}
