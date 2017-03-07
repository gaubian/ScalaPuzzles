/*
 * The Flood game.
 *
 * This file provides the game interface as an object ‘UI’.
 */
package Flood

object Help extends Game.Help
{
	val description =
		"Fill the complete grid with one colour, by repeatly expanding the " +
		"area at the top-left corner. But careful! You have a limited number " +
		"of moves."
	val canLose = true
	val mercyMode = None
	val inGameAction = "play again"
	val actions = List(
		("left click or <Space>", "choose the color from that cell")
	)
}

object UI extends Game.UI
{
	def name = "Flood"
	type Settings = Flood.Settings
	def defaultSettings = new Settings("Default", h=12, w=12, numColors=7)
	def predefinedSettings = List(
		new Settings("Easy",   h=12, w=12, numColors=7),
		new Settings("Medium", h=16, w=16, numColors=7),
		new Settings("Hard",   h=16, w=16, numColors=10)
	)
	def makeSettings (params:Game.ParameterSet) = {
		val h         = params.get("h")
		val w         = params.get("w")
		val numColors = params.get("c")
		new Settings("Custom", h, w, numColors)
	}
	/* The number of colours is limited to the number available. */
	def numColorsChecker = (value:Int, params:Game.ParameterSet) =>
		2 <= value && value <= Resource.colors.length
	def parameters = List(
		new Game.Parameter("h", defaultSettings.h, Game.Checkers.positive, "height"),
		new Game.Parameter("w", defaultSettings.w, Game.Checkers.positive, "width"),
		new Game.Parameter("c", defaultSettings.numColors, numColorsChecker, "number of colors")
	)
	def defaultDifficulty = None
	def makeBoard (settings:Settings, seed:Int) =
		new BoardUI(new Board(settings, seed))
	def help = Flood.Help
}
