/*
 * The Flip game.
 *
 * This file provides the game interface as an object ‘UI’.
 */
package Flip

object Help extends Game.Help
{
	val description =
		"Toggle every cell in red. But, as you choose one cell, its four " +
		"neighbours are also toggled…"
	val canLose = false
	val mercyMode = None
	val inGameAction = "cancel last move"
	val actions = List(
		("left click or <Space>", "flip")
	)
}

object UI extends Game.UI
{
	def name = "Flip"
	type Settings = Flip.Settings
	def defaultSettings = new Settings("Default", h=4, w=4)
	def predefinedSettings = List(
		new Settings("Easy",   h=3, w=3),
		new Settings("Medium", h=4, w=4),
		new Settings("Hard",   h=5, w=5)
	)
	def makeSettings (params:Game.ParameterSet) = {
		val h = params.get("h")
		val w = params.get("w")
		new Settings("Custom", h, w)
	}
	def parameters = List(
		new Game.Parameter("h", defaultSettings.h, Game.Checkers.positive, "height"),
		new Game.Parameter("w", defaultSettings.w, Game.Checkers.positive, "width")
	)
	def defaultDifficulty = None
	def makeBoard (settings:Settings, seed:Int) =
		new BoardUI(new Board(settings, seed))
	def help = Flip.Help
}
