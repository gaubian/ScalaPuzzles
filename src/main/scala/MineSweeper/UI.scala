/*
 * The MineSweeper game.
 *
 * This file provides the game interface as an object ‘UI’.
 */
package MineSweeper

object Help extends Game.Help
{
	val description =
		"Find all the mines without triggering them! Flag the cells where " +
		"you think there is a mine, then when you think you have found them " +
		"all, put an end to the game. Empty cells indicate the number of " +
		"mines among their eight neigbours."
	val canLose = true
	val mercyMode = Some("you triggered a mine! — cancel that poor move")
	val inGameAction = "you think you are done? — defuse!"
	val actions = List(
		("left click or <Space>", "reveal"),
		("right click or <Alt>", "flag")
	)
}

object UI extends Game.UI
{
	def name = "MineSweeper"
	type Settings = MineSweeper.Settings
	def defaultSettings = new Settings("Default", h=9, w=9, mines=16, Game.Difficulty.Medium)
	def predefinedSettings = List(
		new Settings("Easy",   h=9,  w=9,  mines=10, Game.Difficulty.Easy),
		new Settings("Medium", h=16, w=16, mines=40, Game.Difficulty.Medium),
		new Settings("Hard",   h=16, w=16, mines=99, Game.Difficulty.Hard)
	)
	def makeSettings (params:Game.ParameterSet) = {
		val h     = params.get("h")
		val w     = params.get("w")
		val mines = params.get("mines")
		new Settings("Custom", h, w, mines, params.getDifficulty)
	}
	/* Do not allow the user to enter more mines than the number of cells. */
	def minesFieldChecker = (value:Int, params:Game.ParameterSet) =>
		value <= params.get("h") * params.get("w")
	def parameters = List(
		new Game.Parameter("h", defaultSettings.h, Game.Checkers.positive, "height"),
		new Game.Parameter("w", defaultSettings.w, Game.Checkers.positive, "width"),
		new Game.Parameter("mines", defaultSettings.mines, minesFieldChecker, "mines")
	)
	def defaultDifficulty = Some(defaultSettings.difficulty)
	def makeBoard (settings:Settings, seed:Int) =
		new BoardUI(new Board(settings, seed))
	def help = MineSweeper.Help
}
