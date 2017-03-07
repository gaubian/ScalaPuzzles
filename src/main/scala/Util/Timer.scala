/*
 * A convenient ‘Timer’ class that wraps Java’s timers.
 *
 * drawn from:
 *     http://otfried.org/scala/timers.html
 */
package Util

object Timer
{
	def apply(interval: Int, repeats: Boolean = true)(op: => Unit) = {
		val timeOut = new javax.swing.AbstractAction() {
			def actionPerformed(e : java.awt.event.ActionEvent) = op
		}
		val t = new javax.swing.Timer(interval, timeOut)
		t.setRepeats(repeats)
		t.start()
		t
	}
}
