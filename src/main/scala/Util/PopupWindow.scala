/*
 * A ‘PopupWindow’ Swing component.
 *
 * Defines a window that can show up and return a value when closed.
 */
package Util

import swing._

class PopupWindow[T] extends Dialog
{
	private var ret : Option[T] = None
	modal = true
	listenTo(this)
	reactions += {
	  case e : event.WindowClosed => return_(None)
	}
	private def return_ (r:Option[T]) = {
		ret = r
		//visible = false
		close()
	}
	/* Call this method from outside the window and wait for the result: */
	def show () = {
		//peer.setLocationRelativeTo(null)
		centerOnScreen()
		//visible = true
		open()
		ret
	}
	/* Call these methods from the popup when you are done: */
	def popupDone (r:T) =
		return_(Some(r))
	def popupCancel () =
		return_(None)
}
