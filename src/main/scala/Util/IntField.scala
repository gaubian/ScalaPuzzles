/*
 * A ‘IntField’ Swing component that expects an integer input.
 *
 * Only digits can be entered, and a default value and additional checkings can
 * be provided.
 */
package Util

import swing._
import javax.swing.text._
import javax.swing.text.DocumentFilter._

private object IntChecking
{
	def check (s:String) = s matches """\d+"""
	object Filter extends DocumentFilter {
		override def insertString (fb:FilterBypass, off:Int, s:String, a:AttributeSet) =
			if (check(s))
				super.insertString(fb, off, s, a)
		override def replace (fb:FilterBypass, off:Int, len:Int, s:String, a:AttributeSet) =
			if (check(s))
				super.replace(fb, off, len, s, a)
	}
}

import java.awt.Color

object IntField
{
	val normalForeground = (new TextField).foreground
	val errorForeground = Color.red
}

class IntField (cols:Int, default:Int = 0) extends TextField(default.toString, cols)
{
	def value = text.toInt
	def value_= (v:Int) = text = v.toString
	/* Prevent the user from inputing any character other than a digit. */
	peer.getDocument().asInstanceOf[AbstractDocument]
	    .setDocumentFilter(IntChecking.Filter)
	/* Reset to the default value when losing focus if the input is invalid
	   (eg. empty). */
	shouldYieldFocus = (s:String) => {
		if (!IntChecking.check(s))
			value = default
		if (check(value)) {
			foreground = IntField.normalForeground
			true
		}
		else {
			foreground = IntField.errorForeground
			false
		}
	}
	/* Modify this variable to give additional checking on the field’s value: */
	var check = (value:Int) => true
	/* Select contents when focused. */
	listenTo(this)
	reactions += {
	  case e : event.FocusGained => selectAll()
	}
}
