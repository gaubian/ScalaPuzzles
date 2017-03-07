/*
 * A tabbed panel that automatically disable tabs when they are inactive, so
 * that only the current tab is taken into account when computing the size of
 * the panel. It can also call an additional method on tab changes.
 */
package Util

import swing._
import javax.swing.event.{ChangeEvent,ChangeListener}

abstract class ResponsiveTabbedPane extends TabbedPane
{
	private def empty = new Label
	private val pageContents = new collection.mutable.ArrayBuffer[Component]
	private var current = 0
	private val changeListener = new ChangeListener() {
		def stateChanged (changeEvent:ChangeEvent) = {
			val newIndex = peer.getSelectedIndex
			if (current != newIndex) {
				pages(current) .content = empty
				pages(newIndex).content = pageContents(newIndex)
				tabSelected(current, newIndex)
				current = newIndex
			}
		}
	}
	peer.addChangeListener(changeListener)
	/* Add a tab page to the panel. */
	def add (page:TabbedPane.Page) = {
		pages += page
		pageContents += page.content
		if (pages.length >= 2)
			page.content = empty
	}
	/* Abstract method called on tab changes. */
	def tabSelected (previous:Int, current:Int) : Unit
}
