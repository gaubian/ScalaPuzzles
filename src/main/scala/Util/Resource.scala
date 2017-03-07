/*
 * Some handy methods for loading resources.
 */
package Util

import javax.swing.ImageIcon
import java.awt.Font
import java.io.FileNotFoundException

package object Resource
{
	private def resURL (relPath:String) = {
		getClass.getResource(relPath) match {
			case null => throw new FileNotFoundException(relPath)
			case url  => url
		}
	}
	def img (path:String) =
		new ImageIcon(resURL(path))
	def font (path:String) =
		Font.createFont(Font.TRUETYPE_FONT, resURL(path).openStream())
}
