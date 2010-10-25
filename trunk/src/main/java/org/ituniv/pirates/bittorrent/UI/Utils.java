package org.ituniv.pirates.bittorrent.UI;
import java.io.File;

/**
 * 
 * @author Jianping Wang
 * @email wjpsky@gmail.com
 */
public class Utils {
	public final static String torrent = "torrent";

	// get ".torrent" file suffix.
	public static String getExtension(File f) {
		String ext = null;
		String s = f.getName();
		int i = s.lastIndexOf('.');
		if (i > 0 && i < s.length() - 1) {
			ext = s.substring(i + 1).toLowerCase();
		}
		return ext;
	}
}
