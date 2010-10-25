package org.ituniv.pirates.bittorrent.UI;
import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 * 
 * 
 * @author Jianping Wang
 * @email wjpsky@gmail.com
 */
public class TorrentFilter extends FileFilter {

	// accept file path and find all ".torrent" file.
	public boolean accept(File f) {
		if (f.isDirectory()) {
			return true;
		}
		String extension = Utils.getExtension(f);
		if (extension != null) {
			if (extension.equals(Utils.torrent)) {
				return true;
			} else {
				return false;
			}
		}
		return false;
	}

	// filter description
	public String getDescription() {
		return "Torrent";
	}
}
