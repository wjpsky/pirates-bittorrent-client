package org.ituniv.pirates.bittorrent.UI;

import org.ituniv.pirates.bittorrent.UI.Launch;


/**
 *    main
 * @author Jianping Wang
 *
 */

public class Launch {

	public void open() {
		TorrentUI UIBt=new TorrentUI();
		UIBt.open();
	}

	/**
	 * 
	 */
	public static void main(String[] args) {
		try {
			Launch TorrentUI = new Launch();
			TorrentUI.open();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
