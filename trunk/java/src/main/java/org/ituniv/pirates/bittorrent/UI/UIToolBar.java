package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.ituniv.pirates.bittorrent.UI.listener.ToolBarListener;
import org.ituniv.pirates.bittorrent.UI.util.ImageLoader;
import org.ituniv.pirates.bittorrent.UI.util.NullImageException;

/**
 * ToolBar
 * 
 * @author Jianping Wang
 * 
 */
public class UIToolBar {
	ToolBar toolBar;
	SashForm sashForm;
	public static String ADD_TORRENT_NAEM = "add Torrent";

	public static String REMOVE_NAEM = "remove";
	public static String START_NAEM = "start";
	public static String PAUSE_NAEM = "pause";
	public static String STOP_NAEM = "stop";

	public static String LISTENER_OBJECT_NAME = "ToolItem";

	public UIToolBar(SashForm sashForm) {
		this.sashForm = sashForm;

	}

	public void open() {
		createToolBar();
	}

	private void createToolBar() {
		toolBar = new ToolBar(sashForm, SWT.NONE);
		ToolItem addTorrentItem = new ToolItem(toolBar, SWT.PUSH);
		addTorrentItem.setToolTipText(ADD_TORRENT_NAEM);
		try {
			addTorrentItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/add.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		addTorrentItem.addSelectionListener(new ToolBarListener(sashForm
				.getShell(), LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem removeItem = new ToolItem(toolBar, SWT.PUSH);
		removeItem.setToolTipText(REMOVE_NAEM);
		try {
			removeItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/remove.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		removeItem.addSelectionListener(new ToolBarListener(
				sashForm.getShell(), LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem startItem = new ToolItem(toolBar, SWT.PUSH);
		startItem.setToolTipText(START_NAEM);
		try {
			startItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/start.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		startItem.addSelectionListener(new ToolBarListener(sashForm.getShell(),
				LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem pauseItem = new ToolItem(toolBar, SWT.PUSH);
		pauseItem.setToolTipText(PAUSE_NAEM);
		try {
			pauseItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/pause.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		pauseItem.addSelectionListener(new ToolBarListener(sashForm.getShell(),
				LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem stopItem = new ToolItem(toolBar, SWT.PUSH);
		stopItem.setToolTipText(STOP_NAEM);
		try {
			stopItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/stop.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		stopItem.addSelectionListener(new ToolBarListener(sashForm.getShell(),
				LISTENER_OBJECT_NAME));
		createSeparator();

	}

	/**
		 * 
		 */
	private void createSeparator() {
		ToolItem separator = new ToolItem(toolBar, SWT.SEPARATOR);
		separator.setWidth(30);
	}

}
