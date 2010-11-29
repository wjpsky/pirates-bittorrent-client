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
 * 
 * @author Jianping Wang
 * 
 */
public class UIToolBar {
	ToolBar toolBar;
	SashForm sashForm;
	public static String ADD_TORRENT_NAEM = "add Torrent";
	//public static String ADD_TORRENT_URL_NAEM = "add torrent from link";

	public static String REMOVE_NAEM = "remove";
	public static String START_NAEM = "begin";
	public static String PAUSE_NAEM = "pause";
	public static String STOP_NAEM = "stop";
	public static String MOVE_UP__NAEM = "move up";
	public static String MOVE_DOWN__NAEM = "move down";
	public static String PERFERENCE_NAEM = "option";

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
		/*
		ToolItem addTorrentURLItem = new ToolItem(toolBar, SWT.PUSH);
		addTorrentURLItem.setToolTipText(ADD_TORRENT_URL_NAEM);
		try {
			addTorrentURLItem.setImage(new Image(sashForm.getShell()
					.getDisplay(), ImageLoader.addImage("image/add_url.bmp")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		addTorrentURLItem.addSelectionListener(new ToolBarListener(sashForm
				.getShell(), LISTENER_OBJECT_NAME));
		createSeparator();
	*/
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
		/*
		ToolItem moveUpItem = new ToolItem(toolBar, SWT.PUSH);
		moveUpItem.setToolTipText(MOVE_UP__NAEM);
		try {
			moveUpItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/1_00x07.bmp")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		moveUpItem.addSelectionListener(new ToolBarListener(
				sashForm.getShell(), LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem moveDownItem = new ToolItem(toolBar, SWT.PUSH);
		moveDownItem.setToolTipText(MOVE_DOWN__NAEM);
		try {
			moveDownItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/1_00x08.bmp")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		moveDownItem.addSelectionListener(new ToolBarListener(sashForm
				.getShell(), LISTENER_OBJECT_NAME));
		createSeparator();

		ToolItem perferenceItem = new ToolItem(toolBar, SWT.PUSH);
		perferenceItem.setToolTipText(PERFERENCE_NAEM);
		try {
			perferenceItem.setImage(new Image(sashForm.getShell().getDisplay(),
					ImageLoader.addImage("image/1_00x11.bmp")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
		perferenceItem.addSelectionListener(new ToolBarListener(sashForm
				.getShell(), LISTENER_OBJECT_NAME));
				*/
	}
	

	/**
		 * 
		 */
	private void createSeparator() {
		ToolItem separator = new ToolItem(toolBar, SWT.SEPARATOR);
		separator.setWidth(5);
	}

}
