package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import org.ituniv.pirates.bittorrent.ErlangModelDataAccessObject;
import org.ituniv.pirates.bittorrent.UI.util.ImageLoader;
import org.ituniv.pirates.bittorrent.UI.util.NullImageException;

/**
 * main user interface
 * 
 * @author Jianping Wang
 * 
 */
public class TorrentUI {
	private Shell shell;
	private static UITorrentTable uiTorrentTable;

	public TorrentUI() {

	}

	public void open() {
		final Display display = Display.getDefault();
		shell = new Shell(display);
		shell.setText("Pirates BitTorrent Client");
		try {
			shell.setImage(new Image(display, ImageLoader
					.addImage("image/ourbt1.png")));
		} catch (NullImageException e1) {
			e1.printStackTrace();
		}
		shell.setLayout(new FillLayout());
		// init data

		ErlangModelDataAccessObject model = new ErlangModelDataAccessObject();

		// Tray
		UITray uiTray = new UITray(shell);
		uiTray.open();

		// Menu
		// UIMenu uiMenu=new UIMenu(shell);
		// uiMenu.open();

		SashForm sashForm = new SashForm(shell, SWT.VERTICAL | SWT.NONE);

		// Toolbar
		UIToolBar uiToolBar = new UIToolBar(sashForm);
		uiToolBar.open();

		SashForm childSashForm = new SashForm(sashForm, SWT.VERTICAL);
		SashForm childchildSashForm = new SashForm(childSashForm, SWT.NONE);

		// statusbar
		StatusBar statusBar = new StatusBar(childSashForm);
		statusBar.open();
		sashForm.setWeights(new int[] { 5, 100 });
		sashForm.setLayout(new FillLayout());

		// Category
		UICategory uiCategory = new UICategory(childchildSashForm);
		uiCategory.open();

		// table

		uiTorrentTable = new UITorrentTable(childchildSashForm);
		childSashForm.setWeights(new int[] { 110, 4 });
		childSashForm.setLayout(new FillLayout());
		childchildSashForm.setWeights(new int[] { 10, 90 });
		childchildSashForm.setLayout(new FillLayout());

		shell.setMaximized(true);
		shell.open();
		// childchildSashForm.getDisplay().syncExec(thread);
		// fileDig(shell);// add open file
		shell.addShellListener(new ShellAdapter() {

			public void shellClosed(ShellEvent e) {
				Shell[] shells = display.getShells();
				for (int i = 0; i < shells.length; i++) {
					if (shells[i] != shell)
						shells[i].close();
				}
			}
		});
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}

	}

	/**
	 * 
	 * @return
	 */
	public static UITorrentTable getUITorrentTable() {
		return uiTorrentTable;
	}

}
