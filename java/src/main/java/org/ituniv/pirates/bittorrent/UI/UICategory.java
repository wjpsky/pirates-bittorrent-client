package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.ituniv.pirates.bittorrent.UI.listener.ToolBarListener;
import org.ituniv.pirates.bittorrent.UI.util.ImageLoader;
import org.ituniv.pirates.bittorrent.UI.util.NullImageException;

/**
 * 
 * @author Jianping Wang
 * 
 */
public class UICategory {
	private SashForm sashForm;
	public static String LISTENER_OBJECT_NAME = "Tree";

	public UICategory(SashForm sashForm) {
		this.sashForm = sashForm;

	}

	public void open() {
		createCategory();
	}

	private void createCategory() {
		Tree tree = new Tree(sashForm, SWT.BORDER | SWT.SINGLE);
		tree.addSelectionListener(new ToolBarListener(sashForm.getShell(),
				"Tree"));

		tree.setLinesVisible(false);
		TreeItem allItem = new TreeItem(tree, SWT.BORDER);
		allItem.setText("ALL");
		try {
			allItem.setImage(new Image(sashForm.getDisplay(), ImageLoader
					.addImage("image/all.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}

		TreeItem downloadItem = new TreeItem(tree, SWT.BORDER);
		downloadItem.setText("downloading");
		try {
			downloadItem.setImage(new Image(sashForm.getDisplay(), ImageLoader
					.addImage("image/downloading.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}

		TreeItem completeItem = new TreeItem(tree, SWT.BORDER);
		completeItem.setText("Finished");
		try {
			completeItem.setImage(new Image(sashForm.getDisplay(), ImageLoader
					.addImage("image/finished.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}

		TreeItem activeItem = new TreeItem(tree, SWT.BORDER);
		activeItem.setText("Active");
		try {
			activeItem.setImage(new Image(sashForm.getDisplay(), ImageLoader
					.addImage("image/activity.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}

		TreeItem inActiveItem = new TreeItem(tree, SWT.BORDER);
		inActiveItem.setText("stop");
		try {
			inActiveItem.setImage(new Image(sashForm.getDisplay(), ImageLoader
					.addImage("image/stop.png")));
		} catch (NullImageException e) {
			e.printStackTrace();
		}
	}

}
