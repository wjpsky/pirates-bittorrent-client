package org.ituniv.pirates.UI.listener;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.ituniv.pirates.bittorrent.UI.UICategory;
import org.ituniv.pirates.bittorrent.UI.UIToolBar;
import org.ituniv.pirates.bittorrent.UI.UITorrentTable;

import org.ituniv.pirates.bittorrent.ErlangModelDataAccessObject;
import org.ituniv.pirates.bittorrent.File;
import org.ituniv.pirates.bittorrent.Torrent;

/**
 * 
 * @author Jianing Wang
 * 
 */
public class BaseListener  implements SelectionListener {
    public Shell shell;
    private String type;
    private String text;
   
	
    public BaseListener(Shell shell, String type) {
	this.shell = shell;
	this.type = type;
	
	
	
	//thread.start();
	
    }

    public void widgetDefaultSelected(SelectionEvent e) {

    }

    public void widgetSelected(SelectionEvent e) {
	addBaseListener(e);
    }

    protected void addBaseListener(SelectionEvent e) {
	if (type.equals(UIToolBar.LISTENER_OBJECT_NAME)) {
	    text = ((ToolItem) e.getSource()).getToolTipText();
	    System.out.println("SelectionEvent:" + text);

	    if (text.equals(UIToolBar.ADD_TORRENT_NAEM)) {
		fileDig(this.shell);
	    } else if (text.equals(UIToolBar.REMOVE_NAEM)) {

		UITorrentTable.delete();
	    }else if (text.equals(UIToolBar.STOP_NAEM)) {
		UITorrentTable.StopTorrent();
		// System.out.println("SelectionEvent:" + text);
	    }else if (text.equals(UIToolBar.START_NAEM)) {
		 UITorrentTable.BeginTorrent();
		// System.out.println("SelectionEvent:" + text);
	    }
	    // } else if (type.equals(UIMenu.LISTEN_OBJECT_NAME)) {
	    // text = ((MenuItem) e.getSource()).getText();
	} else if (type.equals(UICategory.LISTENER_OBJECT_NAME)) {

	    text = ((Tree) e.getSource()).getSelection()[0].getText();//获取选中�?TreeItem
	    UITorrentTable.setText(text);   
	    if (text.equals("ALL")) {

		    UITorrentTable.initData();//
			System.out.println("SelectionEvent:" + text);
		    } else if (text.equals("download")) {
			;
			UITorrentTable.DownloadTable();
			

		    } else if (text.equals("Finished")) {
			UITorrentTable.FinishedTable();
			System.out.println("SelectionEvent:" + text);

		    } else if (text.equals("Activity")) {
			UITorrentTable.ActivityTable();
			System.out.println("SelectionEvent:" + text);

		    } else if (text.equals("stop")) {
			UITorrentTable.StopTable();
			System.out.println("SelectionEvent:" + text);

		    }

	}

	//System.out.println("SelectionEvent:"+e);

    }

    /** torrent文件选择�?
     * zjh 2010-11-20
     * @param shell
     */
    protected void fileDig(Shell shell) {
	FileDialog fileSelect = new FileDialog(shell, SWT.SINGLE);
	fileSelect.setFilterNames(new String[] { "*.torrent" });
	fileSelect.setFilterExtensions(new String[] { "*.torrent" });
	String url = "";
	url = fileSelect.open();
	if (null != url && url.length() > 0) {

	    System.out.println("filepath:" + url);
	    List<File> files1 = new ArrayList<File>();
	    File f = new File(url);
	    new java.io.File(url).getName();
	    files1.add(f);
	    UITorrentTable.getTorrentTable().setVisible(false);
	    String filename = new java.io.File(url).getName();
	    Torrent torrentObject = new Torrent(filename.substring(0, filename
		    .indexOf(".")), files1, 1500);
	    ErlangModelDataAccessObject.getTorrents().add(torrentObject);

	    new TableItem(UITorrentTable.getTorrentTable(), SWT.LEFT)
		    .setText(new String[] { torrentObject.getName(),
			    "" + torrentObject.getSize(),
			    "" + torrentObject.getDownloadSpeed(),
			    "" + torrentObject.getAmountDownloaded(),
			    "" + torrentObject.getFiles().size(), "zjh1",
			    "zjh2", "zjh3", "zjh4", "zjh5", "zjh6" });

	    System.out.println("\t" + torrentObject.toString() + " active:"
		    + torrentObject.isActive());
	    UITorrentTable.initData();

	    UITorrentTable.getTorrentTable().setVisible(true);

	}

    }

    
   

}
