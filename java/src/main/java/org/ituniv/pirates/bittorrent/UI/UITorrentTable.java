package org.ituniv.pirates.bittorrent.UI;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import org.ituniv.pirates.bittorrent.ErlangModelDataAccessObject;
import org.ituniv.pirates.bittorrent.Torrent;

/**
 * TorrentTable
 * 
 * @author Jianping Wang
 * 
 */
public class UITorrentTable {
	private SashForm sashForm;
	private static String text = "";

	// private List<DownloadManager> list;
	private static Table torrentTable;

	public UITorrentTable(SashForm sashForm) {
		this.sashForm = sashForm;
		divide();
	}

	private void divide() {

		SashForm childSashForm = new SashForm(sashForm, SWT.VERTICAL);
		createTorrentTable(childSashForm);
		new Thread(tableThread).start();// update every 3 second

	}

	/**
	 * 
	 * @param childSashForm
	 */
	private void createTorrentTable(SashForm childSashForm) {

		torrentTable = new Table(childSashForm, SWT.FULL_SELECTION);
		torrentTable.setLayout(new RowLayout(SWT.VERTICAL));
		torrentTable.setHeaderVisible(true);
		torrentTable.setLinesVisible(true);

		TableColumn nameColumn = new TableColumn(torrentTable, SWT.BORDER);
		nameColumn.setText("file name");
		nameColumn.setWidth(150);

		TableColumn sizeColumn = new TableColumn(torrentTable, SWT.BORDER);
		sizeColumn.setText("size");
		sizeColumn.setWidth(100);

		TableColumn doneColumn = new TableColumn(torrentTable, SWT.BORDER);
		doneColumn.setText("progress");
		doneColumn.setWidth(100);

		TableColumn downloadedColumn = new TableColumn(torrentTable, SWT.BORDER);
		downloadedColumn.setText("finished");
		downloadedColumn.setWidth(100);

		TableColumn statusColumn = new TableColumn(torrentTable, SWT.BORDER);
		statusColumn.setText("status");
		statusColumn.setWidth(100);

		TableColumn seedColumn = new TableColumn(torrentTable, SWT.BORDER);
		seedColumn.setText("torrent");
		seedColumn.setWidth(100);

		TableColumn peerColumn = new TableColumn(torrentTable, SWT.BORDER);
		peerColumn.setText("user");
		peerColumn.setWidth(100);

		TableColumn downSpeedColumn = new TableColumn(torrentTable, SWT.BORDER);
		downSpeedColumn.setText("download speed");
		downSpeedColumn.setWidth(100);

		TableColumn upSpeedColumn = new TableColumn(torrentTable, SWT.BORDER);
		upSpeedColumn.setText("upload speed");
		upSpeedColumn.setWidth(100);

		TableColumn upLoadColumn = new TableColumn(torrentTable, SWT.BORDER);
		upLoadColumn.setText("upload size");
		upLoadColumn.setWidth(100);

		TableColumn addTimeColumn = new TableColumn(torrentTable, SWT.BORDER);
		addTimeColumn.setText("when");
		addTimeColumn.setWidth(100);

		initData();

	}

	/**
	 * add "all data" to table
	 */
	public synchronized static void initData() {

		// getTorrentTable().removeAll();

		deleteAll();//

		/**
		 * add table data
		 */

		List<Torrent> list_torrents = ErlangModelDataAccessObject.getTorrents();

		for (Torrent torrent : list_torrents) {
			Torrent torrentObject = torrent;

			System.out.println("\t" + torrent + " active:" + torrent.getName());

			new TableItem(getTorrentTable(), SWT.LEFT).setText(new String[] {
					torrentObject.getName(), "" + torrentObject.getSize(),
					"" + torrentObject.getFiles().size(),
					"" + torrentObject.getAmountDownloaded(),
					"" + getStatus(torrentObject), "wjp1", "wjp2",
					"" + torrentObject.getDownloadSpeed(), "wjp4", "wjp5",
					"wjp6" });

		}

	}

	/**
	 * reload table data
	 */
	public synchronized static void DownloadTable() {
		deleteAll();//
		/**
		 * add table data
		 */

		List<Torrent> list_torrents = ErlangModelDataAccessObject.getTorrents();

		for (Torrent torrent : list_torrents) {
			Torrent torrentObject = torrent;

			System.out.println("\t" + torrent + " active:" + torrent.getName());

			if (!torrentObject.isDownloaded() && torrentObject.isActive()) {// un
				// finished
				new TableItem(getTorrentTable(), SWT.LEFT)
						.setText(new String[] { torrentObject.getName(),
								"" + torrentObject.getSize(),
								"" + torrentObject.getFiles().size(),
								"" + torrentObject.getAmountDownloaded(),
								"" + getStatus(torrentObject), "wjp1", "wjp2",
								"" + torrentObject.getDownloadSpeed(), "wjp4",
								"wjp5", "wjp6" });
			}
		}

	}

	/**
	 * Finished add finished data to table
	 */
	public synchronized static void FinishedTable() {
		deleteAll();//
		/**
		 * add table data
		 */

		List<Torrent> list_torrents = ErlangModelDataAccessObject.getTorrents();

		for (Torrent torrent : list_torrents) {
			Torrent torrentObject = torrent;

			System.out.println("\t" + torrent + " active:" + torrent.getName());

			if (torrentObject.isDownloaded() && torrentObject.isActive()) {// un
				// finished
				new TableItem(getTorrentTable(), SWT.LEFT)
						.setText(new String[] { torrentObject.getName(),
								"" + torrentObject.getSize(),
								"" + torrentObject.getFiles().size(),
								"" + torrentObject.getAmountDownloaded(),
								"" + getStatus(torrentObject), "wjp1", "wjp2",
								"" + torrentObject.getDownloadSpeed(), "wjp4",
								"wjp5", "wjp6" });
			}
		}

	}

	/**
	 * Finished reload "activity data" to table
	 */
	public synchronized static void ActivityTable() {
		deleteAll();//  
		/**
		 * add table data
		 */

		List<Torrent> list_torrents = ErlangModelDataAccessObject.getTorrents();

		for (Torrent torrent : list_torrents) {
			Torrent torrentObject = torrent;

			System.out.println("\t" + torrent + " active:" + torrent.getName());

			if (torrentObject.isActive()) {// unfinished
				new TableItem(getTorrentTable(), SWT.LEFT)
						.setText(new String[] { torrentObject.getName(),
								"" + torrentObject.getSize(),
								"" + torrentObject.getFiles().size(),
								"" + torrentObject.getAmountDownloaded(),
								"" + getStatus(torrentObject), "wjp1", "wjp2",
								"" + torrentObject.getDownloadSpeed(), "wjp4",
								"wjp5", "wjp6" });
			}
		}

	}

	/**
	 * Finished add stoped task to table
	 */
	public synchronized static void StopTable() {
		deleteAll();//	    
		/**  
		 * 
		 */

		List<Torrent> list_torrents = ErlangModelDataAccessObject.getTorrents();

		for (Torrent torrent : list_torrents) {
			Torrent torrentObject = torrent;

			System.out.println("\t" + torrent + " active:" + torrent.getName());

			if (!torrentObject.isActive()) {// unfinished
				new TableItem(getTorrentTable(), SWT.LEFT)
						.setText(new String[] { torrentObject.getName(),
								"" + torrentObject.getSize(),
								"" + torrentObject.getFiles().size(),
								"" + torrentObject.getAmountDownloaded(),
								"" + getStatus(torrentObject), "wjp1", "wjp2",
								"" + torrentObject.getDownloadSpeed(), "wjp4",
								"wjp5", "wjp6" });
			}
		}

	}

	/**
	 * get status
	 * 
	 * @param torrent
	 * @return
	 */
	public static String getStatus(Torrent torrent) {
		String status = "download";
		if (torrent.isActive()) {
			status = "Activity";
		} else if (!torrent.isActive()) {
			status = "stop";
		}

		return status;

	}

	public synchronized static void delete() {

		// ErlangModelDataAccessObject.getTorrents().remove(torrentTable.getItem(torrentTable.getSelectionIndex()));//

		System.out.println("nnnn:" + torrentTable.getSelectionIndex());

		if (torrentTable.getSelectionIndex() > -1) {
			TableItem item = torrentTable.getItem(torrentTable
					.getSelectionIndex());
			deleteTorrent(item);
			torrentTable.remove(torrentTable.getSelectionIndices());
		}
	}

	public static void deleteTorrent(TableItem item) {
		List<Torrent> list = ErlangModelDataAccessObject.getTorrents();

		for (int i = 0; i < list.size(); i++) {

			Torrent torrent = list.get(i);

			if (torrent.getName().equals(item.getText())) {

				list.remove(i);
			}

		}

	}

	public static void StopTorrent() {

		if (torrentTable.getSelectionIndex() > -1) {
			TableItem item = torrentTable.getItem(torrentTable
					.getSelectionIndex());
			List<Torrent> list = ErlangModelDataAccessObject.getTorrents();

			for (int i = 0; i < list.size(); i++) {

				Torrent torrent = list.get(i);

				if (torrent.getName().equals(item.getText())) {

					torrent.setActive(false);
				}

			}

		}

	}

	public static void BeginTorrent() {

		if (torrentTable.getSelectionIndex() > -1) {
			TableItem item = torrentTable.getItem(torrentTable
					.getSelectionIndex());
			List<Torrent> list = ErlangModelDataAccessObject.getTorrents();

			for (int i = 0; i < list.size(); i++) {

				Torrent torrent = list.get(i);

				if (torrent.getName().equals(item.getText())) {

					torrent.setActive(true);
				}

			}

		}

	}

	public static void setTableValues() {

		if (text.equals("ALL")) {

			initData();//
			System.out.println("SelectionEvent:" + text);
		} else if (text.equals("download")) {
			;
			DownloadTable();

		} else if (text.equals("Finished")) {
			FinishedTable();
			System.out.println("SelectionEvent:" + text);

		} else if (text.equals("Activity")) {
			ActivityTable();
			System.out.println("SelectionEvent:" + text);

		} else if (text.equals("stop")) {
			StopTable();
			System.out.println("SelectionEvent:" + text);

		}
		System.out.println("SelectionEvent:" + text);

	}

	public Runnable tableThread = new Runnable() {
		@Override
		public void run() {
			while (true) {

				Display.getDefault().asyncExec(new Runnable() {
					public void run() {

						setTableValues();

					}
				});

				try {
					Thread.sleep(3000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}

		}
	};

	public synchronized static void deleteAll() {

		if (torrentTable != null)
			torrentTable.removeAll();
	}

	/**
	 * @return the torrentTable
	 */
	public static Table getTorrentTable() {
		return torrentTable;
	}

	/**
	 * @param torrentTable
	 *            the torrentTable to set
	 */
	public void setTorrentTable(Table torrentTable) {
		this.torrentTable = torrentTable;
	}

	/**
	 * @return the text
	 */
	public String getText() {
		return text;
	}

	/**
	 * @param text
	 *            the text to set
	 */
	public static void setText(String tex) {
		text = tex;
	}

}
