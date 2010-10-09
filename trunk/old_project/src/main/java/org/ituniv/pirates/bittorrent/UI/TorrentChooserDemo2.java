package org.ituniv.pirates.bittorrent.UI;
import java.io.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 * Torrent Interface
 * @author Jianping Wang
 * @email wjpsky@gmail.com
 */
public class TorrentChooserDemo2 extends JFrame {
	static private String newline = "\n";

	public TorrentChooserDemo2() {
		super("Torrent Chooser Demo2");
		// log
		final JTextArea log = new JTextArea(30, 80);
		log.setMargin(new Insets(5, 5, 5, 5));
		log.setEditable(false);
		JScrollPane logScrollPane = new JScrollPane(log);
		JButton sendButton = new JButton("Choose a torrent file");
		sendButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				JFileChooser fc = new JFileChooser();
				fc.addChoosableFileFilter(new TorrentFilter());
				int returnVal = fc.showDialog(TorrentChooserDemo2.this,
						"Attach");

				if (returnVal == JFileChooser.APPROVE_OPTION) {
					File file = fc.getSelectedFile();
					log.append("Attaching file: " + file.getName() + "."
							+ newline);
				} else {
					log.append("Attachment cancelled by user." + newline);
				}
			}
		});

		Container contentPane = getContentPane();
		contentPane.add(sendButton, BorderLayout.NORTH);
		contentPane.add(logScrollPane, BorderLayout.CENTER);
	}

	public static void main(String[] args) {
		JFrame frame = new TorrentChooserDemo2();
		frame.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				System.exit(0);
			}
		});
		frame.pack();
		frame.setVisible(true);
	}
}
