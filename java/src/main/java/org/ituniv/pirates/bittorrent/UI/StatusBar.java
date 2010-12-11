package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

public class StatusBar {
	private SashForm sashForm;
	private Label statusBar;
	private Label separatorLabel;
	private Composite composite;
	public StatusBar(SashForm parentSashForm){
		this.sashForm=parentSashForm;
	}
	
	public void open(){
		composite=new Composite(sashForm,SWT.NONE);
		separatorLabel=new Label(composite,SWT.SEPARATOR|SWT.BORDER);
		separatorLabel.setLocation(490, 2);
		statusBar=new Label(composite,SWT.NONE);
		statusBar.setAlignment(SWT.CENTER);
		statusBar.setBounds(500, 2, 300, 100);
		statusBar.setText("Pirates BitTorrent Client");
		composite.layout();
	}
}
