package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.TrayItem;
import org.ituniv.pirates.bittorrent.UI.util.ImageLoader;
import org.ituniv.pirates.bittorrent.UI.util.NullImageException;



/**
 * UI Tray
 * @author Jianping Wang
 *
 */

public class UITray {
	private Shell shell;
	private TrayItem trayItem;
	private boolean isAnotherClick=false;

	public UITray(Shell shell) {
		this.shell = shell;
		
	}
	
	public void open(){
		createTray();
	}

	private void createTray() {
		final ToolTip tip = new ToolTip(shell, SWT.BALLOON
				| SWT.ICON_INFORMATION);
		final Tray tray = shell.getDisplay().getSystemTray();
		if (tray != null) {
			trayItem = new TrayItem(tray, SWT.NONE);
			trayItem.setToolTipText(getTrayMessage());
			try {
				trayItem
						.setImage(new Image(shell.getDisplay(), ImageLoader.addImage("image/oursbt1.png")));
			} catch (NullImageException e1) {
				e1.printStackTrace();
			}
			trayItem.setToolTip(tip);
			
			final Menu menu=new Menu(shell,SWT.POP_UP);
			final MenuItem showMenu=new MenuItem(menu,SWT.PUSH);
			showMenu.setText("S&how");
			showMenu.addSelectionListener(new SelectionAdapter(){
				public void widgetSelected(SelectionEvent e){
					boolean showFlag=shell.isVisible();
					shell.setVisible(showFlag);
					showMenu.setText(showFlag?"&Show":"&Hide");
					tip.setText("Pirates BitTorrent");
					tip.setMessage("right click");
					tip.setVisible(true);
				}
			});
			
			MenuItem exitItem=new MenuItem(menu,SWT.PUSH);
			exitItem.setText("E&xit");
			exitItem.addSelectionListener(new SelectionAdapter(){
				public void widgetSelected(SelectionEvent e){
					System.exit(0);
				}
			});


			trayItem.addListener(SWT.Selection, new Listener(){
				public void handleEvent(Event e){
					if(!isAnotherClick){
						shell.setVisible(true);
						isAnotherClick=true;
					}else{
						shell.setVisible(false);
						isAnotherClick=false;
					}
				}
			});
			
			
			
			trayItem.addListener(SWT.MenuDetect, new Listener(){
				public void handleEvent(Event e){
					menu.setVisible(true);
				}
			});

			shell.addShellListener(new ShellAdapter() {
				public void shellClosed(ShellEvent e) {
					e.doit = false;
					shell.setVisible(false);
					showMenu.setText("S&how");
					tip.setText("Pirates BitTorrent");
					tip.setMessage(getTrayMessage());
					tip.setVisible(true);

				}
			});
		}
	}
	
	/**
	 * 
	 * @return 
	 */
	public TrayItem getTrayItem(){
		return this.trayItem;
	}
	
	/**
	 * 
	 * @return
	 */
	private String getTrayMessage(){
		return "Pirates BitTorrent\n0(0)downing,0(0)seeding\n" +
		"0.0Kb/s down ,0.0Kb/s up";
		//TODO
	}
}
