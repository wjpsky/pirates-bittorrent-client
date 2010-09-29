package org.ituniv.pirates.bittorrent.UI;

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class SWTTest
{
	public static void main(String[] args)
	{
		Display display = new Display();
		Shell shell = new Shell(display);
		
		// TODO: Add widgets here.
		
		shell.open();

		while(!shell.isDisposed())
		{
			if(!display.readAndDispatch())
			{
				display.sleep();
			}
		}
		display.dispose();
	}
}
