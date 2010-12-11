
package org.ituniv.pirates.bittorrent;

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

/**
 * Beginning of a Data Access Object to glue together the Java and Erlang code
 * bases. It will connect to an Erlang node and start polling for updates. Use
 * the get methods to get information about the status of the application.
 * 
 * @author Jon Kristensen
 */
public class ErlangModelDataAccessObject
{
	private static List<Torrent> torrents = new ArrayList<Torrent>();

	private volatile boolean isRunning = true;

	private ErlangModelState currentErlangModelState = new ErlangModelState();

	// Runnable to take care of polling the Erlang node for updates.
	
	// TODO: Reactive this when we have a state to parse.

	@SuppressWarnings("unused")
	private Runnable erlangUpdater = new Runnable()
	{
		@Override
		public void run()
		{
			// Create a node for the Java part of the project.

			OtpSelf node;

			try
			{
				node = new OtpSelf("javapbtc");
			}
			catch(IOException exception)
			{
				throw new RuntimeException(exception);
			}

			// Create a node for the Erlang part of the project.

			OtpPeer peer = new OtpPeer("erlangpbtc@localhost");

			// Establish a connection between the two.

			OtpConnection connection;

			try
			{
				connection = node.connect(peer);
			}
			catch(UnknownHostException exception)
			{
				throw new RuntimeException(exception);
			}
			catch(OtpAuthException exception)
			{
				throw new RuntimeException(exception);
			}
			catch(IOException exception)
			{
				throw new RuntimeException(exception);
			}

			// Assemble the data to send.

			OtpErlangObject[] data = new OtpErlangObject[0];

			while(isRunning)
			{
				synchronized(currentErlangModelState)
				{
					try
					{
						connection.sendRPC("model", "get_state", data);
					}
					catch(IOException exception)
					{
						throw new RuntimeException(exception);
					}

					try
					{
						System.out.println(connection.receiveRPC());
					}
					catch(OtpErlangExit exception)
					{
						throw new RuntimeException(exception);
					}
					catch(OtpAuthException exception)
					{
						throw new RuntimeException(exception);
					}
					catch(IOException exception)
					{
						throw new RuntimeException(exception);
					}
				}

				try
				{
					Thread.sleep(1000);
				}
				catch(InterruptedException exception)
				{
					throw new RuntimeException(exception);
				}
			}
		}
	};

	/**
	 * Constructor. Connects to the Erlang node and starts polling for updates.
	 */
	public ErlangModelDataAccessObject()
	{
		// new Thread(erlangUpdater).start();

		// Create a couple of mock object torrents.
		
		List<File> files0 = new ArrayList<File>();
		files0.add(new File("/home/jon/Books/The God delusion.pdf"));

		List<File> files1 = new ArrayList<File>();
		files0.add(new File("/home/jon/Books/Wicket in Action.pdf"));

		List<File> files2 = new ArrayList<File>();
		files0
				.add(new File(
						"/home/jon/Books/Professional XMPP - Programming with JavaScript and jQuery.pdf"));

		torrents.add(new Torrent("The God delusion", files0, 1500));
		torrents.add(new Torrent("Wicket in Action", files1, 10000));
		torrents.add(new Torrent(
				"Professional XMPP - Programming with JavaScript and jQuery",
				files2, 100000));
	}

	// TODO: This is the type of method that will be used by the view to get the
	// information needed to properly draw the GUI.

	// public String getSomething()
	// {
	// synchronized(currentErlangModelState)
	// {
	// return currentErlangModelState.getSomething();
	// }
	// }

	public static void main(String[] args)
	{
		// TODO: Wang: Try to run this and move this code to your view class.

		new ErlangModelDataAccessObject();
		List<Torrent> torrents = ErlangModelDataAccessObject.getTorrents();

		while(true)
		{
			System.out.println("TORRENT DETAILS:");
			for(Torrent torrent : torrents)
			{
				System.out.println("\t" + torrent+" active:"+torrent.getName());
			}
			

			try
			{
				Thread.sleep(1000);
			}
			catch(InterruptedException exception)
			{
				throw new RuntimeException(exception);
			}
		}
	}

	public static List<Torrent> getTorrents()
	{
		return torrents;
	}
}
