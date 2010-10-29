
package org.ituniv.pirates.bittorrent;

import java.util.List;

public class Torrent
{
	private Runnable fileDownloader = new Runnable()
	{
		@Override
		public void run()
		{
			// Randomize the characteristics of the torrent and start
			// downloading.

			float downloadSpeed = 200 + (float) Math.random() * 500;

			while(true)
			{
				if(isActive && !isDownloaded())
				{
					// Download a little bit and modify the download speed.

					amountDownloaded += downloadSpeed;

					if(amountDownloaded >= size)
					{
						amountDownloaded = size;
						Torrent.this.downloadSpeed = 0;
					}
					else
					{
						Torrent.this.downloadSpeed =
								downloadSpeed + (float) Math.random() * 30 - 15;
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

	private String name;

	private boolean isActive = true;

	private List<File> files;

	private float downloadSpeed = 0;

	private float amountDownloaded = 0;

	private final float size;

	public Torrent(String name, List<File> files, float size)
	{
		this.name = name;
		this.files = files;
		this.size = size;

		Thread thread = new Thread(fileDownloader);

		thread.setDaemon(true);
		thread.start();
	}

	public float getAmountDownloaded()
	{
		return amountDownloaded;
	}

	public float getSize()
	{
		return size;
	}

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public List<File> getFiles()
	{
		return files;
	}

	public float getDownloadSpeed()
	{
		return downloadSpeed;
	}

	public void setDownloadSpeed(float downloadSpeed)
	{
		this.downloadSpeed = downloadSpeed;
	}

	public boolean isActive()
	{
		return isActive;
	}

	public void setActive(boolean isActive)
	{
		this.isActive = isActive;
	}

	public boolean isDownloaded()
	{
		return size == amountDownloaded;
	}

	public String toString()
	{
		return "Torrent: [ name = \"" + name + "\", amountDownloaded = \""
				+ amountDownloaded + "\", size = \"" + size
				+ "\", downloadSpeed = \"" + downloadSpeed
				+ "\", isDownloaded = \"" + isDownloaded() + "\" ]";
	}
}
