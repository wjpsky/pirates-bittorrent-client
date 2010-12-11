package org.ituniv.pirates.bittorrent;

/**
 * A class used for the {@link Torrent} mock objects. 
 * 
 * @author Jon Kristensen
 */
public class File
{
	private String path;
	// private float size;
	// private float amountDownloaded;
	
	public File(String path)
	{
		this.path = path;
	}

	public String getPath()
	{
		return path;
	}
	
	public void setPath(String path)
	{
			this.path = path;
	}
}
