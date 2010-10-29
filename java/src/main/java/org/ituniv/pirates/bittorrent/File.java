package org.ituniv.pirates.bittorrent;

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
