package org.ituniv.pirates.bittorrent.UI.util;

import java.io.InputStream;

/**

 * @author Jianping Wang
 *
 */
public class ImageLoader {
	
	public static InputStream  addImage(String path) throws NullImageException{
		InputStream in=ClassLoader.getSystemResourceAsStream(path);
		
		if(in!=null){
			return in; 
		}else{
			throw new NullImageException("wrong picture path");
		}
	}

}
