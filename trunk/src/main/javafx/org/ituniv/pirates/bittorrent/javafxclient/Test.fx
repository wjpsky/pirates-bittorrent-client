package org.ituniv.pirates.bittorrent.javafxclient;

import javafx.scene.Scene;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;
 
Stage {
	title: "Pirates BitTorrent JavaFX Client"
	width: 250
	height: 100
	scene: Scene {
		content: [
			Text {
				content: "Hello World!"
				x: 0
				y: 10
				font: Font {
					name: "Sans Serif"
					size: 14
				}
			}
		]
	}
}
