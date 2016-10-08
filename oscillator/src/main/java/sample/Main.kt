package sample

import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.Parent
import javafx.scene.Scene
import javafx.stage.Stage

class Main : Application() {

    @Throws(Exception::class)
    override fun start(primaryStage: Stage) {
        val root = FXMLLoader.load<Parent>(javaClass.getResource("sample.fxml"))
        primaryStage.title = "Sound Player"
        primaryStage.scene = Scene(root)
        primaryStage.sizeToScene()
        primaryStage.show()
    }

    companion object {

        @JvmStatic fun main(args: Array<String>) {
            launch(Main::class.java)
        }
    }
}
