package rwt.example

import javafx.application.Application
import javafx.fxml._
import javafx.event.ActionEvent
import javafx.stage.Stage
import javafx.scene.Scene
import javafx.scene.control.Label
import javafx.scene.control.Slider
import javafx.scene.Parent
import javafx.beans.binding.Bindings

import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.SourceDataLine

final class Controller {

  // FXML items... 
  @FXML private var sliderLabel : Label  = null
  @FXML private var freq        : Slider = null

  // for sound support
  private val sampleRate = 8000
  private val af = new AudioFormat(sampleRate.toFloat, 16, 1, true, true)

  @FXML private def playSound(ae : ActionEvent) {
      val line = AudioSystem.getSourceDataLine(af)
      line.open(af)
      line.start()
      val data = generateData(freq.getValue(), 1)
      line.write(data, 0, data.length)
      line.drain()
      line.close()
  }

  private def clip(orig: Double) = Math.min(127.0, Math.max(orig, -127.0)).toByte

  private def generateData(signalFreq: Double, seconds: Int) : Array[Byte] = {
        val samplingInterval = (sampleRate / signalFreq)
        Array.tabulate(seconds * sampleRate) { i =>
            val angle = 2.0 * Math.PI * i.toDouble / samplingInterval
            val dblAngle = 2.0 * Math.PI * i.toDouble / (2.0 * samplingInterval)
            val trpAngle = 2.0 * Math.PI * i.toDouble / (3.0 * samplingInterval)
            clip( (Math.sin(angle) * 127) +
                  (Math.sin(dblAngle) * 64) +
                  (Math.sin(trpAngle) * 32) )
        }
  }

  def initialize() : Unit = {
      System.out.println("hi")
      sliderLabel.textProperty().bind( Bindings.format("%.2f", freq.valueProperty()) )
  }

}

final class Main extends Application {
  override def start( stage : Stage ) : Unit = {
     stage.setTitle("Hello!")
     val root : Parent = FXMLLoader.load(getClass().getResource("/fxml/main.fxml"))
     stage.setScene( new Scene(root) )
     stage.sizeToScene()
     stage.show()
  }
}

final object Main {
   def main(args : Array[String]) : Unit = {
       Application.launch(classOf[Main], args: _*)
   }
}
