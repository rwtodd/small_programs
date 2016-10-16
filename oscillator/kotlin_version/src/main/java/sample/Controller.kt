package sample

import javafx.beans.binding.Bindings
import javafx.event.ActionEvent
import javafx.fxml.FXML
import javafx.fxml.Initializable
import javafx.scene.control.Label
import javafx.scene.control.Slider
import java.net.URL
import java.util.*
import javax.sound.sampled.AudioFormat
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.SourceDataLine

/**
 * The controller for the sample application.
 */
class Controller : Initializable {

    private val sampleRate = 8000
    private val af = AudioFormat(sampleRate.toFloat(),16,1,true,true)
    @FXML private var freq : Slider? = null
    @FXML private var sliderLabel : Label? = null

    override fun initialize(location: URL?, resources: ResourceBundle?) {
        sliderLabel!!.textProperty()!!.bind(
                Bindings.format("%.2f", freq!!.valueProperty())
        )
    }

    @FXML
    private fun playSound(ae: ActionEvent) {
        val line = AudioSystem.getSourceDataLine(af)
        line.open(af);
        line.start();
        //play Frequency = 200 Hz for 1 seconds
        play(line, generateData(freq!!.value, 1));
        line.drain();
        line.close();
    }

    private fun clip(orig: Double) = Math.min(127.0, Math.max(orig,-127.0)).toByte()


    private fun generateData(frequencyOfSignal: Double, seconds: Int): ByteArray {
        // total samples = (duration in second) * (samples per second)
        val sin = ByteArray(seconds * sampleRate)
        val samplingInterval = (sampleRate / frequencyOfSignal)
        for (i in sin.indices) {
            val angle = 2.0 * Math.PI * i.toDouble() / samplingInterval
            val dblAngle = 2.0 * Math.PI * i.toDouble() / (2.0 * samplingInterval)
            val trpAngle = 2.0 * Math.PI * i.toDouble() / (3.0 * samplingInterval)
            sin[i] = clip( (Math.sin(angle) * 127) +
                           (Math.sin(dblAngle) * 64) +
                           (Math.sin(trpAngle) * 32) )
        }
        return sin
    }

    private fun  play(line: SourceDataLine, data: ByteArray) {
        line.write(data, 0, data.size)
    }
}
