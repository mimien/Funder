import scalafx.scene.Scene
import scalafx.scene.shape.{Arc, Line, Rectangle, Shape}
import scalafx.application.JFXApp
import scalafx.scene.paint.Color

class DrawFrame(list: List[Shape]) extends JFXApp {
  stage = new JFXApp.PrimaryStage {
    title.value = "Funder"
    width = 1000
    height = 1000
    scene = new Scene {
      fill = Color.White
      content = list
    }
  }
}