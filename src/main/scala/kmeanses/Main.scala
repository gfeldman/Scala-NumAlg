package kmeanses
import breeze.linalg.{DenseVector,linspace}
import breeze.plot.{Figure,plot}
import breeze.numerics.sigmoid
object Main extends App{
  
  val x = linspace(-4.0, 4.0, 200)
  val fig = Figure()
  val plt = fig.subplot(0)
  plt += plot(x,sigmoid(1.0*x),name="s1")
  plt += plot(x,sigmoid(2.0*x),name="s2")
  plt += plot(x,sigmoid(10.0*x),name="s10")
  plt.legend = true
  fig.refresh()
}