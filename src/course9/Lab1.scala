import java.awt._
import java.awt.event._
import java.awt.geom._
import javax.swing._

// Geometric shapes

class Point(val x: Double, val y: Double) {
  def interpolate(other: Point, t: Double) = new Point((1 - t) * x + t * other.x, (1 - t) * y + t * other.y)
}

abstract class Drawable(var center: Point = null, var transparency: Double = 0) {
  def draw(g: Graphics) {
    g.setColor(new Color(0F, 0F, 0F, 1F - transparency.toFloat))
    g.asInstanceOf[Graphics2D].draw(boundary)
  }

  def boundary: Shape
  
  def -->(to: Drawable)(implicit d: Drawing): Unit =
    d += new Arrow(this, to)
}

class Circle(c: Point, var radius: Double) extends Drawable(c) {
  def boundary = new Ellipse2D.Double(center.x - radius, center.y - radius, 2 * radius, 2 * radius)
}

class Rectangle(c: Point, var width: Double, var height: Double) extends Drawable(c) {
  def boundary = new Rectangle2D.Double(center.x - width / 2, center.y - height / 2, width, height)
}

class Arrow(from: Drawable, to: Drawable) extends Drawable {
  private def intersection(s: Shape, p1: Point, p2: Point): Point = {
    val threshold = 0.1
    val mid = new Point((p1.x + p2.x) / 2, (p1.y + p2.y) / 2)
    val dx = p1.x - p2.x
    val dy = p1.y - p2.y
    if (dx * dx + dy * dy < 0.1) mid
    else if (s.contains(mid.x, mid.y)) intersection(s, mid, p2)
    else intersection(s, p1, mid)
  }

  def start = {
    val fromBoundary = from.boundary
    val fromBounds = fromBoundary.getBounds2D
    val toBounds = to.boundary.getBounds2D
    intersection(fromBoundary, new Point(fromBounds.getCenterX, fromBounds.getCenterY),
      new Point(toBounds.getCenterX, toBounds.getCenterY))
  }

  def end = {
    val toBoundary = to.boundary
    val toBounds = toBoundary.getBounds2D
    val fromBounds = from.boundary.getBounds2D
    intersection(toBoundary, new Point(toBounds.getCenterX, toBounds.getCenterY),
      new Point(fromBounds.getCenterX, fromBounds.getCenterY))
  }

  def boundary = {
    val p = new Path2D.Double
    val s = start
    val e = end

    p.moveTo(s.x, s.y)
    p.lineTo(e.x, e.y)
    val dx = s.x - e.x
    val dy = s.y - e.y
    val d = Math.sqrt(dx * dx + dy * dy)
    val a = 10 / d
    val b = 5 / d
    p.lineTo(e.x + a * dx + b * dy, e.y + a * dy - b * dx)
    p.moveTo(e.x, e.y)
    p.lineTo(e.x + a * dx - b * dy, e.y + a * dy + b * dx)
    p
  }
}

// Component

class Drawing extends JComponent {
  private val drawables = new scala.collection.mutable.ArrayBuffer[Drawable]

  def start(e: Effect) {
    val startTime = System.currentTimeMillis

    val timerListener = new ActionListener {
      override def actionPerformed(evt: ActionEvent) {
        val currentTime = System.currentTimeMillis
        val elapsedTime = currentTime - startTime
        if (elapsedTime < e.duration) {
          e.act(elapsedTime.toInt)
          repaint()
        }
      }
    }

    val delay = 10
    val timer = new Timer(delay, timerListener)
    timer.start
  }

  def +=(d: Drawable) {
    drawables += d;
    repaint()
  }

  override def getPreferredSize = new Dimension(500, 500)

  override def paintComponent(g: Graphics) {
    for (d <- drawables) {d.draw(g)}
  }
}

// Effects

object Effect {
  implicit def IntToIntEffect = new IntEffect(_)
}

abstract class Effect(val duration: Int) {
  def act(t: Int)

  def ==> = followedBy _

  def followedBy(e2: Effect) = new InOrderEffect(this, e2)

  def || = and _

  def and(e2: Effect) = new TogetherEffect(this, e2)

  def unary_- = reversed

  def reversed = new ReverseEffect(this)

  def completion(t: Int) = (1.0 * t / duration) min 1

  def times(n: Int): Effect = if (n == 1) this else new InOrderEffect(this, times(n - 1))
}

class MoveEffect(d: Drawable, to: Point, duration: Int) extends Effect(duration) {
  private var from: Point = null

  override def act(t: Int) {
    if (from == null) from = d.center
    d.center = from.interpolate(to, completion(t))
  }
}

class HideEffect(d: Drawable, duration: Int) extends Effect(duration) {
  override def act(t: Int) {
    d.transparency = completion(t)
  }
}

class InOrderEffect(e1: Effect, e2: Effect) extends Effect(e1.duration + e2.duration) {
  override def act(t: Int) {
    if (t < e1.duration) e1.act(t)
    else if (t < duration) e2.act(t - e1.duration)
  }
}

class TogetherEffect(e1: Effect, e2: Effect) extends Effect(math.max(e1.duration, e2.duration)) {
  override def act(t: Int) {
    if (t < e1.duration) e1.act(t)
    if (t < duration) e2.act(t)
  }
}

class ReverseEffect(e: Effect) extends Effect(e.duration) {
  override def act(t: Int) {
    if (t < e.duration) e.act(e.duration - t)
  }
}

class UpdateEffect(duration: Int, block: Double => Unit) extends Effect(duration) {
  override def act(t: Int): Unit = block(completion(t))
}

class IntEffect(i: Int) {
  require(i > 0)

  def times(e: Effect): Effect = {
    def _times(i: Int): Effect = if (i == 1) e else new InOrderEffect(e, _times(i - 1))

    _times(i)
  }
}


object Lab1 extends App {

  val f = new JFrame
  implicit val d = new Drawing
  
  f.add(d)
  f.pack()
  f.setVisible(true)

  val c1 = new Circle(new Point(200, 200), 25)
  d += c1
  val c2 = new Circle(new Point(400, 400), 30)
  d += c2
  //d += new Arrow(c1, c2)
  c1 --> c2

  // part 1
  //  val e1 = new MoveEffect(c1, new Point(400, 200), 8000)
  //  val e2 = new MoveEffect(c2, new Point(200, 200), 6000)
  //  val e3 = new HideEffect(c2, 3000)
  //  d.start(new InOrderEffect(e1, new TogetherEffect(e2, e3)))

  // part 2: cf. Effect class
  val e1 = new MoveEffect(c1, new Point(400, 200), 3000)
  val e2 = new MoveEffect(c2, new Point(200, 200), 3000)
  val e3 = new HideEffect(c2, 1000)

  //val effects = e1 followedBy (e2 and (e3 followedBy e3.reversed))

  // part 3
  val effects = e1 ==> (e2 || (e3 ==> -e3))
  //d.start(effects)


  // part 4
  def update(duration: Int)(block: Double => Unit): UpdateEffect = new UpdateEffect(duration, block)

  val e4 = update(1000) { t => c2.radius = 30.0 + 20.0 * t } // because of curryfication, the compiler cannot
  // infer the type...  And no way to tell him what the first update(d) returns !


  //d.start(e4 times 3)
  //d.start(new IntEffect(3) times e4)
  //d.start(new IntEffect(3) times e4)

  import Effect._

  d.start(3 times e4)
}
