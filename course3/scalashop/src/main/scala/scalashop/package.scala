

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    def clampWidth(w: Int) = clamp(w, 0 ,src.width - 1)
    def clampHeight(h: Int) = clamp(h, 0, src.height - 1)
    var i = clampWidth(x - radius)
    var j = clampHeight(y - radius)
    var rgbaTuple = (0, 0, 0, 0)
    while (i <= clampWidth(x + radius)) {
      while (j <= clampHeight(y + radius)) {
        val rgba = src(i, j)
        rgbaTuple = (rgbaTuple._1 + red(rgba), rgbaTuple._2 + green(rgba), rgbaTuple._3 + blue(rgba), rgbaTuple._4 + alpha(rgba))
        j = j + 1
      }
      i = i + 1
      j = clampWidth(y - radius)
    }
    var diameter = ((clampWidth(x + radius) - clampWidth(x - radius) + 1)
      * (clampHeight(y + radius) - clampHeight(y - radius) + 1))
    rgba(rgbaTuple._1 / diameter, rgbaTuple._2 / diameter, rgbaTuple._3 / diameter, rgbaTuple._4 / diameter)
  }

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))

    def apply(x: Int, y: Int): RGBA = data(y * width + x)

    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

}
