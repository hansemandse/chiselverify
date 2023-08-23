package chiselverify.approximation

import chiselverify.approximation.Metrics.MSE

/** 
  * This implementation assumes that all compound metrics are instantaneous.
  * It also does not currently distinguish between absolute and relative
  * metrics, per say.
  * 
  * Beware that the current interface supports samples only in either
  * `BigInt` or `Double`. All `BigInt`s are converted to `Double` before
  * any computations are carried out.
  */
object CompoundMetrics {

  /** 
    * Introduce some type abbreviations here for clarity.
    */
  type Signal = Iterable[BigInt]
  type Image  = Iterable[Iterable[Iterable[BigInt]]]

  /** 
    * Implicit operations on signals for simplifying code.
    */
  private[chiselverify] implicit class SignalOps(signal: Signal) {
    /** 
      * Checks if a signal is compatible for computations with a given signal
      * @return true iff the image is compatible with `that`
      */
    def isCompatibleWith(that: Signal): Boolean = this.signal.size == that.size
  }

  /** 
    * Implicit operations on images for simplifying code.
    */
  private[chiselverify] implicit class ImageOps(image: Image) {
    /** 
      * Checks if an image is rectangular and that all its pixels have
      * the same dimensions
      * @return true iff the image is valid
      */
    def isValid: Boolean = this.image.isEmpty || {
      val dimsValid   = this.image.tail.forall(_.size == this.image.head.size)
      val pixelsValid = this.image.head.isEmpty || {
        val flat  = this.image.flatten
        flat.tail.forall(_.size == flat.head.size)
      }
      dimsValid && pixelsValid
    }

    /** 
      * Checks if an image is compatible for computations with a given image
      * @return true iff the image is compatible with `that`
      */
    def isCompatibleWith(that: Image): Boolean = this.image.size == that.size && this.image.zip(that).forall {
      case (row1, row2) => row1.size == row2.size && row1.zip(row2).forall {
        case (pxl1, pxl2) => pxl1.size == pxl2.size
      }
    }
  }

  /** 
    * Represents a generic compound error metric
    */
  private[chiselverify] sealed trait CompoundMetric {
    def maxVal: Option[Double]

    /** 
      * Checks if the metric is constrained
      * @return true if the metric has a maximum value
      */
    final def isConstrained: Boolean = maxVal.isDefined

    /** 
      * Checks if the value of the metric is less than a given maximum
      * @param err value of the metric
      * @return true if `maxVal` is `None` or if `err` is less than `maxVal`
      */
    final def check(err: Double): Boolean = maxVal == None || err <= maxVal.get
  }

  /** 
    * Represents a generic compound metric operating on images
    * @param pixelWidth bit-width of pixels in the image
    */
  private[chiselverify] sealed abstract class ImageMetric(maxVal: Option[Double], pixelWidth: Int)
    extends CompoundMetric {
    require(pixelWidth > 0, "pixel width must be positive")

    /** 
      * Maximum integral pixel value with `pixelWidth`-bit pixels
      */
    final val Max: BigInt = (BigInt(1) << pixelWidth) - 1

    /** 
      * Computes the value of the metric given two images
      * @param vs1 first image
      * @param vs2 second image
      * @return value of the metric
      */
    def compute(vs1: Image, vs2: Image): Double

    /** 
      * Checks if the value of the metric given two samples is less than its maximum
      * @return `check(compute(vs1, vs2))`
      */
    final def check(vs1: Image, vs2: Image): Boolean = check(compute(vs1, vs2))
  }

  /** 
    * Represent a generic compound metric operating on uni-dimensional signals
    */
  private[chiselverify] sealed abstract class SignalMetric(maxVal: Option[Double])
    extends CompoundMetric {
    /** 
      * Computes the value of the metric given two signals
      * @param vs1 first signal
      * @param vs2 second signal
      * @return value of the metric
      */
    def compute(vs1: Signal, vs2: Signal): Double

    /** 
      * Checks if the value of the metric given two samples is less than its maximum
      * @return `check(compute(vs1, vs2))`
      */
    final def check(vs1: Signal, vs2: Signal): Boolean = check(compute(vs1, vs2))
  }

  /** 
    * Mean squared error metric for images
    * $$mse(vs1, vs2) = sum[i=0..len(vs1)-1](sum[j=0..len(vs1[i])-1]((vs2[i][j] - vs1[i][j]) ** 2)
    *                 / len(vs1[i])) / len(vs1)$$
    * 
    * @param maxVal [Optional] maximum value of the metric
    */
  final case class ImageMSE(maxVal: Option[Double] = None) extends ImageMetric(maxVal, 8) {
    def compute(vs1: Image, vs2: Image): Double = {
      require(vs1.isValid && vs2.isValid, "the images must be valid")
      require(vs1.isCompatibleWith(vs2), "the images must have identical dimensions")
      val vs1Flat = vs1.flatten
      val vs2Flat = vs2.flatten
      vs1Flat.zip(vs2Flat).foldLeft(0.0) { case (acc, (pxl1, pxl2)) =>
        acc + pxl1.zip(pxl2).foldLeft(0.0) { case (pxlAcc, (c1, c2)) =>
          pxlAcc + scala.math.pow((c2 - c1).toDouble / Max.toDouble, 2.0)
        }
      } / vs1Flat.size
    }
  }
  case object ImageMSE {
    /** 
      * Creates a new unconstrained MSE metric
      */
    def apply(): ImageMSE = new ImageMSE

    /** 
      * Creates a new constrained MSE metric
      * @param maxVal maximum value of the metric
      */
    def apply(maxVal: Double): ImageMSE = new ImageMSE(Some(maxVal))

    /** 
      * Creates a new MSE metric and computes its value on the two given images
      * @param vs1 first image
      * @param vs2 second image
      * @return value of the metric
      */
    def apply(vs1: Image, vs2: Image): Double = (new ImageMSE).compute(vs1, vs2)
  }

  /** 
    * Peak signal-to-noise ratio metric
    * $$psnr(vs1, vs2) = 20 * log[10](MAX / sqrt(MSE(vs1, vs2)))$$
    * https://en.wikipedia.org/wiki/Peak_signal-to-noise_ratio
    * 
    * @param maxVal [Optional] maximum value of the metric
    * @param pixelWidth [Optional] bit-width of the pixels in the image (defaults to 8)
    */
  final case class PSNR(maxVal: Option[Double] = None, pixelWidth: Int = 8)
    extends ImageMetric(maxVal, pixelWidth) {
    private val mse: ImageMSE = new ImageMSE
    def compute(vs1: Image, vs2: Image): Double = {
      require(vs1.isValid && vs2.isValid, "the images must be valid")
      require(vs1.isCompatibleWith(vs2), "the images must have identical dimensions")
      20 * scala.math.log10(1.0 / scala.math.sqrt(mse.compute(vs1, vs2)))
    }
  }
  case object PSNR {
    /** 
      * Creates a new unconstrained PSNR metric
      */
    def apply(): PSNR = new PSNR

    /** 
      * Creates a new unconstrained PSNR metric
      * @param pixelWidth bit-width of the pixels in the image
      */
    def apply(pixelWidth: Int): PSNR = new PSNR(pixelWidth=pixelWidth)

    /** 
      * Creates a new constrained PSNR metric
      * @param maxVal maximum value of the metric
      */
    def apply(maxVal: Double): PSNR = new PSNR(Some(maxVal))

    /** 
      * Creates a new constrained PSNR metric
      * @param maxVal maximum value of the metric
      * @param pixelWidth bit-width of the pixels in the image
      */
    def apply(maxVal: Double, pixelWidth: Int): PSNR = new PSNR(Some(maxVal), pixelWidth)

    /** 
      * Creates a new PSNR metric and computes its value on the two given images
      * @param vs1 first image
      * @param vs2 second image
      * @return value of the metric
      */
    def apply(vs1: Image, vs2: Image): Double = (new PSNR).compute(vs1, vs2)

    /** 
      * Creates a new PSNR metric and computes its value on the two given images
      * @param vs1 first image
      * @param vs2 second image
      * @param pixelWidth bit-width of the pixels in the image
      * @return value of the metric
      */
    def apply(vs1: Image, vs2: Image, pixelWidth: Int): Double = {
      (new PSNR(pixelWidth=pixelWidth)).compute(vs1, vs2)
    }
  }

  /** 
    * Structural similarity metric
    * https://en.wikipedia.org/wiki/Structural_similarity
    * 
    * @param maxVal [Optional] maximum value of the metric
    * @param pixelWidth [Optional] bit-width of the pixels in the image (defaults to 8)
    */
  final case class SSIM(maxVal: Option[Double] = None, pixelWidth: Int = 8)
    extends ImageMetric(maxVal, pixelWidth) {
    /** 
      * Stability constants
      */
    private val k1: Double = 0.01
    private val k2: Double = 0.03
    private val c1: Double = scala.math.pow(k1 * Max.toDouble, 2.0)
    private val c2: Double = scala.math.pow(k2 * Max.toDouble, 2.0)

    /** 
      * Generate a Gaussian window of size `n` by `n` with standard deviation 1.5
      * 
      * @param n side length of the window
      * 
      * Follows the definition used in Wang et al [2004]
      * Image Quality Assessment: From Error Visibility to Structural Similarity
      */
    private def gaussian(n: Int): Iterable[Iterable[Double]] = {
      require(n > 0)
      val std = 1.5
      val denum  = 2 * scala.math.pow(std, 2.0)
      val coords = (0 until n).map(i => i - ((n-1)/2) - (if ((n & 0x1) == 0) 0.5 else 0.0))
      val gauss  = coords.map { x => coords.map { y =>
        scala.math.exp(-((scala.math.pow(x, 2.0) + scala.math.pow(y, 2.0)) / denum))
      }}
      val sum = gauss.foldLeft(0.0) { case (acc, row) => acc + row.sum }
      gauss.map(row => row.map(_ / sum))
    }

    /** 
      * Compute the element-wise product of two identically-dimensioned images
      * 
      * @param vs1 first image
      * @param vs2 second image
      * @return the element-wise product of `vs1` and `vs2`
      */
    private def mm(vs1: Iterable[Iterable[Double]],
                   vs2: Iterable[Iterable[Double]]): Iterable[Iterable[Double]] = {
      require(vs1.size == vs2.size && vs1.zip(vs2).forall { case (row1, row2) => row1.size == row2.size },
        "the matrices must have identical dimensions")
      vs1.zip(vs2).map { case (row1, row2) => row1.zip(row2).map { case (pxl1, pxl2) => pxl1 * pxl2 }}
    }

    /** 
      * Compute the cross-correlation of two identically-dimensioned images
      * 
      * @param vs1 first image
      * @param vs2 second image
      * @return the cross-correlation of `vs1` and `vs2`
      * 
      * Assumes the implementation defined at
      * https://observablehq.com/@lemonnish/cross-correlation-of-2-matrices
      */
    private def xcorr(vs1: Iterable[Iterable[Double]],
                      vs2: Iterable[Iterable[Double]]): Double = {
      mm(vs1, vs2).flatten.sum
    }

    /** 
      * Compute the sample mean of an image
      * 
      * @param vs image to compute over
      * @return the mean of the elements in `vs`
      */
    private def mean(vs: Iterable[Iterable[Double]]): Double = {
      val flat = vs.flatten
      flat.sum / flat.size
    }

    /** 
      * Compute the sample variance of an image
      * 
      * @param vs image to compute over
      * @param meanOpt [Optional] mean of the image values
      * @return the variance of the elements in `vs`
      */
    private def variance(vs: Iterable[Iterable[Double]], meanOpt: Option[Double] = None): Double = {
      val avg  = meanOpt.getOrElse(mean(vs))
      val flat = vs.flatten
      flat.foldLeft(0.0) { case (acc, c) => acc + scala.math.pow(c - avg, 2.0) } / flat.size
    }

    /** 
      * Generates windows of size `n` by `n` of an inpute image
      * 
      * @param vs image to generate windows from
      * @param n side length of the windows
      * @return a list of windows of size `n` by `n` sliced from `vs`
      * 
      * @note Assumes the image is at least `n` by `n` pixels
      */
    private def windows(vs: Iterable[Iterable[Double]], n: Int):
      Iterable[Iterable[Iterable[Double]]] = {
      assume(vs.size >= n && vs.forall(_.size >= n))
      // If the image is empty, there is no need to compute anything here
      if (vs.isEmpty) {
        Iterable.empty[Iterable[Iterable[Double]]]
      } else {
        val verts = vs.size - n + 1
        val horzs = vs.head.size - n + 1
        (0 until verts).flatMap { rOffset =>
          (0 until horzs).map { cOffset =>
            vs.view.slice(rOffset, rOffset + n).map(_.slice(cOffset, cOffset + n))
          }
        }
      }
    }

    def compute(vs1: Image, vs2: Image): Double = {
      require(vs1.isValid && vs2.isValid, "the images must be valid")
      require(vs1.isCompatibleWith(vs2), "the images must have identical dimensions")
      // If the image is empty, there is no need to compute anything here
      if (vs1.isEmpty || vs2.isEmpty || vs1.head.isEmpty || vs2.head.isEmpty) {
        1.0
      } else {
        // Check that the color dimensionality is acceptable
        val cs = vs1.head.head.size
        assume(Set(1, 3, 4).contains(cs), "the images must have color dimensionality 1, 3, or 4")

        // Pick a suitable value for `n` (at most 11, otherwise minimum of image's dimensions)
        val n = scala.math.min(11, scala.math.min(vs1.size, vs1.head.size))

        // Compute the luma channels of the images
        val (vs1Luma, vs2Luma) = if (cs == 1) {
          (vs1.map(_.map(_.head.toDouble)),
           vs2.map(_.map(_.head.toDouble)))
        } else {
          val lumaCoefs = Seq(.2126, .7152, .0722)
          (vs1.map(_.map(_.take(3).zip(lumaCoefs).map { case (c, coef) => c.toDouble * coef }.sum)),
           vs2.map(_.map(_.take(3).zip(lumaCoefs).map { case (c, coef) => c.toDouble * coef }.sum)))
        }

        // Generate the Gaussian window needed for multiplications
        val gauss = gaussian(n)

        // Slice the images into `n` by `n` windows
        val vs1Wndws = windows(vs1Luma, n)
        val vs2Wndws = windows(vs2Luma, n)

        // Compute the SSIM score for each pair of windows
        val ssims = vs1Wndws.zip(vs2Wndws).map { case (wndw1, wndw2) =>
          // Apply the Gaussian to both the windows
          val gWndw1 = mm(wndw1, gauss)
          val gWndw2 = mm(wndw2, gauss)

          // Compute the elements of the formula
          val mu1  = mean(gWndw1)
          val var1 = variance(gWndw1, Some(mu1))
          val mu2  = mean(gWndw2)
          val var2 = variance(gWndw2, Some(mu2))
          val xsigma = xcorr(gWndw1, gWndw2)

          // Compute the final score
          ((2 * mu1 * mu2 + c1) * (2 * xsigma + c2)) / ((mu1 * mu1 + mu2 * mu2 + c1) * (var1 + var2 + c2))
        }

        // Compute and return the mean SSIM score
        ssims.sum / ssims.size
      }
    }
  }
  case object SSIM {
    /** 
      * Creates a new unconstrained SSIM metric
      */
    def apply(): SSIM = new SSIM

    /** 
      * Creates a new unconstrained SSIM metric
      * @param pixelWidth bit-width of the pixels in the image
      */
    def apply(pixelWidth: Int): SSIM = new SSIM(pixelWidth=pixelWidth)

    /** 
      * Creates a new constrained SSIM metric
      * @param maxVal maximum value of the metric
      */
    def apply(maxVal: Double): SSIM = new SSIM(Some(maxVal))

    /** 
      * Creates a new constrained SSIM metric
      * @param maxVal maximum value of the metric
      * @param pixelWidth bit-width of the pixels in the image
      */
    def apply(maxVal: Double, pixelWidth: Int): SSIM = new SSIM(Some(maxVal), pixelWidth)

    /** 
      * Creates a new SSIM metric and computes its value on the two given images
      * @param vs1 first image
      * @param vs2 second image
      * @return value of the metric
      */
    def apply(vs1: Image, vs2: Image): Double = (new SSIM).compute(vs1, vs2)

    /** 
      * Creates a new SSIM metric and computes its value on the two given images
      * @param vs1 first image
      * @param vs2 second image
      * @param pixelWidth bit-width of the pixels in the image
      * @return value of the metric
      */
    def apply(vs1: Image, vs2: Image, pixelWidth: Int): Double = {
      (new SSIM(pixelWidth=pixelWidth)).compute(vs1, vs2)
    }
  }

  /** 
    * Signal-to-noise ratio metric
    * $$snr(vs1, vs2) = 10 * log[10]((sum[i=0..len(vs1)-1](vs2[i] ** 2) / len(vs2)) / MSE(vs1, vs2))$$
    * https://en.wikipedia.org/wiki/Signal-to-noise_ratio
    * 
    * @param maxVal [Optional] maximum value of the metric
    */
  final case class SNR(maxVal: Option[Double] = None) extends SignalMetric(maxVal) {
    def compute(vs1: Signal, vs2: Signal): Double = {
      require(vs1.isCompatibleWith(vs2), "the signals must have identical size")
      (vs2.foldLeft(0.0) { case (acc, smpl) => acc + scala.math.pow(smpl.toDouble, 2.0) } / vs2.size) / MSE(vs1, vs2)
    }
  }
  case object SNR {
    /** 
      * Creates a new unconstrained SNR metric
      */
    def apply(): SNR = new SNR

    /** 
      * Creates a new constrained SNR metric
      * @param maxVal maximum value of the metric
      */
    def apply(maxVal: Double): SNR = new SNR(Some(maxVal))

    /** 
      * Creates a new SNR metric and computes its value on the two given signals
      * @param vs1 first signal
      * @param vs2 second signal
      * @return value of the metric
      */
    def apply(vs1: Signal, vs2: Signal): Double = (new SNR).compute(vs1, vs2)
  }
}
