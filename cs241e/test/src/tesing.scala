import cs241e.mips._

/**
 * Created by Jerrydream on 2015-09-17.
 */
class testing {

  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(i >= 0)
    require(i < twoTo(bits))
    var sequence: Seq[Boolean] = List()
    var num = i
    while (num != 0)
    {
      var remainder = i % 2
      if (remainder == 1)
      {
        sequence = sequence :+ true
      }
      else
      {
        sequence = sequence :+ false
      }
      num = num % 2
    }
    if (sequence.size == bits)
    {
      sequence
    }
    else
    {
      var sequence2: Seq[Boolean] = List()
      for(x <- 1 to bits - sequence.size)
      {
        sequence2 = sequence2 :+ false
      }
      for (x <- sequence)
      {
        sequence2 = sequence2 :+ x
      }
      sequence2
    }
  }

  val result = encodeUnsigned(1,1)
  println(result)

}
