package aoc

object day6 extends App:
  trait Solution:
    val packetSize: Int

    def solve(inputPath: String): List[Int] =
      val messages = getFileContent(inputPath).toList
      messages.map { message =>
        val chars = message.toArray
        val marker = 0 to (chars.length - packetSize) find { start =>
          val packet = chars.slice(start, start + packetSize).toSet
          packet.size == packetSize
        }
        marker.get + packetSize
      }

  val initial = new Solution:
    override val packetSize: Int = 4

  val actual = new Solution:
    override val packetSize: Int = 14

  // val res = initial.solve("./inputs/day6/data.txt")
  val res = actual.solve("./inputs/day6/data.txt")
  println(res)
