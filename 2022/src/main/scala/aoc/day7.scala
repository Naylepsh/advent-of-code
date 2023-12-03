package aoc

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Map

object day7 extends App:
  case class File(size: Int)
  case class Directory(
      name: String,
      files: List[File],
      directories: List[Directory]
  )

  case class Command(call: String, output: List[String])

  object Command:
    def parseTerminalOutput(lines: List[String]) =
      val currentOutput = ArrayBuffer[String]()
      var currentCommand = ""
      val commands = ArrayBuffer[Command]()

      lines.foreach {
        case s"$$ $cmd" =>
          commands += Command(
            call = currentCommand,
            output = currentOutput.toList
          )
          currentOutput.clear()
          currentCommand = cmd
        case output =>
          currentOutput += output
      }

      if (currentOutput.length > 0)
        commands += Command(
          call = currentCommand,
          output = currentOutput.toList
        )

      commands.toList.tail

  def makeKey(cwd: Stack[String]): String =
    cwd.toList.reverse.mkString("/")

  def buildFileSystem(commands: List[Command]) =
    val cwd = Stack[String]()
    val fs = Map[String, Int]()

    commands.foreach { case Command(call, output) =>
      call match
        case s"cd .."   => cwd.pop()
        case s"cd $dir" => cwd.push(dir)
        case s"ls" =>
          val size = output
            .filterNot(_.startsWith("dir"))
            .map(out => out.split(" ").toList.head.toInt)
            .sum
          fs.addOne(makeKey(cwd) -> size)
    }

    fs

  def calculateSpaceUsage(fs: Map[String, Int]) =
    fs.map { case (path, size) =>
      val subDirectories = fs
        .filter { case (subDirPath, _) =>
          subDirPath.startsWith(path) && subDirPath != path
        }
        .map { case (_, size) =>
          size
        }
      path -> (size + subDirectories.sum)
    }

  val terminalOutput = getFileContent("./inputs/day7/data.txt").toList
  val commands = Command.parseTerminalOutput(terminalOutput)
  val fs = buildFileSystem(commands)
  val spaceUsage = calculateSpaceUsage(fs)

  def solveA(spaceUsage: Map[String, Int]): Int =
    val smallDirs = spaceUsage
      .filter { case (_, size) =>
        size <= 100000
      }
    smallDirs.map { case (_, size) =>
      size
    }.sum

  def solveB(spaceUsage: Map[String, Int]): Int =
    val totalSpace = 70000000
    val requiredSpaceForUpdate = 30000000
    val usedSpace = spaceUsage("/")
    val spaceNeeded = requiredSpaceForUpdate - (totalSpace - usedSpace)

    val (_, size) = spaceUsage
      .filter { case (_, size) =>
        size > spaceNeeded
      }
      .toList
      .sortBy { case (_, size) =>
        size
      }
      .head

    size

  solveB(spaceUsage)
