import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using

@main def lox(args: String*): Unit =
  if args.length > 1 then
    println("Usage: jlox [script]")
    System.exit(64)
  else if args.length == 1 then runFile(args(0))
  else runPrompt()

def runFile(path: String): Unit =
  run(Using(Source.fromFile(path))(_.mkString).get)

@tailrec
def runPrompt(): Unit =
  val line = readLine("> ")
  if line == null then return
  run(line)
  runPrompt()

def run(source: String): Unit =
  println(source)
