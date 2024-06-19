import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Using

object Lox:

  private var hadError = false

  @main
  def main(args: String*): Unit =
    if args.length > 1 then
      println("Usage: slox [script]")
      System.exit(64)
    else if args.length == 1 then runFile(args(0))
    else runPrompt()

  private def runFile(path: String): Unit =
    run(Using(Source.fromFile(path))(_.mkString).get)
    if hadError then sys.exit(65)

  @tailrec
  private def runPrompt(): Unit =
    val line = readLine("> ")
    if line == null then return
    run(line)
    hadError = false
    runPrompt()

  private def run(source: String): Unit =
    val scanner = Scanner(source)
    val tokens = scanner.scanTokens()

    tokens.foreach(println)

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  private def report(line: Int, where: String, message: String): Unit =
    println("[line " + line + "] Error" + where + ": " + message)
    hadError = true
