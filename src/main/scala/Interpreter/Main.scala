package Interpreter

import java.io.InputStreamReader

import _root_.jline.console.ConsoleReader
import Interpreter.Lisp._

import scala.annotation.tailrec

object Main extends App {
  val inputStream = new InputStreamReader(System.in, "UTF-8")
  val b = new ConsoleReader()
  b.setPrompt("lisp> ")
  val skip = 2

  class UnmatchedParenthesisException(message:String = "parens don't match", cause: Throwable = null) extends Exception

  def countParens(s:String):Int = {
    @tailrec
    def acc(str:String, c: Int): Int = {
      if(str.length > 0){
        if(str.head == '(') acc(str.tail, c + 1)
        else if(str.head == ')') acc(str.tail, c -1)
        else acc(str.tail, c)
      }else{
        c
      }
    }
    acc(s, 0)
  }

  read()

  def read():Unit = {
    b.setPrompt("lisp> ")
    try {
      val line = readLine("", 0)
      val end = line.replace(" ", "").toUpperCase()
      if(end == "EXIT" ||
        end == "QUIT") System.exit(0)
      val lisp = string2lisp(line)
      val result = evaluate(lisp)
      println(result)
    }catch{
      case of: StackOverflowError => println("/!\\ stack overflow error, check for infinite recursions")
      case ex: Exception => println("/!\\ " + ex.getMessage)
      case other: Throwable => println(s"unknown exception, please contact admin. Message: " + other.getMessage)
    }
    read()
  }

  def readLine(current: String, skipCount:Int ): String = {
    if(skipCount == skip) {""}else{
      val read = b.readLine()
      val count = if (read.isEmpty) 1 else 0
      val next = current + read + ' '
      if (countParens(next) > 0) {
        b.setPrompt("    | ")
        readLine(next, skipCount + count)
      } else if (countParens(next) < 0) {
        throw new UnmatchedParenthesisException(message = next)
      } else {
        next
      }
    }
  }
}