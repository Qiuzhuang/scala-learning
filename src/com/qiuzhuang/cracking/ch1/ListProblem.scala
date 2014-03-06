package com.qiuzhuang.cracking.ch1

/**
 * All rights reserved by Qiuzhuang.Lian
 */
object ListProblem {

  /**
   * Implement an algorithm to determine if a string has all unique characters.
   * What if you can not use additional data structures?
   */
  def stringUnique(s: String): Boolean = {
    def loop(ch: Char, suffix: String): Boolean = {
      suffix.contains(ch)
    }

    if (s == null || s.size == 0) true else loop(s.head, s.tail)
  }

  /**
   * Design an algorithm and write code to remove the duplicate characters in a string without using
   * any additional buffer. NOTE: One or two additional variables are fine.
   * An extra copy of the array is not.
   * FOLLOW UP: Write the test cases for this method.
   */
  def truncate(s: String): String = {
    val sb = new StringBuilder
    for (ch <- s if sb.contains(ch)) sb.append(ch)
    sb.toString
  }

  def main(args: Array[String]) {
    val s0 = ""
    val s1 = "my name is kand lian"
    val s2 = "let's go."
    println(s0 + "=" + stringUnique(s0))
    println(s1 + "=" + stringUnique(s1))
    println(s2 + "=" + stringUnique(s2))
  }
}
