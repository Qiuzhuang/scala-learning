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

  def main(args: Array[String]) {
    val s0 = ""
    val s1 = "my name is kand lian"
    val s2 = "let's go."
    println(s0 + "=" + stringUnique(s0))
    println(s1 + "=" + stringUnique(s1))
    println(s2 + "=" + stringUnique(s2))
  }
}
