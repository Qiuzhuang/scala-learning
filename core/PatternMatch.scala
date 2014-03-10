/**
 * All rights reserved by Qiuzhuang.Lian
 */
abstract class Expression {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(el, er) => el.eval + er.eval
    case _ => error("error Expression")
  }
}

{case p1 => E1; case p2 => E2;... case pn => En} is equivalent to

(x match {case p1 => E1; case p2 => E2;... case pn => En})




























