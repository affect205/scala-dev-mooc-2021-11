package scala.fp.opt

import module1.opt
import org.scalatest.flatspec.AnyFlatSpec

class option_test extends AnyFlatSpec {

  "check null" should "None" in  {
    val v: String = null
    val vOpt: opt.Option[String] = opt.Option(v)
    assert(vOpt == opt.Option.None)
  }

  "check isEmpty" should "true" in  {
    val v: String = null
    val vOpt: opt.Option[String] = opt.Option(v)
    assert(vOpt.isEmpty)
  }

  "check None zip None" should "None" in  {
    val v1Opt: opt.Option[String] = opt.Option(null)
    val v2Opt: opt.Option[String] = opt.Option(null)
    assert(v1Opt.zip(v2Opt) == opt.Option.None)
  }

  "check Some(x) zip None" should "None" in  {
    val v1Opt: opt.Option[String] = opt.Option("x")
    val v2Opt: opt.Option[String] = opt.Option(null)
    assert(v1Opt.zip(v2Opt) == opt.Option.None)
  }

  "check None zip Some(x)" should "None" in  {
    val v1Opt: opt.Option[String] = opt.Option(null)
    val v2Opt: opt.Option[String] = opt.Option("x")
    assert(v1Opt.zip(v2Opt) == opt.Option.None)
  }

  "check Some(x) zip Some(y)" should "(x, y))" in  {
    val v1Opt: opt.Option[String] = opt.Option("x")
    val v2Opt: opt.Option[String] = opt.Option("y")
    assert(v1Opt.zip(v2Opt).get == ("x", "y"))
  }

  "check Some(x) filter(x != x)" should "None" in  {
    val v = "x"
    val vOpt: opt.Option[String] = opt.Option(v)
    assert(vOpt.filter(_ != v) == opt.Option.None)
  }

  "check Some(x) filter(x == x)" should "Some(x)" in  {
    val v = "x"
    val vOpt: opt.Option[String] = opt.Option(v)
    assert(vOpt.filter(_ == v).get == v)
  }

}
