package tofu.common

import derevo.derive
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tofu.common.Display._

import derevo.insertInstancesHere

@derive(Display)
case class Bar(value: Int)

object Bar {
  insertInstancesHere()
}

@derive(Display)
case class Foo(bar: Bar, field: Double, xs: List[Int])

object Foo {
  insertInstancesHere()
}


class DisplaySpec extends AnyFunSpec with Matchers {
  describe("derivation") {

    val value                      = Foo(Bar(3), 3.4, List(1, 2, 3))
    val expected                   =
      """
        |Foo{
        |   bar = Bar{
        |       value = 3
        |       },
        |   field = 3.4,
        |   xs = List(1,2,3)
        |}""".stripMargin
    val expectedList: List[String] =
      List(
        "Foo{",
        "\t\tbar = Bar{",
        "\t\t\t\tvalue = 3",
        "\t\t\t\t},",
        "\t\tfield = \"fld\",",
        "\t\txs = List(1,2,3)",
        "}"
      )

    val cfg                            = Display.Config.default
    import cfg._
    val expectedListSubs: List[String] =
      List(
        s"Foo${brackets.left}",
        s"${indent}bar${fieldAssign}Bar${brackets.left}",
        s"$indent${indent}value${fieldAssign}3",
        s"$indent$indent${brackets.right},",
        s"${indent}field${fieldAssign}3.4,",
        s"${indent}xs${fieldAssign}List(1,2,3)",
        s"${brackets.right}"
      )
    it("should display as string") {
      value.display() shouldBe expected
    }
    it("should display as list"){
      Display[Foo].displayBuild(0, cfg, value) shouldBe expectedList
    }
  }
}
