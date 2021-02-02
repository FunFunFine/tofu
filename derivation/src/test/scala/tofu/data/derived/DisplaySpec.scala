package tofu.data.derived

import derevo.{derive, insertInstancesHere}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tofu.common.Display
import tofu.common.Display._
import tofu.data.derived.display

@derive(display)
case class Bar(value: Int, another: String)

object Bar {
  insertInstancesHere()
}

@derive(display)
case class Foo(bar: Bar, field: Double, xs: List[Int])

object Foo {
  insertInstancesHere()
}

object DisplaySpec extends App {// extends AnyFunSpec with Matchers {
  //describe("derivation") {

    val bar = Bar(
      3,
      "abc"
    )

    val expectedBar =
      """
        |Bar{
        | value = 3,
        | another = "abc"
        |}""".stripMargin

    val foo =
      Foo(
        bar = Bar(
          3,
          "abc"
        ),
        field = 3.4,
        xs = List(1, 2, 3)
      )

    val expectedFoo                =
      """
        |Foo{
        |   bar = Bar{
        |       value = 3,
        |       another = "abc"
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

      //it("should display complex case classes string") {
      println(foo.display()) //shouldBe expectedFoo
    //}
    //it("should display as list") {
      //Display[Foo].displayBuild(0, Display.Config.default, foo).value.toString// shouldBe expectedList
    //}
  //}
}
