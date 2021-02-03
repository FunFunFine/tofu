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

class DisplaySpec extends AnyFunSpec with Matchers {
  describe("derivation") {
    describe("simple cases") {
      val bar = Bar(
        3,
        "abc"
      )

      val expectedBar =
        """Bar{
          |	value = 3,
          |	another = "abc"
          |}""".stripMargin

      val expectedBarBuild: Vector[String] =
        Vector("Bar{", "\n\tvalue = 3,", "\n\tanother = \"abc\"", "\n}")
      it("should display case classes string") {
        bar.display() shouldBe expectedBar
      }
      it("should display case classes build") {
        val build = Display[Bar].displayBuild(0, Display.Config.default, bar).value
        build shouldBe expectedBarBuild
      }

      it("should display sealed traits") {
        @derive(display)
        sealed trait FooBar
        object FooBar {
          case class Barn(i: Int)    extends FooBar
          case class Darn(j: Double) extends FooBar
        }
        val adt: FooBar = FooBar.Barn(3)
        adt.display() shouldBe "Barn{\n\ti = 3\n}"
      }

    }

    describe("nested case classes") {
      val foo =
        Foo(
          bar = Bar(
            3,
            "abc"
          ),
          field = 3.4,
          xs = List(1, 2, 3)
        )

      val expectedFoo                      =
        """Foo{
          |	bar = Bar{
          |		value = 3,
          |		another = "abc"
          |	},
          |	field = 3.4,
          |	xs = List(1, 2, 3)
          |}""".stripMargin
      val expectedFooBuild: Vector[String] =
        Vector(
          "Foo{",
          "\n\tbar = Bar{",
          "\n\t\tvalue = 3,",
          "\n\t\tanother = \"abc\"",
          "\n\t},",
          "\n\tfield = 3.4,",
          "\n\txs = List(1, 2, 3)",
          "\n}"
        )

      it("should display complex case classes string") {
        println(foo.display())
        foo.display() shouldBe expectedFoo
      }
      it("should display complex case classes build") {
        val build = Display[Foo].displayBuild(0, Display.Config.default, foo).value
        build shouldBe expectedFooBuild
      }

    }

  }
}
