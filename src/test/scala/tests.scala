import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import scales.Main
import sys.process._

object ScalesSpecification extends Properties("Scales") {

  def checkOutput(file:String) : Prop = {
    val output = ("java -verify Main" !!)
    val correct = io.Source.fromFile(file).mkString
    output == correct
  }

  def clean : Prop = {
    Seq("sh", "-c", "rm -f *.class *.j").!
    true
  }

  def compile(file:String) : Prop = {
    try {
      Main.main(Array(file))
      Seq("sh", "-c", "jasmin *.j").!
      true
    } catch {
      case _ : Throwable => false
    }
  }

  property("test1.uc") = {
    compile("tests/in/test1.uc") &&
    checkOutput("tests/out/test1.out") &&
    clean
  }

  property("test2.uc") = {
    compile("tests/in/test2.uc") &&
    checkOutput("tests/out/test2.out") &&
    clean
  }

  property("test3.uc") = {
    compile("tests/in/test3.uc") &&
    checkOutput("tests/out/test3.out") &&
    clean
  }

  property("test4.uc") = {
    compile("tests/in/test4.uc") &&
    clean
  }

  property("test5.uc") = {
    compile("tests/in/test5.uc") &&
    clean
  }

  property("test6.uc") = {
    compile("tests/in/test6.uc")  &&
    checkOutput("tests/out/test6.out") &&
    clean
  }

  property("test7.uc") = {
    compile("tests/in/test7.uc") &&
    checkOutput("tests/out/test7.out") &&
    clean
  }

  property("sort.uc") = {
    compile("tests/in/sort.uc") &&
    clean
  }

}
