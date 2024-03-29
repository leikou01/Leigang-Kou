package cvmorph

import scala.util.matching.Regex


/** *
 * Main function
 *
 */
object ChiselVerilogMain extends App {

  import java.io._

  import scala.io.Source

  // 1. check the command line
  if (args.length != 2) {
    // println("args.length = " + args.length)
    println("Usage: scala -J-Xms256m -J-Xmx2G cvmorph.jar old_foo.v new_foo.v")
    throw new IllegalArgumentException("Wrong # of arguments")
  } else {
    println("Input:  " + args(0))
    println("Output: " + args(1))
  }

  // 2. read in the verilog file
  //    parse the assignment statements in the original verilog file
  //    get pair of (LHS, RHS) from pattern match on assign statement
  val bufferedSource = Source.fromFile(args(0))
  val lines = bufferedSource.getLines().toList // NOT including newline characters
  bufferedSource.close

  // 3. process the list from the verilog file
  val verilogInput = ChiselVerilogList(lines)
  val verilogOutput = verilogInput.newlines

  // 4. write the new list of verilog statements
  val writer = new PrintWriter(new File(args(1)))
  writer.write("// new verilog file generated by a chisel verilog morph script")
  writer.write("\n")
  for (line <- verilogOutput) {
    writer.write(line + "\n")
  }
  writer.close()
}

/** *
 * How to run the program
 * > cd cvmorph/src/main/scala
 * > scalac src/main/ChiselVerilogMain.scala                 // create java bytecode *.class
 * > scala cvmorph.ChiselVerilogMain old_foo.v new_foo.v    // execute the class
 *
 * pack into a fat jar file and execute from command line
 * > sbt assembly
 * > java -cp target/scala-2.13/cvmorph-assembly-0.1.jar cvmorph.ChiselVerilogMain old_foo.v new_foo.v
 *
 * once fix build.sbt to specify the Main class
 * > java -jar target/scala-2.13/cvmorph-assembly-0.1.jar  old_foo.v new_foo.v
 * > scala -J-Xms256m -J-Xmx2G target/scala-2.13/cvmorph-assembly-0.1.jar  old_foo.v new_foo.v
 */
