package lib.day

import java.io.BufferedReader
import scala.io.Source

abstract class AocDay[InputType, OutputType <: AnyVal] {
    def parse(text: String): InputType

    def solve1(data: InputType): OutputType

    def solve2(data: InputType): OutputType

    def run(taskId: Int): Unit =
        println(s"Task $taskId")
        println(s"Sample: ${solve(taskId, "sample.txt")}")
        println(s"Answer: ${solve(taskId, "input.txt")}")

    def solve(taskId: Int, fileName: String): OutputType =
        val data = parseInputFile(fileName)
        val solver: InputType => OutputType = getSolver(taskId)
        solver(data)

    private def parseInputFile(fileName: String): InputType =
        val resourceFileContent: BufferedReader = Source
            .fromResource(
              s"${this.getClass.getPackageName.split('.').last}/${this.getClass.getSimpleName
                      .stripSuffix("$")}/$fileName"
            )
            .bufferedReader()
        parse(readFileToString(resourceFileContent))

    private def readFileToString(reader: BufferedReader): String =
        val sb = new StringBuilder
        try {
            var line = reader.readLine()
            while (line != null) {
                sb.append(line)
                sb.append(System.lineSeparator())
                line = reader.readLine()
            }
        } finally reader.close()
        sb.toString().strip()

    private def getSolver(taskId: Int): InputType => OutputType =
        taskId match
            case 1 => solve1
            case 2 => solve2
}
