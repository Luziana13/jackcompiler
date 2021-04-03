import java.io.File

enum class Command(val operator: String, val unary: Boolean){
    ADD("+", false),
    SUB("-", false),
    NEG("-", true),
    EQ("=", false),
    GT(">", false),
    LT("<", false),
    AND("&", false),
    OR("|", false),
    NOT("~", true);

    companion object {
        fun find(operator: String, unary: Boolean): Command {
            return values().find { it.operator == operator && it.unary == unary } ?: throw IllegalArgumentException()
        }
    }
}

enum class Segment(val value: String) {
    CONST("constant"),
    ARG("argument"),
    LOCAL("local"),
    STATIC("static"),
    THIS("this"),
    THAT("that"),
    POINTER("pointer"),
    TEMP("temp");

    companion object {
        fun find(kind: Kind): Segment {
            return when (kind) {
                Kind.VAR -> LOCAL
                Kind.ARG -> ARG
                Kind.STATIC -> STATIC
                Kind.FIELD -> THIS
                else -> throw IllegalArgumentException()
            }
        }
    }
}

class VMWriter (private val outputFile: File){
        fun writePush(segment: Segment, index: Int){
            outputFile.writeWithBreakLine("push ${segment.value} $index")
        }

        fun writePop(segment: Segment, index: Int){
            outputFile.writeWithBreakLine("pop ${segment.value} $index")
        }

        fun writeArithmetic(command: Command) {
            outputFile.writeWithBreakLine(command.name.toLowerCase())
        }

        fun writeLabel(label: String) {
            outputFile.writeWithBreakLine("label $label")
        }

        fun writeGoto(label: String) {
            outputFile.writeWithBreakLine("goto $label")
        }

        fun writeIf(label: String) {
            outputFile.writeWithBreakLine("if-goto $label")
        }

        fun writeCall(name: String, nArgs: Int){
            outputFile.writeWithBreakLine("call $name $nArgs")
        }

        fun writeFunctions(name: String, nLocals: Int){
            outputFile.writeWithBreakLine("function $name $nLocals")
        }

        fun writeReturn(){
            outputFile.writeWithBreakLine("return")
        }
}

