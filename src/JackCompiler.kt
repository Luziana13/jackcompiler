import java.io.File
import java.lang.IllegalArgumentException

fun main(vararg args: String) {
    val arq = File(args[0])
    if(arq.isDirectory) {
        val files = arq.listFiles {file -> file.extension == "jack"} ?: throw IllegalArgumentException()
        files.forEach { inputArq -> compile(inputArq) }
    } else{
        compile(arq)
    }
}

fun compile(inputArq: File) {
    val tempArq = File("${inputArq.parent}/${inputArq.nameWithoutExtension}T.xml")
    if (tempArq.exists()) tempArq.delete()
    val outArq = File("${inputArq.parent}/${inputArq.nameWithoutExtension}.xml")
    if (outArq.exists()) outArq.delete()
    val compiler = CompilationEngine(inputArq, outArq)
    tokenize(inputArq, tempArq)
    compiler.compileClass()
}

fun tokenize(inputArq: File, tempArq: File) {
    val tokenizer = JackTokenizer(inputArq)
    tempArq.writeWithBreakLine("<tokens>")
        while(tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when(tokenizer.tokenType()) {
                JackTokenizer.TokenType.IDENTIFIER -> {
                    val entrada = tokenizer.identifier()
                    tempArq.writeWithBreakLine("<identifier> $entrada </identifier>")
                }
                JackTokenizer.TokenType.STRING_CONSTANT -> {
                    val entrada = tokenizer.stringVal()
                    tempArq.writeWithBreakLine("<stringConstant> $entrada </stringConstant>")
                }
                JackTokenizer.TokenType.INT_CONSTANT -> {
                    val entrada = tokenizer.intVal()
                    tempArq.writeWithBreakLine("<integerConstant> $entrada </integerConstant>")
                }
                JackTokenizer.TokenType.KEYWORD -> {
                    val entrada = tokenizer.keyWord()
                    tempArq.writeWithBreakLine("<keyword> $entrada </keyword>")
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    val entrada = when(val simbolo = tokenizer.symbol()) {
                        '<' -> "&lt;"
                        '>' -> "&gt;"
                        '&' -> "&amp"
                        else -> simbolo.toString()
                    }
                    tempArq.writeWithBreakLine("<symbol> $entrada </symbol>")
                }
            }
        }
    tempArq.writeWithBreakLine("</tokens>")
}
