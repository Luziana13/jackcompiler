import java.io.File

fun main() {
    val input = File("D:\\Kotlin\\jackcompiler\\Main.jack")
    compile(input)
}

fun compile(inputArq: File) {
    val tempArq = File(inputArq.parent + "/" + inputArq.nameWithoutExtension + ".xml")
    if (tempArq.exists()) tempArq.delete()
    tokenize(inputArq, tempArq)
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
