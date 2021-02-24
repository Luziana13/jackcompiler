import java.io.File

fun main() {
    val input = File("D:\\Kotlin\\jackcompiler\\Main.jack")
    compile(input)
}

fun compile(inputArq: File) {
    val tempArq = File(inputArq.parent + "/" + inputArq.nameWithoutExtension + "_temp.xml")
    if (tempArq.exists()) tempArq.delete()
    val outputArq = File(inputArq.parent + "/" + inputArq.nameWithoutExtension + ".xml")
    if(outputArq.exists()) outputArq.delete()
    val compiler = tokenize(inputArq, tempArq)
    println(compiler)
}

fun tokenize(inputArq: File, tempArq: File) {
    val tokenizer = JackTokenizer(inputArq)
    println(tokenizer.bufferedReader.readLine()?.trim()?.substringBefore("//"))
    tempArq.writeWithBreakLine("<tokens>")
        while(tokenizer.hasMoreTokens()) {
            println("tem mais tokens")
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