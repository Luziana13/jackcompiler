import java.io.File
import java.io.IOException


fun main() {
    var i = 0
    var diretorio = ""
    try {
        diretorio = File(".").canonicalPath
    } catch (e: IOException) {
        e.printStackTrace()
    }
    val arq = File("$diretorio/arquivosJack")
    val arquivos = arq.listFiles()
    while (arquivos?.isNotEmpty() == true && i != arquivos.size) {
        if(arquivos[i].extension == "jack"){
            compile(arquivos[i])
        }
        i++
    }

}

fun compile(inputArq: File) {
    val tempArq = File("${inputArq.parent}/${inputArq.nameWithoutExtension}T.xml")
    if (tempArq.exists()) tempArq.delete()
    val outArq = File("${inputArq.parent}/${inputArq.nameWithoutExtension}.xml")
    if (tempArq.exists()) tempArq.delete()
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
