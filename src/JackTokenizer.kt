import java.io.BufferedReader
import java.io.File
import java.util.Arrays

fun File.writeWithBreakLine(text: String) {
    this.appendText("$text\n")
}

class JackTokenizer (input: File){
    private val bufferedReader: BufferedReader = input.bufferedReader()
    private var tokenList: MutableList<String> = mutableListOf()
    var currentToken: String = ""

    enum class TokenType (val tokenName: String){
        KEYWORD("keyword"), SYMBOL("symbol"), IDENTIFIER("identifier"), STRING_CONSTANT("string_constant"), INT_CONSTANT("int_constant")
    }
    enum class Keyword (val keywordName: String){
        BOOLEAN("boolean"),
        CHAR("char"),
        CLASS("class"),
        CONSTRUCTOR("constructor"),
        DO("do"),
        ELSE("else"),
        FALSE("false"),
        FUNCTION("function"),
        FIELD("field"),
        IF("if"),
        INT("int"),
        LET("let"),
        METHOD("method"),
        NULL("null"),
        RETURN("return"),
        STATIC("static"),
        THIS("this"),
        TRUE("true"),
        VAR("var"),
        VOID("void"),
        WHILE("while");
    }

    enum class Symbol (val symbolIcon: String){
        LBRACE("{"),
        RBRACE("}"),
        LPAREN("("),
        RPAREN(")"),
        LBRACKET("["),
        RBRACKET("]"),
        DOT("."),
        COMMA(","),
        SEMICOLON(";"),
        PLUS("+"),
        MINUS("-"),
        ASTERISK("*"),
        SLASH("/"),
        AND("&"),
        OR("|"),
        LT("<"),
        RT(">"),
        EQ("="),
        NOT("~")
    }

    init {
        var multLineComment = false
        var line = nextLine()
        while (line != null) {
            var hasTokensBefComment = false
            if(line.isEmpty()) {
                line = nextLine()
                continue
            }

            val multLineBegin = line.indexOf("/*") //retorna -1 se o elemento nao for encontrado
            val multLineEnd = line.indexOf("*/")

            if(multLineBegin != -1 && multLineEnd == -1) {  //multLineComment começando com /*
                line = line.substringBefore("/*")
                multLineComment = true
                if(line.isNotEmpty()) hasTokensBefComment = true
                else {
                    line = nextLine()
                    continue
                }
            }

            if(multLineBegin == -1 && multLineEnd != -1){    //multLineComment terminando com */
                line = line.substringAfter("*/")
                multLineComment = false
            }

            if (multLineBegin != -1 && multLineEnd != -1) {                                     //para o caso de comment do tipo /**/ em uma mesma linha.
                line = line.replaceRange(multLineBegin, multLineEnd+2, " ")
            }

            if(multLineComment && !hasTokensBefComment) { // comentarios de varias linhas apenas com //
                line = nextLine()
                continue
            }

            val stringLine = line.split("\"")
            for (i in stringLine.indices) {
                if (i % 2 != 0){
                    tokenList.add("\"${stringLine[i]}\"")
                    continue
                }
                val trimLine = replaceEmptySpace(stringLine[i])
                trimLine.split(" ").filterNot { it.isEmpty() }.forEach { word -> splitSymbol(word, tokenList) }
            }
            line = nextLine()
        }
    }

    fun hasMoreTokens() : Boolean {
        return tokenList.isNotEmpty()
    }

    fun advance() {
        currentToken = tokenList.first()
        tokenList.removeAt(0)
    }

    fun nextToken(): String{
        return tokenList.first()
    }

    fun tokenType(): TokenType {
        return when {
            isInEnumIdentifier(currentToken) -> TokenType.KEYWORD
            isInEnumSymbol(currentToken) -> TokenType.SYMBOL
            isInteger(currentToken) -> TokenType.INT_CONSTANT
            currentToken.startsWith("\"") && currentToken.endsWith("\"") -> TokenType.STRING_CONSTANT
            isIdentifier(currentToken) -> TokenType.IDENTIFIER
            else -> throw NoSuchElementException()
        }
    }

    private fun isInEnumIdentifier(value: String?): Boolean {
        return Arrays.stream(Keyword.values()).anyMatch { e -> e.keywordName == value }
    }

    private fun isInEnumSymbol(value: String?): Boolean{
        return Arrays.stream(Symbol.values()).anyMatch{e -> e.symbolIcon == value}
    }

    fun keyWord() : String {
        return currentToken
    }

    fun symbol(): Char {
        return currentToken.first()
    }

    fun identifier(): String{
        return currentToken
    }

    fun intVal(): Int {
        return currentToken.toInt()
    }

    fun stringVal(): String {
        return currentToken.trim('"')
    }

    private fun isInteger(token: String): Boolean {
        val pattern = Regex("^0$|^[1-9][0-9]*$")
        return pattern.containsMatchIn(token)
    }

    private fun isIdentifier(token: String): Boolean {
        val pattern = Regex("^([a-z]|[A-Z_])([a-z]|[A-Z_\\w])*$")
        return pattern.containsMatchIn(token)
    }

    private fun replaceEmptySpace(value: String) : String {
        val pattern = Regex("(\\s)+")
        return pattern.replace(value, " ")
    }
    private fun nextLine() : String? {
        return bufferedReader.readLine()?.trim()?.substringBefore("//")
    }

    private fun splitSymbol (word: String, tokenList: MutableList<String>) {
        val symbolIndex = word.indexOfFirst { isInEnumSymbol(it.toString()) }
        if (word.length == 1 || symbolIndex == -1){
            tokenList.add(word)
            return
        }
        val token = if (symbolIndex == 0 ) {
            word.first().toString()
        } else {
            word.substring(0, symbolIndex)
        }

        val restante = if (symbolIndex == 0) {
            word.substring(1)
        } else {
            word.substring(symbolIndex)
        }
        tokenList.add(token)
        splitSymbol(restante, tokenList)
    }

}
