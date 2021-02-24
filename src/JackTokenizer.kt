import java.io.BufferedReader
import java.io.File
import java.util.Arrays

fun File.writeWithBreakLine(text: String) {
    this.appendText("$text\n")
}

class JackTokenizer (input: File){
    val bufferedReader : BufferedReader = input.bufferedReader()

    enum class TokenType (val tokenName: String){
        KEYWORD("keyword"), SYMBOL("symbol"), IDENTIFIER("identifier"), STRING_CONSTANT("string_constant"), INT_CONSTANT("int_constant")
    }
    enum class Keyword (val keywordName: String){
        BOOLEAN("boolean"),
        CHAR("char"),
        CLASS("class"),
        CONSTRUCTOR("constructor"),
        DO("do"),
        ELSE("do"),
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

    private var tokenList : MutableList<String> = mutableListOf()
    var currentToken: String = ""


    fun hasMoreTokens() : Boolean {
        return tokenList.isNotEmpty()
    }

    //tokenList precisa ser incrementado. Fazer isso amanha!
    fun advance() {
        currentToken = tokenList.first()
        tokenList.removeAt(0)
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

    fun isInEnumIdentifier(value: String?): Boolean {
        return Arrays.stream(Keyword.values()).anyMatch { e -> e.keywordName == value }
    }

    fun isInEnumSymbol(value: String?): Boolean{
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

    fun isInteger(token: String): Boolean {
        val pattern = Regex("^0$|^[1-9][0-9]*$")
        return pattern.containsMatchIn(token)
    }

    fun isIdentifier(token: String): Boolean {
        val pattern = Regex("^([a-z]|[A-Z_])([a-z]|[A-Z_\\w])*$")
        return pattern.containsMatchIn(token)
    }

    fun nextLine() : String? {
        return bufferedReader.readLine()?.trim()?.substringBefore("//")
    }

}

/*

fun main() {
    */
/*val symbols: List<Byte> = listOf(
        '{', '}', '(', ')', '[', ']', '.', ',', ';',
        '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'
    ).map(Char::toByte)
    println(symbols)*//*


   */
/* var testando = JackTokenizer("teste")
    *//*
*/
/*println(testando.isIdentifier("huateste12"))
    println(JackTokenizer.TokenType.KEYWORD.tokenName)
    println(JackTokenizer.TokenType.KEYWORD)
    "teste".contains('t')*//*
*/
/*
    println(testando.tokenType("="))
    println(testando.tokenType("class"))
    println(testando.tokenType("-"))
    println(testando.tokenType("\"constructor\""))
    println(testando.tokenType("name123_teste"))
    println(testando.tokenType("teste_id2"))*//*

    //println(testando.tokenType("123_name123")) // identifier invalido. Variavel nao pode comecar por numero
}*/
