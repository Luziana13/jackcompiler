import java.io.File
import java.lang.IllegalArgumentException

class CompilationEngine (inputFile: File, private  val outputFile: File){
    private val tokenizer = JackTokenizer(inputFile)
    private val opSymbols: Set<String> = setOf(JackTokenizer.Symbol.PLUS.symbolIcon,
                                               JackTokenizer.Symbol.MINUS.symbolIcon,
                                               JackTokenizer.Symbol.ASTERISK.symbolIcon,
                                               JackTokenizer.Symbol.SLASH.symbolIcon,
                                               JackTokenizer.Symbol.AND.symbolIcon,
                                               JackTokenizer.Symbol.OR.symbolIcon,
                                               JackTokenizer.Symbol.LT.symbolIcon,
                                               JackTokenizer.Symbol.RT.symbolIcon,
                                               JackTokenizer.Symbol.EQ.symbolIcon)

    private val setStatements: Set<String> = setOf(JackTokenizer.Keyword.LET.keywordName,
                                                   JackTokenizer.Keyword.IF.keywordName,
                                                   JackTokenizer.Keyword.WHILE.keywordName,
                                                   JackTokenizer.Keyword.DO.keywordName,
                                                   JackTokenizer.Keyword.RETURN.keywordName)

    fun compileClass() {
        outputFile.writeWithBreakLine(initTag(TagName.CLASS.value))
        while (tokenizer.hasMoreTokens()){
            tokenizer.advance()
           when(tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> when(val key = tokenizer.keyWord()) {
                    JackTokenizer.Keyword.CLASS.keywordName -> outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, key))
                    JackTokenizer.Keyword.STATIC.keywordName, JackTokenizer.Keyword.FIELD.keywordName -> compileClassVarDec()
                    JackTokenizer.Keyword.CONSTRUCTOR.keywordName, JackTokenizer.Keyword.FUNCTION.keywordName, JackTokenizer.Keyword.METHOD.keywordName -> compileSubRoutine()
                    else -> throw IllegalArgumentException()
                }
                JackTokenizer.TokenType.SYMBOL -> when(val sym = tokenizer.symbol().toString()) {
                    JackTokenizer.Symbol.LBRACE.symbolIcon -> outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                    JackTokenizer.Symbol.RBRACE.symbolIcon -> {
                        outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                        break
                    }
                    else -> throw IllegalArgumentException()
                }
               JackTokenizer.TokenType.IDENTIFIER -> outputFile.writeWithBreakLine(xmlTag(TagName.IDENTIFIER, tokenizer.identifier()))
               else -> throw IllegalArgumentException()
            }
        }
        outputFile.writeWithBreakLine(endTag(TagName.CLASS.value))
    }

    private fun compileClassVarDec() {
        outputFile.writeWithBreakLine(initTag(TagName.CLASS_VAR_DEC.value))
        outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, tokenizer.keyWord()))
        while (tokenizer.tokenType() != JackTokenizer.TokenType.SYMBOL || tokenizer.symbol().toString() != JackTokenizer.Symbol.SEMICOLON.symbolIcon) {
            tokenizer.advance()
            when(tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> {
                    when(val key = tokenizer.keyWord()){
                        JackTokenizer.Keyword.INT.keywordName, JackTokenizer.Keyword.CHAR.keywordName, JackTokenizer.Keyword.BOOLEAN.keywordName -> {
                            outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, key))
                        }
                        else -> throw  IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when(val sym = tokenizer.symbol().toString()){
                        JackTokenizer.Symbol.COMMA.symbolIcon -> outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                        JackTokenizer.Symbol.SEMICOLON.symbolIcon -> {
                            outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                            break
                        }
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    outputFile.writeWithBreakLine(xmlTag(TagName.IDENTIFIER, tokenizer.identifier()))
                }
                else -> throw IllegalArgumentException()
            }
        }
        outputFile.writeWithBreakLine(endTag(TagName.CLASS_VAR_DEC.value))
    }

    private fun compileSubRoutine() {
        outputFile.writeWithBreakLine(initTag(TagName.SUBROUTINE_DEC.value))
        outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, tokenizer.keyWord()))
        while (tokenizer.hasMoreTokens()){
            tokenizer.advance()
            when(tokenizer.tokenType()){
                JackTokenizer.TokenType.KEYWORD -> {
                    when (val key = tokenizer.keyWord()){
                        JackTokenizer.Keyword.VOID.keywordName, JackTokenizer.Keyword.INT.keywordName, JackTokenizer.Keyword.CHAR.keywordName, JackTokenizer.Keyword.BOOLEAN.keywordName -> {
                            outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, key))
                        }
                        in setStatements -> {
                            compileStatements()
                        }
                        JackTokenizer.Keyword.VAR.keywordName -> compileVarDec()
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when(val sym = tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                            outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                            tokenizer.advance()
                            compileParameterList()
                            outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, tokenizer.symbol().toString()))
                        }
                        JackTokenizer.Symbol.LBRACE.symbolIcon -> {
                            outputFile.writeWithBreakLine(initTag(TagName.SUBROUTINE_BODY.value))
                            outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                        }
                        JackTokenizer.Symbol.RBRACE.symbolIcon -> {
                            outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                            outputFile.writeWithBreakLine(endTag(TagName.SUBROUTINE_BODY.value))
                            break
                        }
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    outputFile.writeWithBreakLine(xmlTag(TagName.IDENTIFIER, tokenizer.identifier()))
                }
                else -> throw IllegalArgumentException()
            }
        }
        outputFile.writeWithBreakLine(endTag(TagName.SUBROUTINE_DEC.value))
    }

    fun compileParameterList() {
        outputFile.writeWithBreakLine(initTag(TagName.PARAMETER_LIST.value))
        if (tokenizer.tokenType() == JackTokenizer.TokenType.SYMBOL && tokenizer.symbol()
                .toString() == JackTokenizer.Symbol.RPAREN.symbolIcon
        ) {
            outputFile.writeWithBreakLine(endTag(TagName.PARAMETER_LIST.value))
            return
        }
        outputFile.writeWithBreakLine(xmlTag(TagName.KEYWORD, tokenizer.keyWord()))
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> when (val key = tokenizer.keyWord()) {
                    JackTokenizer.Keyword.INT.keywordName, JackTokenizer.Keyword.CHAR.keywordName, JackTokenizer.Keyword.BOOLEAN.keywordName -> outputFile.writeWithBreakLine(
                        xmlTag(TagName.KEYWORD, key)
                    )
                    else -> throw IllegalArgumentException()
                }
                JackTokenizer.TokenType.SYMBOL -> when(val sym = tokenizer.symbol().toString()){
                    JackTokenizer.Symbol.COMMA.symbolIcon -> outputFile.writeWithBreakLine(xmlTag(TagName.SYMBOL, sym))
                    JackTokenizer.Symbol.RPAREN.symbolIcon -> break
                    else -> throw IllegalArgumentException()
                }
                JackTokenizer.TokenType.IDENTIFIER -> outputFile.writeWithBreakLine(xmlTag(TagName.IDENTIFIER, tokenizer.identifier()))
                else -> throw IllegalArgumentException()
            }
        }
        outputFile.writeWithBreakLine(endTag(TagName.PARAMETER_LIST.value))
    }
    fun compileVarDec() {

    }

    fun compileStatements() {

    }

    fun compileDo() {

    }

    fun compileLet() {

    }

    fun compileReturn() {

    }

    fun compileIf() {

    }

    fun compileWhile() {

    }

    fun compileExpression() {

    }

    fun compileExpressionList() {

    }

    fun compileTerm() {

    }

    private fun initTag(value: String) = "<$value>"
    private fun endTag(value: String) = "</$value>"
    private fun xmlTag(tagName: TagName, value: String): String = initTag(tagName.value) + value + endTag(tagName.value)

}