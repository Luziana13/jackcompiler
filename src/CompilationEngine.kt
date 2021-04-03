import java.io.File
import java.lang.IllegalArgumentException

class CompilationEngine (inputFile: File, outputFile: File){
    private val tokenizer = JackTokenizer(inputFile)
    private val symbolTable = SymbolTable()
    private val vmWriter = VMWriter(outputFile)

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

    private lateinit var className: String
    private lateinit var subroutName: String
    private var callSubroutName = ""
    private var currentSubroutKind = ""
    private var nArgs = 0
    private var whileLabel = 0
    private var ifLabel = 0

    fun compileClass() {
        while (tokenizer.hasMoreTokens()){
            tokenizer.advance()
            when(tokenizer.tokenType()){
                JackTokenizer.TokenType.KEYWORD -> {
                    when(tokenizer.keyWord()){
                        JackTokenizer.Keyword.STATIC.keywordName, JackTokenizer.Keyword.FIELD.keywordName -> compileClassVarDec()
                        JackTokenizer.Keyword.CONSTRUCTOR.keywordName, JackTokenizer.Keyword.FUNCTION.keywordName, JackTokenizer.Keyword.METHOD.keywordName -> compileSubRoutine()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> when(val sym = tokenizer.symbol().toString()) {
                    JackTokenizer.Symbol.LBRACE.symbolIcon -> {}
                    JackTokenizer.Symbol.RBRACE.symbolIcon -> {
                        break
                    }
                    else -> throw IllegalArgumentException()
                }
                JackTokenizer.TokenType.IDENTIFIER -> className = tokenizer.identifier()
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun compileClassVarDec() {
        val kind = tokenizer.keyWord() //pode ser field ou static
        tokenizer.advance()
        val type = when(tokenizer.tokenType()){
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            else -> throw IllegalArgumentException()
        }
        tokenizer.advance()
        symbolTable.define(tokenizer.identifier(), type, Kind.find(kind))
        while (tokenizer.tokenType() != JackTokenizer.TokenType.SYMBOL || tokenizer.symbol().toString() != JackTokenizer.Symbol.SEMICOLON.symbolIcon){
            tokenizer.advance()
            if(tokenizer.tokenType() == JackTokenizer.TokenType.IDENTIFIER){
                symbolTable.define(tokenizer.identifier(), type, Kind.find(kind))
            }
        }
    }

    private fun compileSubRoutine(){
        symbolTable.startSubroutine()
        currentSubroutKind = tokenizer.keyWord()
        while (tokenizer.hasMoreTokens()){
            tokenizer.advance()
            when(tokenizer.tokenType()){
                JackTokenizer.TokenType.KEYWORD -> {
                    when(val key = tokenizer.keyWord()){
                        in setStatements -> {
                            vmWriter.writeFunctions("$className.$subroutName", symbolTable.varCount(Kind.VAR))
                            if(currentSubroutKind == JackTokenizer.Keyword.CONSTRUCTOR.keywordName){
                                vmWriter.writePush(Segment.CONST, symbolTable.varCount(Kind.FIELD))
                                vmWriter.writeCall("Memmory.alloc", 1)
                                vmWriter.writePop(Segment.POINTER, 0)
                            }
                            if (currentSubroutKind == JackTokenizer.Keyword.METHOD.keywordName){
                                vmWriter.writePush(Segment.ARG, 0)
                                vmWriter.writePop(Segment.POINTER, 0)
                            }
                            compileStatements()
                        }
                        JackTokenizer.Keyword.VAR.keywordName -> compileVarDec()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when(val sym = tokenizer.symbol().toString()){
                        JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                            tokenizer.advance()
                            compileParameterList()
                        }
                        JackTokenizer.Symbol.RBRACE.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    if(tokenizer.nextToken() == JackTokenizer.Symbol.LPAREN.symbolIcon) subroutName = tokenizer.identifier()
                }
                else -> throw IllegalArgumentException()
            }
        }
    }
    private fun compileParameterList(){
        if(tokenizer.tokenType() == JackTokenizer.TokenType.SYMBOL && tokenizer.symbol().toString() == JackTokenizer.Symbol.RPAREN.symbolIcon) return
        var type = when(tokenizer.tokenType()){
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            else -> ""
        }
        while (tokenizer.hasMoreTokens()){
            tokenizer.advance()
            when(tokenizer.tokenType()){
                JackTokenizer.TokenType.KEYWORD -> {
                    when(val key = tokenizer.keyWord()) {
                        JackTokenizer.Keyword.INT.keywordName, JackTokenizer.Keyword.CHAR.keywordName, JackTokenizer.Keyword.BOOLEAN.keywordName -> type = key
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when(tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.RPAREN.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    if(tokenizer.nextToken() == JackTokenizer.Symbol.COMMA.symbolIcon || tokenizer.nextToken() == JackTokenizer.Symbol.RPAREN.symbolIcon){
                        symbolTable.define(tokenizer.identifier(), type, Kind.ARG)
                    } else {
                        type = tokenizer.identifier()
                    }
                }
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun compileVarDec() {
        tokenizer.advance()
        val type = when(tokenizer.tokenType()) {
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            else -> throw IllegalArgumentException()
        }
        while(tokenizer.hasMoreTokens()){
            tokenizer.advance()
            when(tokenizer.tokenType()){
                JackTokenizer.TokenType.SYMBOL -> {
                    when(tokenizer.symbol().toString()){
                        JackTokenizer.Symbol.SEMICOLON.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> symbolTable.define(tokenizer.identifier(), type, Kind.VAR)
                else -> throw IllegalArgumentException()
            }
        }
    }
    private fun compileStatements() {

    }
}