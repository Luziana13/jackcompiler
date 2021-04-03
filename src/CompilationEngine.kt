import java.io.File

class CompilationEngine(inputFile: File, outputFile: File) {

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
    private var numArgs = 0
    private var whileLabel = 0
    private var ifLabel = 0

    fun compileClass() {
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> {
                    when (tokenizer.keyWord()) {
                        JackTokenizer.Keyword.STATIC.keywordName, JackTokenizer.Keyword.FIELD.keywordName -> compileClassVarDec()
                        JackTokenizer.Keyword.CONSTRUCTOR.keywordName, JackTokenizer.Keyword.FUNCTION.keywordName, JackTokenizer.Keyword.METHOD.keywordName -> compileSubroutine()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LBRACE.symbolIcon -> {}
                        JackTokenizer.Symbol.RBRACE.symbolIcon -> break
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> className = tokenizer.identifier()
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun compileClassVarDec() {
        val kind = tokenizer.keyWord()
        tokenizer.advance()
        val type = when (tokenizer.tokenType()) {
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            else -> throw IllegalArgumentException()
        }
        tokenizer.advance()
        symbolTable.define(tokenizer.identifier(), type, Kind.find(kind))

        while (tokenizer.tokenType() != JackTokenizer.TokenType.SYMBOL || tokenizer.symbol().toString() != JackTokenizer.Symbol.SEMICOLON.symbolIcon) {
            tokenizer.advance()
            if (tokenizer.tokenType() == JackTokenizer.TokenType.IDENTIFIER) {
                symbolTable.define(tokenizer.identifier(), type, Kind.find(kind))
            }
        }
    }

    private fun compileSubroutine() {
        symbolTable.startSubroutine()
        currentSubroutKind = tokenizer.keyWord()
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> {
                    when (tokenizer.keyWord()) {
                        in setStatements -> {
                            vmWriter.writeFunction("$className.$subroutName", symbolTable.varCount(Kind.VAR))
                            if (currentSubroutKind == JackTokenizer.Keyword.CONSTRUCTOR.keywordName) {
                                vmWriter.writePush(Segment.CONST, symbolTable.varCount(Kind.FIELD))
                                vmWriter.writeCall("Memory.alloc", 1)
                                vmWriter.writePop(Segment.POINTER, 0)
                            }
                            if (currentSubroutKind == JackTokenizer.Keyword.METHOD.keywordName) {
                                vmWriter.writePush(Segment.ARG, 0)
                                vmWriter.writePop(Segment.POINTER, 0)
                            }
                            compileStatements()
                        }
                        JackTokenizer.Keyword.VAR.keywordName -> compileVarDec()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                            tokenizer.advance()
                            compileParameterList()
                        }
                        JackTokenizer.Symbol.RBRACE.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    if (tokenizer.nextToken() == JackTokenizer.Symbol.LPAREN.symbolIcon) subroutName = tokenizer.identifier()
                }
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun compileParameterList() {
        if (tokenizer.tokenType() == JackTokenizer.TokenType.SYMBOL && tokenizer.symbol().toString() == JackTokenizer.Symbol.RPAREN.symbolIcon) return
        var type = when (tokenizer.tokenType()) {
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            else -> ""
        }
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> {
                    when (val keyword = tokenizer.keyWord()) {
                        JackTokenizer.Keyword.INT.keywordName, JackTokenizer.Keyword.CHAR.keywordName, JackTokenizer.Keyword.BOOLEAN.keywordName -> type = keyword
                        else -> throw IllegalArgumentException()
                    }
                }
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.RPAREN.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    if (tokenizer.nextToken() == JackTokenizer.Symbol.COMMA.symbolIcon || tokenizer.nextToken() == JackTokenizer.Symbol.RPAREN.symbolIcon) {
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
        val type = when (tokenizer.tokenType()) {
            JackTokenizer.TokenType.KEYWORD -> tokenizer.keyWord()
            JackTokenizer.TokenType.IDENTIFIER -> tokenizer.identifier()
            else -> throw IllegalStateException()
        }
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.SEMICOLON.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> symbolTable.define(tokenizer.identifier(), type, Kind.VAR)
                else -> throw IllegalArgumentException()
            }
        }
    }

    private fun compileStatements() {
        while (tokenizer.hasMoreTokens()) {
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.KEYWORD -> {
                    when (tokenizer.keyWord()) {
                        JackTokenizer.Keyword.LET.keywordName -> compileLet()
                        JackTokenizer.Keyword.IF.keywordName -> compileIf()
                        JackTokenizer.Keyword.WHILE.keywordName -> compileWhile()
                        JackTokenizer.Keyword.DO.keywordName -> compileDo()
                        JackTokenizer.Keyword.RETURN.keywordName -> compileReturn()
                        else -> throw IllegalArgumentException()
                    }
                }
                else -> throw IllegalArgumentException()
            }
            if (!setStatements.contains(tokenizer.nextToken())) break
            tokenizer.advance()
        }
    }

    private fun compileLet() {
        var isArrayAccess = false
        numArgs = 0
        tokenizer.advance()
        val varName = tokenizer.identifier()
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LBRACKET.symbolIcon -> {
                            isArrayAccess = true
                            vmWriter.writePush(Segment.find(symbolTable.kindOf(varName)), symbolTable.indexOf(varName))
                            tokenizer.advance()
                            compileExpression()
                            tokenizer.advance()
                            vmWriter.writeArithmetic(Command.ADD)
                            vmWriter.writePop(Segment.TEMP, 1)
                        }
                        JackTokenizer.Symbol.EQ.symbolIcon -> {
                            tokenizer.advance()
                            compileExpression()
                            tokenizer.advance()
                            break
                        }
                    }
                }
                else -> throw IllegalArgumentException()
            }
        }
        if (isArrayAccess) {
            vmWriter.writePush(Segment.TEMP, 1)
            vmWriter.writePop(Segment.POINTER, 1)
            vmWriter.writePop(Segment.THAT, 0)
            return
        }
        val segment = Segment.find(symbolTable.kindOf(varName))
        vmWriter.writePop(segment, symbolTable.indexOf(varName))
    }

    private fun compileIf() {
        numArgs = 0
        val branchLabel = "if.else.$subroutName.$className$ifLabel"
        val exitLabel = "if.exit.$subroutName.$className$ifLabel"
        ifLabel++
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                            tokenizer.advance()
                            compileExpression()
                            tokenizer.advance()
                            vmWriter.writeArithmetic(Command.NOT)
                            vmWriter.writeIf(branchLabel)
                        }
                        JackTokenizer.Symbol.LBRACE.symbolIcon -> {
                            tokenizer.advance()
                            compileStatements()
                            tokenizer.advance()
                            vmWriter.writeGoto(exitLabel)
                            break
                        }
                    }
                }
                else -> throw IllegalArgumentException()
            }
        }

        vmWriter.writeLabel(branchLabel)
        if (tokenizer.nextToken() == JackTokenizer.Keyword.ELSE.keywordName) {
            tokenizer.advance()
            tokenizer.advance()
            tokenizer.advance()
            compileStatements()
            tokenizer.advance()
        }
        vmWriter.writeLabel(exitLabel)
    }

    private fun compileDo() {
        numArgs = 0
        callSubroutName = ""
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.SEMICOLON.symbolIcon -> break
                    }
                }
                JackTokenizer.TokenType.IDENTIFIER -> {
                    callSubroutName = ""
                    callSubroutName += if (symbolTable.kindOf(tokenizer.identifier()) == Kind.NONE) {
                        if (tokenizer.nextToken() == JackTokenizer.Symbol.LPAREN.symbolIcon) {
                            numArgs++
                            vmWriter.writePush(Segment.POINTER, 0)
                            "$className.${tokenizer.identifier()}"
                        } else {
                            tokenizer.identifier()
                        }
                    } else {
                        numArgs++
                        vmWriter.writePush(Segment.find(symbolTable.kindOf(tokenizer.identifier())), symbolTable.indexOf(tokenizer.identifier()))
                        symbolTable.typeOf(tokenizer.identifier())
                    }
                    tokenizer.advance()
                    when (tokenizer.tokenType()) {
                        JackTokenizer.TokenType.SYMBOL -> {
                            when (tokenizer.symbol().toString()) {
                                JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                                    compileExpressionList()
                                    tokenizer.advance()
                                }
                                JackTokenizer.Symbol.DOT.symbolIcon -> {
                                    tokenizer.advance()
                                    callSubroutName += ".${tokenizer.identifier()}"
                                    tokenizer.advance()
                                    compileExpressionList()
                                    tokenizer.advance()
                                }
                                else -> throw IllegalArgumentException()
                            }
                        }
                        else -> throw IllegalArgumentException()
                    }
                }
                else -> throw IllegalArgumentException()
            }
        }
        vmWriter.writeCall(callSubroutName, numArgs)
        vmWriter.writePop(Segment.TEMP, 2)
    }

    private fun compileWhile() {
        val loopLabel = "while.loop.$subroutName.$className$whileLabel"
        val exitLabel = "while.exit.$subroutName.$className$whileLabel"
        whileLabel++
        vmWriter.writeLabel(loopLabel)
        while (tokenizer.hasMoreTokens()) {
            tokenizer.advance()
            when (tokenizer.tokenType()) {
                JackTokenizer.TokenType.SYMBOL -> {
                    when (tokenizer.symbol().toString()) {
                        JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                            tokenizer.advance()
                            compileExpression()
                            tokenizer.advance()
                            vmWriter.writeArithmetic(Command.NOT)
                            vmWriter.writeIf(exitLabel)
                        }
                        JackTokenizer.Symbol.LBRACE.symbolIcon -> {
                            tokenizer.advance()
                            compileStatements()
                            tokenizer.advance()
                            vmWriter.writeGoto(loopLabel)
                            break
                        }
                    }
                }
                else -> throw IllegalArgumentException()
            }
        }
        vmWriter.writeLabel(exitLabel)
    }

    private fun compileReturn() {
        tokenizer.advance()
        if (tokenizer.tokenType() != JackTokenizer.TokenType.SYMBOL || tokenizer.symbol().toString() != JackTokenizer.Symbol.SEMICOLON.symbolIcon) {
            if (tokenizer.identifier() == JackTokenizer.Keyword.THIS.keywordName) vmWriter.writePush(Segment.POINTER, 0)
            else compileExpression()
            tokenizer.advance()
        } else vmWriter.writePush(Segment.CONST, 0)
        vmWriter.writeReturn()
    }

    private fun compileExpression() {
        compileTerm()
        while (tokenizer.hasMoreTokens()) {
            if (!opSymbols.contains(tokenizer.nextToken())) break
            tokenizer.advance()
            val ope = tokenizer.symbol().toString()
            tokenizer.advance()
            compileTerm()
            if (ope == JackTokenizer.Symbol.ASTERISK.symbolIcon || ope == JackTokenizer.Symbol.SLASH.symbolIcon) {
                val command = if (ope == JackTokenizer.Symbol.ASTERISK.symbolIcon) "Math.multiply" else "Math.divide"
                vmWriter.writeCall(command, 2)
            } else vmWriter.writeArithmetic(Command.find(ope, unary = false))
        }
    }

    private fun compileExpressionList() {
        if (tokenizer.nextToken() == JackTokenizer.Symbol.RPAREN.symbolIcon) return
        tokenizer.advance()
        numArgs++
        compileExpression()
        while (tokenizer.hasMoreTokens()) {
            if (tokenizer.nextToken() != JackTokenizer.Symbol.COMMA.symbolIcon) break
            tokenizer.advance()
            tokenizer.advance()
            numArgs++
            compileExpression()
        }
    }

    private fun compileTerm() {
        when (tokenizer.tokenType()) {
            JackTokenizer.TokenType.KEYWORD -> {
                when (tokenizer.keyWord()) {
                    JackTokenizer.Keyword.TRUE.keywordName -> {
                        vmWriter.writePush(Segment.CONST, 1)
                        vmWriter.writeArithmetic(Command.NEG)
                    }
                    JackTokenizer.Keyword.FALSE.keywordName -> vmWriter.writePush(Segment.CONST, 0)
                    JackTokenizer.Keyword.THIS.keywordName -> vmWriter.writePush(Segment.POINTER, 0)
                    JackTokenizer.Keyword.NULL.keywordName -> vmWriter.writePush(Segment.CONST, 0)
                    else -> throw IllegalArgumentException()
                }
            }
            JackTokenizer.TokenType.SYMBOL -> {
                when (val symbol = tokenizer.symbol().toString()) {
                    JackTokenizer.Symbol.MINUS.symbolIcon, JackTokenizer.Symbol.NOT.symbolIcon -> {
                        tokenizer.advance()
                        compileTerm()
                        vmWriter.writeArithmetic(Command.find(symbol, true))
                    }
                    JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                        tokenizer.advance()
                        compileExpression()
                        tokenizer.advance()
                    }
                }
            }
            JackTokenizer.TokenType.IDENTIFIER -> {
                when (tokenizer.nextToken()) {
                    JackTokenizer.Symbol.LBRACKET.symbolIcon -> {
                        vmWriter.writePush(Segment.find(symbolTable.kindOf(tokenizer.identifier())), symbolTable.indexOf(tokenizer.identifier()))
                        tokenizer.advance()
                        tokenizer.advance()
                        compileExpression()
                        vmWriter.writeArithmetic(Command.ADD)
                        vmWriter.writePop(Segment.POINTER, 1)
                        vmWriter.writePush(Segment.THAT, 0)
                        tokenizer.advance()
                    }
                    JackTokenizer.Symbol.LPAREN.symbolIcon -> {
                        callSubroutName = "$className."
                        callSubroutName += tokenizer.identifier()
                        tokenizer.advance()
                        compileExpressionList()
                        tokenizer.advance()
                        vmWriter.writePush(Segment.POINTER, 0)
                        vmWriter.writeCall(callSubroutName, numArgs)
                    }
                    JackTokenizer.Symbol.DOT.symbolIcon -> {
                        callSubroutName = ""
                        callSubroutName += if (symbolTable.kindOf(tokenizer.identifier()) == Kind.NONE) {
                            tokenizer.identifier()
                        } else {
                            numArgs++
                            vmWriter.writePush(Segment.find(symbolTable.kindOf(tokenizer.identifier())), symbolTable.indexOf(tokenizer.identifier()))
                            symbolTable.typeOf(tokenizer.identifier())
                        }
                        tokenizer.advance()
                        tokenizer.advance()
                        callSubroutName += ".${tokenizer.identifier()}"
                        tokenizer.advance()
                        compileExpressionList()
                        tokenizer.advance()
                        vmWriter.writeCall(callSubroutName, numArgs)
                    }
                    else -> {
                        when (symbolTable.kindOf(tokenizer.identifier())) {
                            Kind.ARG -> {
                                val index = if (currentSubroutKind == JackTokenizer.Keyword.METHOD.keywordName) {
                                    symbolTable.indexOf(tokenizer.identifier()) + 1
                                } else symbolTable.indexOf(tokenizer.identifier())
                                vmWriter.writePush(Segment.ARG, index)
                            }
                            Kind.VAR -> vmWriter.writePush(Segment.LOCAL, symbolTable.indexOf(tokenizer.identifier()))
                            Kind.FIELD -> vmWriter.writePush(Segment.THIS, symbolTable.indexOf(tokenizer.identifier()))
                            Kind.STATIC -> vmWriter.writePush(Segment.STATIC, symbolTable.indexOf(tokenizer.identifier()))
                            else -> throw IllegalArgumentException()
                        }
                    }
                }
            }
            JackTokenizer.TokenType.INT_CONSTANT -> vmWriter.writePush(Segment.CONST, tokenizer.intVal())
            JackTokenizer.TokenType.STRING_CONSTANT -> {
                val str = tokenizer.stringVal()
                vmWriter.writePush(Segment.CONST, str.length)
                vmWriter.writeCall("String.new", 1)
                str.forEach {
                    vmWriter.writePush(Segment.CONST, it.toInt())
                    vmWriter.writeCall("String.appendChar", 2)
                }
            }
        }
    }
}