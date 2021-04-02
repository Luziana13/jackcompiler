import java.lang.IllegalArgumentException

enum class Kind {
    STATIC, FIELD, ARG, VAR, NONE;
    companion object {
        fun find(keyword: String): Kind{
            return values().find { it.name.toLowerCase() == keyword } ?: throw IllegalArgumentException()
        }
    }
}

data class SymbolInformation(val type: String, val kind: Kind, val index: Int)

class SymbolTable {
    private val classMap = mutableMapOf<String, SymbolInformation>()
    private val subroutMap = mutableMapOf<String, SymbolInformation>()

    fun startSubroutine() {
        subroutMap.clear()
    }

    fun define(name: String, type: String, kind: Kind){
        when(kind){
            Kind.STATIC, Kind.FIELD -> {
                val maxId = classMap.entries.filter { it.value.kind == kind }.maxByOrNull { it.value.index }?.value?.index ?: -1
                classMap[name] = SymbolInformation(type, kind, maxId + 1)
            }

            Kind.ARG, Kind.VAR -> {
                val maxId = subroutMap.entries.filter { it.value.kind == kind }.maxByOrNull { it.value.index }?.value?.index ?: -1
                subroutMap[name] = SymbolInformation(type, kind, maxId + 1)
            }
            Kind.NONE -> throw IllegalArgumentException()
            }
        }

    fun varCount(kind: Kind): Int {
        return when(kind) {
            Kind.STATIC, Kind.FIELD -> classMap.count{ it.value.kind == kind }
            Kind.ARG, Kind.VAR -> subroutMap.count{ it.value.kind == kind }
            else -> throw IllegalArgumentException()
        }
    }

    fun kindOf(name: String): Kind {
        return classMap[name]?.kind ?: subroutMap[name]?.kind ?: Kind.NONE
    }

    fun typeOf(name: String): String {
        return classMap[name]?.type ?: subroutMap[name]?.type ?: throw IllegalArgumentException()
    }

    fun indexOf(name: String): Int {
        return classMap[name]?.index ?: subroutMap[name]?.index ?: throw IllegalArgumentException()
    }
}
