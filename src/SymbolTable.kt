import java.io.File

class Table {
    val name: String = "";
    val type: String = "";
    val kind: TagName = TODO();

    constructor(name: String, type: String, kind: TagName);
}

class SymbolTable{
     fun startSubroutine(){}

    fun define(name: String, type: String, kind: TagName ){
        val lnTable = Table(name, type, kind);

    }

    fun VarCount(kind: TagName, table: List<Table>):Int{
        var countVariables = 0;
        for (tbl in table){
            if(kind == tbl.kind){
             countVariables++
            }
        }
        return countVariables
    }

    fun KindOf(name: String, table: List<Table>):TagName{
        var kind = TagName.NONE;
        for (tbl in table){
            if(name == tbl.name){
                kind = tbl.kind;
            }
        }
        return kind;
    }
    fun TypeOf(name: String, table: List<Table>):String{
        var type = "";
        for (tbl in table){
            if(name == tbl.name){
                type = tbl.type;
            }
        }
        return type;
    }

    fun IndexOf(name: String, table: List<Table>):Int{
        val elementTable = table.find { equals(name) }
        return table.indexOf(elementTable);
    }



}