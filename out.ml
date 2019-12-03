open List 
type ident = string
 type arg = Reg of ident | FnName of ident 
type instr = Fadd of arg list | Ret of arg list | Label of arg list | Br of arg list | Store of arg list | Load of arg list | Mul of arg list | Sitofp of arg list | Add of arg list | Fpext of arg list | Call of arg list | Alloca of arg list | Icmp of arg list
type func_def = Function of ident * ident * (instr list)
let init_fn_map = fun x -> if x == "main" then Some main else None
let update fn_map id fn = fun x -> if x == id then Some fn else fn_map id
Function("i32", "main", [Alloca([Reg("%1")]);Alloca([Reg("%2")]);Alloca([Reg("%3")]);Store([Reg("%1")]);Store([Reg("%2")]);Store([Reg("%3")]);Br([Reg("$4")]);Label(["$4"]);Load([Reg("%5");Reg("%3")]);Icmp([Reg("10");Reg("%6");Reg("%5")]);Br([Reg("$6");Reg("$7");Reg("$17")]);Label(["$7"]);Load([Reg("%8");Reg("%3")]);Load([Reg("%9");Reg("%3")]);Mul([Reg("%10");Reg("%8");Reg("%9")]);Sitofp([Reg("%11");Reg("%10")]);Load([Reg("%12");Reg("%2")]);Fadd([Reg("%13");Reg("%12");Reg("%11")]);Store([Reg("%13");Reg("%2")]);Br([Reg("$14")]);Label(["$14"]);Load([Reg("%15");Reg("%3")]);Add([Reg("%16");Reg("%15")]);Store([Reg("%16");Reg("%3")]);Br([Reg("$4")]);Label(["$17"]);Load([Reg("%18");Reg("%2")]);Fpext([Reg("%19");Reg("%18")]);Call([FnName("printf");Reg("%20");Reg("%19")]);Ret([])])
