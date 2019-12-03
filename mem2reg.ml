open List 
type ident = string
type arg = Reg of ident | FnName of ident 

type instr = Store of arg list 
| Sitofp of arg list 
| Call of arg list 
| Ret of arg list 
| Alloca of arg list 
| Load of arg list 
| Fpext of arg list
| Fadd of arg list
| Add of arg list
| Mul of arg list
| Icmp of arg list
| Br of arg list
| Label of arg list
| Nop

type func_def = Function of ident * ident * (instr list)
let update fn_map id fn = fun x -> if x == id then Some fn else fn_map id
let main = Function("i32", "main", [Alloca([Reg("%1")]);Alloca([Reg("%2")]);Alloca([Reg("%3")]);Alloca([Reg("%4")]);Store([Reg("%1")]);Store([Reg("%3")]);Store([Reg("%4")]);Call([FnName("printf");Reg("%5")]);Call([FnName("__isoc99_scanf");Reg("%6");Reg("%3")]);Load([Reg("%7");Reg("%4")]);Fadd([Reg("%8");Reg("%7")]);Store([Reg("%8");Reg("%4")]);Load([Reg("%9");Reg("%4")]);Fpext([Reg("%10");Reg("%9")]);Call([FnName("printf");Reg("%11");Reg("%10")]);Load([Reg("%12");Reg("%4")]);Fadd([Reg("%13");Reg("%12")]);Store([Reg("%13");Reg("%4")]);Load([Reg("%14");Reg("%4")]);Fpext([Reg("%15");Reg("%14")]);Call([FnName("printf");Reg("%16");Reg("%15")]);Ret([])])

let init_fn_map = fun x -> if x == "main" then Some main else None

(* 
1) Look for Alloca calls DONE
2) Check that register doesn't appear as operand of any Call() DONE
3) If so -> eligible for promotion ;) DONE
4) Delete Alloca DONE (Alloca is turned into Nop instruction)
5) Delete Store
5) Look for load calls to that register and substitute with phi node. (..)
 *)

(*
Check if reg is upgradable.
*)

let rec scan_reg_upgradable regs reg fn_map =
    match regs with
                | hdr::tailr -> (
                        match hdr with
                        | Reg r -> Printf.printf "scan_reg_upgradable %s %s\n" r reg;if r=reg then false else scan_reg_upgradable tailr reg fn_map
                        | other -> scan_reg_upgradable tailr reg fn_map
                        )
    		| [] -> true

let rec is_upgradable body reg fn_map =
    match body with
    | hd::tail -> (
        match hd with
        | Call params -> Printf.printf "is_upgradable %s\n" reg;if scan_reg_upgradable params reg fn_map then is_upgradable tail reg fn_map else false
        | other -> is_upgradable tail reg fn_map
        )
    | [] -> true

let rec inline_declared_fn_calls body fn_map offset =
    match body with
    | hd::tail -> (
	match hd with
	| Alloca params -> (
		let nly_reg_c = List.nth params 0 in
		match nly_reg_c with 
		| Reg nly_reg -> if is_upgradable tail nly_reg fn_map then Nop::inline_declared_fn_calls tail fn_map offset else Alloca(params)::inline_declared_fn_calls tail fn_map offset
		)
	| other -> other::inline_declared_fn_calls tail fn_map offset 
	)
    | [] -> []

let ssa_enabled =
        match main with
        | Function (fn_type, name, body) -> (
	   Function (fn_type, name, inline_declared_fn_calls body init_fn_map 0)
	)


