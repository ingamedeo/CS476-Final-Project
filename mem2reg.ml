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
let main = Function("i32", "main", [Alloca([Reg("%1")]);Alloca([Reg("%2")]);Alloca([Reg("%3")]);Alloca([Reg("%4")]);Store([Reg("%1")]);Store([Reg("%3")]);Store([Reg("%4")]);Call([FnName("printf");Reg("%5")]);Call([FnName("__isoc99_scanf");Reg("%6");Reg("%3")]);Store([Reg("%2")]);Br([Reg("$7")]);Label([Reg("$7")]);Load([Reg("%8");Reg("%2")]);Load([Reg("%9");Reg("%3")]);Icmp([Reg("%10");Reg("%8");Reg("%9")]);Br([Reg("$10");Reg("$11");Reg("$20")]);Label([Reg("$11")]);Load([Reg("%12");Reg("%4")]);Fadd([Reg("%13");Reg("%12")]);Store([Reg("%13");Reg("%4")]);Load([Reg("%14");Reg("%4")]);Fpext([Reg("%15");Reg("%14")]);Call([FnName("printf");Reg("%16");Reg("%15")]);Br([Reg("$17")]);Label([Reg("$17")]);Load([Reg("%18");Reg("%2")]);Add([Reg("%19");Reg("%18")]);Store([Reg("%19");Reg("%2")]);Br([Reg("$7")]);Label([Reg("$20")]);Ret([])])

let init_fn_map = fun x -> if x == "main" then Some main else None

(* list of removed registers *)
let rm_list = []

(* 
1) Look for Alloca calls DONE
2) Check that register doesn't appear as operand of any Call() DONE
3) If so -> eligible for promotion ;) DONE
4) Delete Alloca DONE (Alloca is turned into Nop instruction)
5) Substitute load with phi node
When you find a load instr -> Look for store instr with that reg as src. -> If multiple we need to get blocks and build phi node
6) Delete Load, Store(s)

 *)

(*
Check if reg appears as operand of a Call() instr. If so, it's not eligible to be upgraded ;(
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

(* Delete all store calls that reference to that reg
Look for load calls -> when found one, delete all and substitute with phi nodes for all substituted regs.
 *)

let rec enable_mem2reg body fn_map offset rm_list =
    match body with
    | hd::tail -> (
	match hd with
	| Alloca params -> (
		let nly_reg_c = List.nth params 0 in
		match nly_reg_c with 
		| Reg nly_reg -> if is_upgradable tail nly_reg fn_map then (
				Nop::enable_mem2reg tail fn_map offset (nly_reg::rm_list)
				(*Delete all store calls that reference to that reg and substitute load calls with phi*)
				) else Alloca(params)::enable_mem2reg tail fn_map offset rm_list
		)
	| Load params -> (
		let src_reg_c = List.nth params 1 in (* Loads have two params, the first is the dest, the second is the source reg *)
		match src_reg_c with
		| Reg src_reg -> //Check if in rm_list if it's in there, we have removed that reg
		)
	| other -> other::enable_mem2reg tail fn_map offset rm_list
	)
    | [] -> []

let ssa_enabled =
        match main with
        | Function (fn_type, name, body) -> (
	   Function (fn_type, name, enable_mem2reg body init_fn_map 0 rm_list)
	)

