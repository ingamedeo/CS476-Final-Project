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
| Phi of arg list
| Nop

type func_def = Function of ident * ident * (instr list)
let update fn_map id fn = fun x -> if x == id then Some fn else fn_map id
let main = Function("i32", "main", [Alloca([Reg("%1")]);Alloca([Reg("%2")]);Alloca([Reg("%3")]);Alloca([Reg("%4")]);Store([Reg("%1")]);Store([Reg("%3")]);Store([Reg("%4")]);Call([FnName("printf");Reg("%5")]);Call([FnName("__isoc99_scanf");Reg("%6");Reg("%3")]);Store([Reg("%2")]);Br([Reg("$7")]);Label([Reg("$7")]);Load([Reg("%8");Reg("%2")]);Load([Reg("%9");Reg("%3")]);Icmp([Reg("%10");Reg("%8");Reg("%9")]);Br([Reg("$10");Reg("$11");Reg("$20")]);Label([Reg("$11")]);Load([Reg("%12");Reg("%4")]);Fadd([Reg("%13");Reg("%12")]);Store([Reg("%13");Reg("%4")]);Load([Reg("%14");Reg("%4")]);Fpext([Reg("%15");Reg("%14")]);Call([FnName("printf");Reg("%16");Reg("%15")]);Br([Reg("$17")]);Label([Reg("$17")]);Load([Reg("%18");Reg("%2")]);Add([Reg("%19");Reg("%18")]);Store([Reg("%19");Reg("%2")]);Br([Reg("$7")]);Label([Reg("$20")]);Ret([])])

let init_fn_map = fun x -> if x == "main" then Some main else None

(* map of removed registers *)
let empty_rm_map = fun x -> None
let update_rm rm_map reg = fun x -> if x = reg then Some reg else rm_map x

(*
So now we have to create the phi node, phi nodes have the following structure.

Let's say we are in label block 4
  ;If we arrived from block %0 (main) then it's 0, otherwise if we arrive from block %11, it's %12.
  %.01 = phi i32 [ 0, %0 ], [ %12, %11 ]

Here we don't represent values, so they should be something like: Phi(?, "%0", %12", "%11")
We take values from store calls. They are AFTER the assign. instr. e.g. %19 = add nsw i32 %18, 1 and then we see a store like: store i32 %19, i32* %2, align 4
We can derive a part of node phi... phi XX [?,?] [%19, %label_block where we found the store]

We should do so for all removed regs... when we can't find any more stores -> We put phi nodes in the place of the Load ;))

*)

(* 
1) Look for Alloca calls DONE
2) Check that register doesn't appear as operand of any Call() DONE
3) If so -> eligible for promotion ;) DONE
4) Delete Alloca DONE (Alloca is turned into Nop instruction)
5) Substitute load with phi node
When you find a load instr -> Look for store instr with that reg as src. -> If multiple we need to get blocks and build phi node
I AM HERE ;)
-> now build phi node
6) Delete Load, Store(s)
7) Rename registers like Francesco did. (But only as final step, otherwise we lose refs for building phi nodes!!)
8) Should we also remove useless regs. Like there's a %1 reg at the beginning of the program. No idea.
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

let rec look_for_stores body reg label_block fn_map =
    match body with
    | hd::tail -> (
        match hd with
        | Store params -> Printf.printf "found store with reg %s in label block %s\n" reg label_block; look_for_stores tail reg label_block fn_map
	| Label params -> (
			  let jmp_lbl_c = List.nth params 0 in
			  match jmp_lbl_c with
			  | Reg jmp_lbl -> look_for_stores tail reg jmp_lbl fn_map
			  | _ -> look_for_stores tail reg label_block fn_map (* This can't happen *)
			)
        | other -> look_for_stores tail reg label_block fn_map
        )
    | [] -> []

(* Delete all store calls that reference to that reg
Look for load calls -> when found one, delete all and substitute with phi nodes for all substituted regs.
 *)

let rec enable_mem2reg body fn_map offset rm_map label_block =
    match body with
    | hd::tail -> (
	match hd with
	| Alloca params -> (
		let nly_reg_c = List.nth params 0 in
		match nly_reg_c with 
		| Reg nly_reg -> if is_upgradable tail nly_reg fn_map then (
				Nop::enable_mem2reg tail fn_map offset (update_rm rm_map nly_reg) label_block
				(*Delete all store calls that reference to that reg and substitute load calls with phi*)
				) else Alloca(params)::enable_mem2reg tail fn_map offset rm_map label_block
		)
	| Load params -> (
		let src_reg_c = List.nth params 1 in (* Loads have two params, the first is the dest, the second is the source reg *)
		match src_reg_c with
		| Reg src_reg -> (
			match rm_map src_reg with
			| Some reg -> (
				(* NOTE: Don't do anything for now. look_for_stores should be able to look for stores in the FULL body. Not only the tail. *)
				let interm_ast = look_for_stores body reg label_block fn_map in
					Printf.printf "found prev. rm reg %s\n" reg;
					Nop::enable_mem2reg tail fn_map offset rm_map label_block
			)
			| None -> Printf.printf "reg %s is still there\n" src_reg;Load(params)::enable_mem2reg tail fn_map offset rm_map label_block
			)
		)
	| other -> other::enable_mem2reg tail fn_map offset rm_map label_block
	)
    | [] -> []

(* LLVM mem2reg optimization entry point *)
let ssa_enabled =
        match main with
        | Function (fn_type, name, body) -> (
	   Function (fn_type, name, enable_mem2reg body init_fn_map 0 empty_rm_map "%0")
	)

