open List 
type ident = string
type arg = Op of ident | FnName of ident

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
(* let main = Function("i32", "main", [Alloca([Op("%1")]);Alloca([Op("%2")]);Alloca([Op("%3")]);Alloca([Op("%4")]);Store([Op("%1")]);Store([Op("%3")]);Store([Op("%4")]);Call([FnName("printf");Op("%5")]);Call([FnName("__isoc99_scanf");Op("%6");Op("%3")]);Store([Op("%2")]);Br([Op("$7")]);Label([Op("$7")]);Load([Op("%8");Op("%2")]);Load([Op("%9");Op("%3")]);Icmp([Op("%10");Op("%8");Op("%9")]);Br([Op("$10");Op("$11");Op("$20")]);Label([Op("$11")]);Load([Op("%12");Op("%4")]);Fadd([Op("%13");Op("%12")]);Store([Op("%13");Op("%4")]);Load([Op("%14");Op("%4")]);Fpext([Op("%15");Op("%14")]);Call([FnName("printf");Op("%16");Op("%15")]);Br([Op("$17")]);Label([Op("$17")]);Load([Op("%18");Op("%2")]);Add([Op("%19");Op("%18")]);Store([Op("%19");Op("%2")]);Br([Op("$7")]);Label([Op("$20")]);Ret([])]) *)
let main = Function("i32", "main", [Alloca([Op("%1")]);Alloca([Op("%2")]);Alloca([Op("%3")]);Alloca([Op("%4")]);Store([Op("0");Op("%1")]);Store([Op("0");Op("%3")]);Store([Op("0.000000e+00");Op("%4")]);Call([FnName("printf");Op("%5")]);Call([FnName("__isoc99_scanf");Op("%6");Op("%3")]);Store([Op("0");Op("%2")]);Br([Op("$7")]);Label([Op("$7")]);Load([Op("%8");Op("%2")]);Load([Op("%9");Op("%3")]);Icmp([Op("%10");Op("%8");Op("%9")]);Br([Op("$10");Op("$11");Op("$20")]);Label([Op("$11")]);Load([Op("%12");Op("%4")]);Fadd([Op("%13");Op("%12")]);Store([Op("%13");Op("%4")]);Load([Op("%14");Op("%4")]);Fpext([Op("%15");Op("%14")]);Call([FnName("printf");Op("%16");Op("%15")]);Br([Op("$17")]);Label([Op("$17")]);Load([Op("%18");Op("%2")]);Add([Op("%19");Op("%18")]);Store([Op("%19");Op("%2")]);Br([Op("$7")]);Label([Op("$20")]);Ret([])])

let init_fn_map = fun x -> if x == "main" then Some main else None

(* map of removed registers *)
let empty_rm_map = fun x -> None
let update_rm rm_map reg = fun x -> if x = reg then Some reg else rm_map x

let empty_proc_map = fun x -> None
let update_proc proc_map reg = fun x -> if x = reg then Some reg else proc_map x


let phi_lst = []

(*
So now we have to create the phi node, phi nodes have the following structure.

Let's say we are in label block 4
  ;If we arrived from block %0 (main) then it's 0, otherwise if we arrive from block %11, it's %12.
  %.01 = phi i32 [ 0, %0 ], [ %12, %11 ]

*)

(*
Here we don't represent values, so they should be something like: Phi(?, %0, %12, %11)
We take values from store calls. They are AFTER the assign. instr. e.g. %19 = add nsw i32 %18, 1 and then we see a store like: store i32 %19, i32* %2, align 4
We can derive a part of node phi... phi XX [?,?] [%19, %label_block where we found the store]

We should do so for all removed regs... when we can't find any more stores -> We put phi nodes in the place of the Load

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

Check if reg appears as operand of a Call() instr. If so, it's not eligible to be upgraded
*)

let rec scan_reg_upgradable regs reg fn_map =
    match regs with
                | hdr::tailr -> (
                        match hdr with
                        | Op r -> Printf.printf "scan_reg_upgradable %s %s\n" r reg;if r=reg then false else scan_reg_upgradable tailr reg fn_map
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

let rec look_for_stores body reg label_block fn_map phi_lst =
    match body with
    | hd::tail -> (
        match hd with
        | Store params -> (
        				  let dst_reg = List.nth params 0 in
        				  match dst_reg with
        				  | Op r -> (
        				  	let reg_tbm = List.nth params 1 in
        				  	match reg_tbm with
        				  	| Op r2 -> if r2=reg then
        				    look_for_stores tail reg label_block fn_map phi_lst@[Op(r)]@[Op(label_block)] else look_for_stores tail reg label_block fn_map phi_lst
        				    (*
        				  	Printf.printf "found store for reg %s with r %s in label block %s\n" reg r label_block;
        				  	[(r, label_block)]
        				    	look_for_stores tail reg label_block fn_map phi_lst@[Op(r)]@[Op(label_block)] *)
        				)
        )
	| Label params -> (
			  let jmp_lbl_c = List.nth params 0 in
			  match jmp_lbl_c with
			  | Op jmp_lbl -> look_for_stores tail reg jmp_lbl fn_map phi_lst
			  | _ -> look_for_stores tail reg label_block fn_map phi_lst (* This can't happen *)
			)
        | other -> look_for_stores tail reg label_block fn_map phi_lst
        )
    | [] -> phi_lst

(* Delete all store calls that reference to that reg
Look for load calls -> when found one, delete all and substitute with phi nodes for all substituted regs.
 *)

(*
let rec delete_load_and_store body reg = 
	match body with 
	|hd::tl -> (
		match 
	)

*)

let rec enable_mem2reg obody body fn_map offset rm_map proc_map label_block =
    match body with
    | hd::tail -> (
	match hd with
	| Alloca params -> (
		let nly_reg_c = List.nth params 0 in
		match nly_reg_c with 
		| Op nly_reg -> if is_upgradable tail nly_reg fn_map then (
				Nop::enable_mem2reg obody tail fn_map offset (update_rm rm_map nly_reg) proc_map label_block
				(*Delete all store calls that reference to that reg and substitute load calls with phi*)
				) else Alloca(params)::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
		)
	| Load params -> (
		let src_reg_c = List.nth params 1 in (* Loads have two params, 1 is the src reg *)
		match src_reg_c with
		| Op src_reg -> (
			(* the register is one that needs processing and hasn't been processed yet *)
			match rm_map src_reg, proc_map src_reg with
			| Some reg, None -> (
				(* NOTE: look_for_stores should be able to look for stores in the FULL body (obody). Not only the body. *)
				let store_list = look_for_stores obody reg label_block fn_map phi_lst in
					Printf.printf "found prev. rm reg %s\n" reg;
					Phi(src_reg_c::store_list)::enable_mem2reg obody tail fn_map offset rm_map (update_proc proc_map reg) label_block
					(* print_store_list store_list; Phi(store_list)::enable_mem2reg tail fn_map offset rm_map label_block *)
			)
			| _, Some reg -> Printf.printf "found load already processed to phi node. removing..\n";Nop::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
			| _, _ -> Printf.printf "Load with reg %s left in place\n" src_reg;Load(params)::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
			)
		)
	(* Alloca() inst. MUST come before any related Load/Store() *)
	| Store params -> (
                let src_reg_c = List.nth params 1 in (* Loads have two params, 1 is the src reg *)
                match src_reg_c with
                | Op src_reg -> (
                        (* the register is one that needs processing and hasn't been processed yet *)
                        match rm_map src_reg, proc_map src_reg with
                        | Some reg, _ -> (
				Nop::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
                        )
                        | _, Some reg -> (
                                Nop::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
                        )
                        | _, _ -> Printf.printf "Store with reg %s left in place\n" src_reg;Store(params)::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
                        )
		)
	    | other -> other::enable_mem2reg obody tail fn_map offset rm_map proc_map label_block
	)
    | [] -> []

let rec remove_nops body = 
    match body with 
    | Nop::rest -> remove_nops rest
    | i::rest -> i::remove_nops rest
    | [] -> []

(* LLVM mem2reg optimization entry point *)
let ssa_enabled =
        match main with
        | Function (fn_type, name, body) -> (
	   Function (fn_type, name, remove_nops (enable_mem2reg body body init_fn_map 0 empty_rm_map empty_proc_map "%0"))
)

