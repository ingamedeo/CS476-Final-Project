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
| Nop

type func_def = Function of ident * ident * (instr list)

let update fn_map id fn = fun x -> if x = id then Some fn else fn_map x
let fun2 = Function("float*", "change_and_return", [Alloca([Op("%2")]);Store([Op("%0");Op("%2")]);Call([FnName("rand");Op("%3")]);Sitofp([Op("%4");Op("%3")]);Load([Op("%5");Op("%2")]);Store([Op("%4");Op("%5")]);Load([Op("%6");Op("%2")]);Ret([Op("%6")])])
let fun1 = Function("void", "change_value_to", [Alloca([Op("%2")]);Store([Op("%0");Op("%2")]);Call([FnName("rand");Op("%3")]);Sitofp([Op("%4");Op("%3")]);Load([Op("%5");Op("%2")]);Store([Op("%4");Op("%5")]);Ret([])])
let main = Function("i32", "main", [Alloca([Op("%1")]);Store([Op("%1")]);Call([FnName("change_value_to");Op("%1")]);Load([Op("%2");Op("%1")]);Fpext([Op("%3");Op("%2")]);Call([FnName("printf");Op("%4");Op("%3")]);Fpext([Op("%3");Op("%2")]); Fpext([Op("%3");Op("%2")]);Ret([])])

let main_unroll = Function("dso_local i32", "main", [Alloca([Op("%1")]);Alloca([Op("%2")]);Alloca([Op("%3")]);Store([Op("%1")]);Store([Op("%2")]);Store([Op("%3")]);Br([Op("$4")]);Label([Op("$4")]);Load([Op("%5");Op("%3")]);Icmp([Op("10");Op("%6");Op("%5")]);Br([Op("$6");Op("$7");Op("$17")]);Label([Op("$7")]);Load([Op("%8");Op("%3")]);Load([Op("%9");Op("%3")]);Mul([Op("%10");Op("%8");Op("%9")]);Sitofp([Op("%11");Op("%10")]);Load([Op("%12");Op("%2")]);Fadd([Op("%13");Op("%12");Op("%11")]);Store([Op("%13");Op("%2")]);Br([Op("$14")]);Label([Op("$14")]);Load([Op("%15");Op("%3")]);Add([Op("%16");Op("%15")]);Store([Op("%16");Op("%3")]);Br([Op("$4")]);Label([Op("$17")]);Load([Op("%18");Op("%2")]);Fpext([Op("%19");Op("%18")]);Call([FnName("printf");Op("%20");Op("%19")]);Ret([])])
let init_fn_map = fun x -> if x = "main" then Some main else None
let fn_map_tmp = update init_fn_map "change_value_to" fun1
let fn_map = update fn_map_tmp "change_and_return" fun2

let rec offset_register reglist offset = 
    match reglist with 
    | hd::tail -> (
        match hd with 
        | Op p -> (
            if String.contains p '%' then  Op ("%"^string_of_int(int_of_string (String.sub p 1 ((String.length p) - 1)) + offset))::offset_register tail offset else Op (p)::offset_register tail offset
        )
        
        | _ -> hd::offset_register tail offset
    )
    | [] -> []

let strip_until_ith str i = int_of_string (String.sub str i (String.length str - 1))

let rec has_useless_registers reg_list base = 
    match reg_list with 
    | Op(i)::t -> if strip_until_ith i 1 <= base then true else has_useless_registers t base
    | _ -> false 

let offset_instruction instr offset base usefullness_check = 
    match instr with 
    | Store i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Store (new_registers)
    ) 
    | Sitofp i ->(
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Sitofp (new_registers)
    ) 
    | Call i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Call (new_registers)
    ) 
    | Ret i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true || List.length new_registers = 0 then Nop else Load (new_registers@[Op ("%"^string_of_int(offset))])
    ) 
    | Alloca i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Alloca (new_registers)
    ) 
    | Load i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Load (new_registers)
    ) 
    | Fpext i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Fpext (new_registers)
    ) 
    | Fadd i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Fadd (new_registers)
    )    
    | Add i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Add (new_registers)
    )    
    | Mul i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Mul (new_registers)
    )
    | Icmp i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Icmp (new_registers)
    )
    | Br i -> (
        let new_registers = offset_register i offset in 
        if has_useless_registers new_registers base = true && usefullness_check = true then Nop else Br (new_registers)
    ) 
    | Label i -> Label (offset_register i offset)
    
    | Nop -> Nop



let get_first_register_from_instruction instr = 
    match instr with 
    | Store (i)
    | Fadd (i)
    | Sitofp (i) 
    | Add (i)
    | Call (i) 
    | Mul (i)
    | Ret (i) 
    | Icmp (i)
    | Alloca (i)
    | Br (i)
    | Load (i)
    | Fpext (i) -> (
        match i with 
        | b::rest -> (
            match b with 
            | Op addr -> strip_until_ith addr 1
            | _ -> -999
            )
        | _ -> -999
    )
    | _ -> -999


let rec rename_registers fn_body offset base = 
    match fn_body with 
    | hd::tail -> offset_instruction hd offset base true:: rename_registers tail offset base
    | [] -> []

let rec find_first_register_addr block = 
    match block with 
    | instr1::tl -> (
        let reg = get_first_register_from_instruction instr1 in 
        if reg = -999 then find_first_register_addr tl else reg
    )
    | _ -> -999

let rec find_last_register_addr block = find_first_register_addr (List.rev block)

let rec inline_fn_body_new fn_name base_addr residual_offset = 
    match fn_map fn_name with 
    | Some (Function (ret, name, body)) -> (
        (* find the first instruction that uses a register *)
        let first_addr = find_first_register_addr body in 
        let last_addr = find_last_register_addr body in 
        (last_addr, rename_registers body (base_addr - first_addr + residual_offset) base_addr)
    )
    | None -> (0, [])


let rec inline_declared_fn_calls body fn_map offset = 
    match body with 
    | hd::tail -> (
        match hd with 
        | Call params -> (
            let fname = List.hd params in 
            let return_reg = List.nth params 1 in 
            match fname, return_reg with 
            | FnName name, Op base -> (
                let new_body = inline_fn_body_new name (strip_until_ith base 1) offset in 
                if snd new_body = [] then [offset_instruction (Call params) offset 0 false] @ inline_declared_fn_calls tail fn_map offset else snd new_body @ inline_declared_fn_calls tail fn_map (fst new_body)
            )
            | _ -> snd (inline_fn_body_new "never_reached" 0 0)
        )
        | _ -> offset_instruction hd offset 0 false ::inline_declared_fn_calls tail fn_map offset
    )
    | [] -> []

let rec remove_nops body = 
    match body with 
    | Nop::rest -> remove_nops rest
    | i::rest -> i::remove_nops rest
    | [] -> []

let add_conditional_return body = 
    match List.rev body with 
    | hd::rest -> (
        match hd with 
        | Ret _ -> body
        | _ -> body@[Ret []]
    )
    | [] -> body

let rec unroll block size orig_size =
    if size > 0 then rename_registers block (orig_size - size) 0 @ unroll block (size - 1) orig_size else []

let rec find_register_pointing_to_label registers label: arg option = 
    match registers with 
    | hd::tl -> (
        match hd with 
        | Op i -> if i = label then Some (Op i) else find_register_pointing_to_label tl label
        | _ -> find_register_pointing_to_label tl label
    )
    | [] -> None

let rec find_until_branch_pointing_to_label label instrs accumulator: (instr list) = 
    match instrs with 
    | hd::tl -> (
        match hd with 
        | Br reg -> (
            match  find_register_pointing_to_label reg label with 
            | Some reg -> accumulator
            | None -> find_until_branch_pointing_to_label label tl accumulator@[hd]
        )
        | _ -> find_until_branch_pointing_to_label label tl accumulator@[hd]
    )
    | [] -> accumulator

let rec find_first_label (body: instr list) (label_table: ident -> (instr list) option): (ident -> (instr list) option) = 
    match body with 
    | hd::tl -> (
        match hd with 
        | Label label-> (
            match List.hd label with 
            | Op label -> (
                let instrs = find_until_branch_pointing_to_label label tl [] in
                    if instrs = [] then find_first_label tl label_table else find_first_label tl (update label_table label instrs)
            )
            | FnName unused -> (fun x -> None)
        )
        | _ -> find_first_label tl label_table
    )
    | [] -> label_table

let rec find_unroll_factor instrs = 
    match instrs with 
    | hd::tl -> (
        match hd with 
        | Icmp args -> (
            match List.hd args with 
            | Op reg -> int_of_string reg
            | FnName _ -> -999
        )
        | _ -> find_unroll_factor tl
    )
    | [] -> -999

let inlined = 
    match main with
    | Function (fn_type, name, body) -> (
        (* first pass, it dumbly inlines the function call substituting all op calls that would have
           resulted in an invalid register use to Nop
         *)
        let pass1 = Function(fn_type, name, inline_declared_fn_calls body fn_map 0) in 
        (* second pass, registers after the function call by the given offset *)
        match pass1 with 
        | Function (_, _, new_body) -> Function (fn_type, name, add_conditional_return(remove_nops new_body))
    )

let empty_label_table = fun x -> None

let rec inject_at_branch_label whole_func to_inject label base = 
    match whole_func with 
    | hd::tl -> (
        match hd with 
        | Br args -> (
            match List.hd args with 
            | Op reg -> if reg = label then to_inject@rename_registers tl ((List.length to_inject) * base)  0 else hd::inject_at_branch_label tl to_inject label base
            | FnName n -> []
        )
        | _ -> hd::inject_at_branch_label tl to_inject label base
    )
    | [] -> []

let rec remove_icmp fn_body = 
    match fn_body with 
    |hd::tl -> (
        match hd with 
        | Icmp _ -> remove_icmp tl
        |_ -> hd::remove_icmp tl
    )
    |[] -> []

let unrolled = 
    match main_unroll with 
    | Function (fn_type, name, body) -> (
        let label_table = find_first_label body empty_label_table in 
        let loop_to_unroll = "$4" in 
        match label_table loop_to_unroll with 
        | Some instrs -> (
            let unroll_factor = find_unroll_factor (List.rev instrs) in 
            if unroll_factor <> -999 then (
                let unrolled_body = unroll (List.rev (instrs)) (unroll_factor - 1) unroll_factor in 
                let cleaned = remove_icmp unrolled_body in 
                Function (fn_type, name, inject_at_branch_label body cleaned loop_to_unroll unroll_factor)
            )
            else (
                raise (Failure "Illegal state -> couldn't unroll")
            )
        )
        | None -> Function(fn_type, name, [])
    )

(*  #use "out_tmp.ml";; *)
(* Function (fn_type, name, unroll body 10 10) *)