open List 
type ident = string
type arg = Reg of ident | FnName of ident 
type instr = Store of arg list | Sitofp of arg list | Call of arg list | Ret of arg list | Alloca of arg list | Load of arg list | Fpext of arg list
type func_def = Function of ident * ident * (instr list)

let update fn_map id fn = fun x -> if x = id then Some fn else fn_map x

let fun1 = Function("void", "change_value_to", [Alloca([Reg("%2")]);Store([Reg("%0");Reg("%2")]);Call([Reg("%3");FnName("rand")]);Sitofp([Reg("%4");Reg("%3")]);Load([Reg("%5");Reg("%2")]);Store([Reg("%4");Reg("%5")]);Ret([])])
let fun2 = Function("float*", "change_and_return", [Alloca([Reg("%2")]);Store([Reg("%0");Reg("%2")]);Call([Reg("%3");FnName("rand")]);Sitofp([Reg("%4");Reg("%3")]);Load([Reg("%5");Reg("%2")]);Store([Reg("%4");Reg("%5")]);Load([Reg("%6");Reg("%2")]);Ret([Reg("%6")])])
let main = Function("i32", "main", [Alloca([Reg("%1")]);Store([Reg("%1")]);Call([FnName("change_value_to");Reg("%1")]);Load([Reg("%2");Reg("%1")]);Fpext([Reg("%3");Reg("%2")]);Call([Reg("%4");FnName("printf");Reg("%3")]);Ret([])])

let init_fn_map = fun x -> if x = "main" then Some main else None
let fn_map_tmp = update init_fn_map "change_value_to" fun1
let fn_map = update fn_map_tmp "change_and_return" fun2

let rec offset_register reglist offset = 
    match reglist with 
    | hd::tail -> (
        match hd with 
        | Reg p -> Reg ("%"^string_of_int(int_of_string (String.sub p 1 (String.length p)) + offset))::offset_register tail offset
        | _-> hd::offset_register tail offset
    )
    | [] -> []

let offset_instruction instr offset = 
    match instr with 
    | Store i -> Store( offset_register i offset)
    | Sitofp i ->Sitofp ( offset_register i offset)
    | Call i -> Call (offset_register i offset)
    | Ret i -> Ret(offset_register i offset)
    | Alloca i -> Alloca(offset_register i offset)
    | Load i -> Load(offset_register i offset)
    | Fpext i -> Fpext(offset_register i offset)

let rec rename_registers fn_body offset = 
    match fn_body with 
    | hd::tail -> offset_instruction hd offset :: rename_registers tail offset
    | [] -> []

let rec inline_fn_body body fn_map = 
    match body with 
    | hd::tl -> (
        match hd with 
        | FnName fname -> (
            Printf.printf "name %s\n" fname;
            match fn_map fname with 
            | Some (Function (ret, name, body)) -> rename_registers body 2
            | None -> []

        )
        | Reg reg -> inline_fn_body tl fn_map
    )
    | [] -> []
let rec inline_fn_calls body fn_map = 
    match body with 
    | hd::tail -> (
        match hd with 
        | Call params ->(
            match inline_fn_body params fn_map with 
            | [] -> hd::inline_fn_calls tail fn_map
            | other -> other @ inline_fn_calls tail fn_map
        )
        | _ -> hd::inline_fn_calls tail fn_map
    )
    | [] -> []

let inlined = 
    match main with
    | Function (fn_type, name, body) -> Function(fn_type, name, inline_fn_calls body fn_map)

