open Llvm
open Llvm_target
(*
  References: 
    * Tutorial on LLVM OCaml bindings: https://www.wzdftpd.net/blog/ocaml-llvm-01.html 
    * Repo:                            https://github.com/llvm-mirror/llvm/tree/master/bindings/ocaml
    * Exported `types`:                https://github.com/llvm-mirror/llvm/blob/master/bindings/ocaml/llvm/llvm.ml
*)

(* let rec print_type llty =
   let ty = Llvm.classify_type llty in
   match ty with
   | Llvm.TypeKind.Function -> Printf.printf "  function\n"
   | Llvm.TypeKind.Pointer  -> Printf.printf "  pointer to" ; print_type (Llvm.element_type llty)
   | _                      -> Printf.printf "  other type\n"

   let print_val lv =
   Printf.printf "Value\n" ;
   Printf.printf "  name %s\n" (Llvm.value_name lv) ;
   let llty = Llvm.type_of lv in
   Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
   print_type llty ; *)

let llctx = Llvm.global_context ()

type layout = 
  | BigEndian of Llvm_target.Endian.t (*Endian.Big*)
  | LittleEndian of Llvm_target.Endian.t (*Endian.Little*)

  (* let _ =
  let llmem = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in
  Llvm.dump_module llm ;
  () *)