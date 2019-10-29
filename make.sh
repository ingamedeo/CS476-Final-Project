#!/bin/sh

make_examples () {
    echo "making examples"
    cd sample_code
    for source_dir in `find . -type d`
    do

        if [ $source_dir = "." ]; then
            continue
        fi  
        
        echo "[${source_dir}] Generating LLVM IR."
        for f in `find ${source_dir} -type f -name "*.c"`
        do
            clang -c -emit-llvm $f
        done
        echo "[${source_dir}] Done."
    done
}

make_parser () {
    echo "Compiling OCaml IR parser..."
    ocamlbuild  -use-ocamlfind -pkgs llvm,llvm.bitreader -I src llvm_parser.byte
    echo "Done."
}

make_all () {
    make_parser
    make_examples
}

case $1 in 
    examples)
        make_examples
    ;;
    parser)
        make_parser
    ;;
    all)
        make_all
    ;;
esac