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
            clang -c $1 -emit-llvm $f
        done
        echo "[${source_dir}] Done."
    done
}

make_parser () {
    echo "Compiling OCaml IR parser..."
    ocamlbuild  -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitwriter,llvm.target,llvm_X86,llvm.bitreader -I src llvm_parser.byte
    echo "Done."
}

make_all () {
    make_parser
    make_examples $1
}

print_usage () {
    echo "Usage: ./make [examples [<optimization_level>]] | [parser] | [all [<optimization_level>]] | [help]"
    echo "For more info use ./make help"
}

print_help () {
    echo "./make "
    echo -e "\t examples [<optimization_level>]: builds the examples in the \"sample_code\" folder"
    echo -e "\t                                  default optimization level is -O0 (no optimization)"
    echo -e "\t                                  allowed values are: -O1, -O2, -O3, -Os"
    echo ""
    echo -e "\t parser:                          builds the ocaml parser in the \"src\" folder"
    echo ""
    echo -e "\t all      [<optimization_level>]: builds both the parser and the examples with the"
    echo -e "\t                                  given optimization level (examples only, default -O0)"
    echo ""
    echo -e "\t help:                            prints this help"
}



case $1 in 
    examples)
        make_examples $2
    ;;
    parser)
        make_parser
    ;;
    all)
        make_all $2
    ;;
    help)
        print_help
    ;;
    *)
        print_usage
    ;;
esac

exit 0
