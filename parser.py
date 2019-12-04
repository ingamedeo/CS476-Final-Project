#
# This parser supports LLVM version 9.0.1 (llvm-dis-9)
#

import re
import sys

input_file = sys.argv[1]

# regex_fun = "define dso_local ([.]+[*]*) @([.]+) \([.]+[*]*\)"
regex_fun = re.compile("define dso_local (.*) @(.*)\((.*)\) (.*) (.*)")


# constructors = ["alloca", "call", "load", "store", "sitofp", "fptext"]


def parse_instr_into_ast(line):
    registers = []

    instr_regex = re.compile("(.*) = (.*)")
    registers_regex = re.compile("%[0-9]+")
    fn_call_regex = re.compile("(.*) (.*) @([a-zA-Z0-9_]+)(.*)")
    result = re.match(instr_regex, line)

    if ":" in line:
        # label
        return "label", [f"Reg(\"${line.split(':')[0]}\")"]

    if result is not None:
        dest, instr_body = result.groups()
        registers.append(f"Reg(\"{dest}\")")
    else:
        instr_body = line

    func_name = instr_body.split(" ")[0]
    instr_registers = re.findall(registers_regex, instr_body)
    if func_name == "icmp":
        value = instr_body.split(" ")[-1]
        if "%" not in value:
            registers.append(f"Reg(\"{value}\")")

    if func_name == "store":
        value = instr_body.split(" ")[2]
        value = value[:-1]
        if "%" not in value:
            registers.append(f"Reg(\"{value}\")")

    if func_name == "call":
        callee = re.match(fn_call_regex, instr_body)
        # print(callee.groups())
        callee = callee.groups()[2]
        registers.append(f"FnName(\"{callee}\")")

    registers.extend([f"Reg(\"{el}\")" for el in instr_registers])

    return func_name, registers


if __name__ == "__main__":

    func_begin_idx = -1
    cur_func_body = []
    func_list = {}
    with open(input_file) as f:
        file = f.readlines()
        all_instr = set()
        func_consts = ""

        for idx, line in enumerate(file):

            if "define dso_local" in line:
                func_begin_idx = idx
                func = re.match(regex_fun, line)
                func_type, func_name, func_args, identifier, parenthesis = func.groups()

                func_list[func_name] = {
                    "type": func_type,
                    "args": func_args,
                    "body": []
                }

            elif func_begin_idx != -1:
                if "}" not in line:
                    func_list[func_name]["body"].append(line.strip())
                else:
                    func_begin_idx = -1
                    cur_func_body = []

        # Here we have funcs
        for func_name in func_list.keys():
            body = func_list[func_name]["body"]
            instr_list = []
            for line in body:

        #Skip IR annotations
                if line.startswith('; '):
                    continue

                name, registers = parse_instr_into_ast(line)
                # reorder registers to make FnName(..) first

                if name == "call":
                    name_idx = -1
                    for idx, reg in enumerate(registers):
                        if "FnName" in reg:
                            name_idx = idx
                    new_registers = [registers[name_idx]]
                    new_registers.extend([rx for rx in registers if rx != registers[name_idx]])
                    registers = new_registers.copy()

                if name == "icmp":
                    name_idx = -1
                    for idx, reg in enumerate(registers):
                        if "%" not in reg:
                            name_idx = idx
                    if name_idx != -1:
                        new_registers = [registers[name_idx]]
                        new_registers.extend([rx for rx in registers if rx != registers[name_idx]])
                        registers = new_registers.copy()

                out = ""
                out += f"{name.capitalize()}("
                out += "["
                out += ";".join(registers)
                out += "])"

                if name == "br":
                    out = out.replace("%", "$")

                # print(f"{out}")
                if out != "([])":
                    all_instr.add(f"{name.capitalize()}")
                    instr_list.append(out)
            func_consts += f"Function(\"{func_list[func_name]['type']}\", \"{func_name}\", [{';'.join(instr_list)}])\n"
        instr_consts = "type instr = "
        instr_consts += " of arg list | ".join(all_instr)
        instr_consts += " of arg list"

    with open("out.ml", "w+") as ff:

        fn_map_init = "let init_fn_map = fun x -> if x == \"main\" then Some main else None"
        fn_map_update = "let update fn_map id fn = fun x -> if x == id then Some fn else fn_map id"

        ff.write(
            f"""open List \ntype ident = string\n type arg = Reg of ident | FnName of ident \n{instr_consts}\ntype func_def = Function of ident * ident * (instr list)\n{fn_map_init}\n{fn_map_update}\n{func_consts}""")