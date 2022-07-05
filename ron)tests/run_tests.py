import os
import subprocess
import difflib

def make_cmd(fn):
    cmd = "cd .. && ocaml compiler.ml ./tests/inputs/{0}.scm > ./tests/bin/{0}.s && nasm -f elf64 -o ./tests/bin/{0}.o ./tests/bin/{0}.s && gcc -static -m64 -o ./tests/executables/{0} ./tests/bin/{0}.o".format(fn)
    return cmd

def run_compiler(filename):
   print(filename)
   fname = filename[:-4]
   if not os.path.isfile('./executables/{0}'.format(fname)):
     os.system(make_cmd(fname))
   os.system('./executables/{0} > ./outputs/{0}'.format(fname))

def run_chez(filename):
   print(filename[:-4])
   if not os.path.isfile('./expected_inputs/{0}'.format(filename)):
    os.system(f'scheme -q < ./inputs/{filename} > ./expected_outputs/{filename[:-4]}')
   else:
    os.system(f'scheme -q < ./expected_inputs/{filename} > ./expected_outputs/{filename[:-4]}')

def compare(filename):
    output_name = filename[:-4]
    with open('./expected_outputs/{}'.format(output_name), 'r') as expected_output:
        with open('./outputs/{}'.format(output_name), 'r') as real_output:
            c = real_output.read()
            d = expected_output.read()

            print(c)
            print(d)

            e = "(equal? '("+c+") "+"'("+d+") )"
            with open("helper.scm", "w") as text_file:
                text_file.write(e)
            os.system(f'scheme -q < helper.scm > output_tester.txt')
            with open("output_tester.txt", "r") as text_file:
                out = text_file.read().replace('\n','')
                print(out)
                if out != "#t":
                   diff = "Different"
                else:
                   diff = ""
    return diff

if __name__ == "__main__":
    input_files = os.listdir('./inputs')
    report = {}
    failed = {}

    tests_total = 0
    tests_passed = 0
    for test in input_files:
        tests_total += 1
        run_compiler(test)
        run_chez(test)
        line_diff = []
        for line in compare(test):
            line_diff.append(line)
        print(line_diff)
        if line_diff == []:
            report[test] = "passed \n"
            tests_passed += 1
        else:
            failed[test] = "failed: \n" + " ".join(line_diff)
    if tests_passed != tests_total:
        print("passed {}/{} tests".format(tests_passed,tests_total))
        for i in failed:
          print(i, end = '\n')
    else:
        print("passed all tests!")
