import json
import os
import subprocess
from termcolor import colored

ERROR_DIR = "./test/error_tests/"
ERROR_STRINGS = "./test/err_strings.json"
ERROR_CODE = 1
VALID_DIR = "./test/valid_tests/"
OPTIONS = ["-i", "-1", "-2"]


def run_valid_tests():
    for input in os.listdir(VALID_DIR):
        if input.endswith(".in"):
            print("Test " + input + ": ", end=" ")
            for option in OPTIONS:
                out = subprocess.check_output(["./plg-2-nka", option, VALID_DIR + input])
                with open(VALID_DIR + input.split('.')[0] + option + ".out", 'r') as ref_out:
                    test = out.decode("utf-8") == ref_out.read()
                    if test:
                        print(colored(option + ", ", "green"), end=" ")
                    else:
                        print(colored(option + ", ", "red"), end=" ")
            print()


def run_error_tests():
    with open(ERROR_STRINGS, 'r') as json_file:
        error_strings = json.load(json_file)
    for input in os.listdir(ERROR_DIR):
        try:
            subprocess.check_output(["./plg-2-nka", ERROR_DIR + input], stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as output_exc:
            if error_strings[input.split('.')[0]] == output_exc.output.decode("utf-8") and \
              ERROR_CODE == output_exc.returncode:
                print(colored("Test " + input + " successful.", 'green'))
            else:
                print(colored("Test " + input + " unsuccessful.", 'red'))


if __name__ == '__main__':
    print(colored("Error tests:", 'blue'))
    run_error_tests()
    print("----------------------")
    print(colored("Valid tests:", 'blue'))
    run_valid_tests()
