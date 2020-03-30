import json
import os
import subprocess
from termcolor import colored

ERROR_DIR = "./test/error_tests/"
ERROR_STRINGS = "./test/err_strings.json"
ERROR_CODE = 1


def run_error_tests():
    with open(ERROR_STRINGS, 'r') as json_file:
        error_strings = json.load(json_file)
    for input in os.listdir(ERROR_DIR):
        try:
            subprocess.check_output(["./plg-2-nka", ERROR_DIR + input], stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as output_exc:
            assert error_strings[input.split('.')[0]] == output_exc.output.decode("utf-8")
            assert ERROR_CODE == output_exc.returncode
            print(colored("Test " + input + " successful.", 'green'))


if __name__ == '__main__':
    run_error_tests()
