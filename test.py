#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import sys


def build():
    os.chdir("vinci")
    subprocess.call(["make"])
    os.chdir("../src")
    subprocess.call(["dotnet", "build", "-o", "../app"])
    os.chdir("..")
    subprocess.call(["cp", "vinci/vinci", "./app"])


if __name__ == "__main__":
    build()
    benchmarks = [
        b
        for b in Path("benchmarks/").glob("**/*.spcf")
        if (b.parent / "expected").exists()
    ]
    # Remove leftover outputs from previous runs:
    if Path("output").exists():
        subprocess.call(["rm", "-rf", "output"])
    failed = []
    for b in benchmarks:
        print(f"Running benchmark {b}...")
        stem = b.stem
        expected = b.parent / "expected"
        subprocess.call(
            ["app/GuBPI", str(b)], stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT
        )
        result = f"output/{stem}-norm.bounds"
        try:
            ret = subprocess.check_call(
                [
                    "git",
                    "--no-pager",
                    "diff",
                    "--color-words",
                    "--no-index",
                    str(expected),
                    result,
                ]
            )
        except subprocess.CalledProcessError as e:
            ret = e.returncode
        if ret != 0:
            print(f"\n{result} and {expected} differ!")
            subprocess.call(["cp", result, str(b.parent / "result")])
            failed.append(b)
        else:
            print("Passed.")

    if failed:
        print("Tests failed:")
        for b in failed:
            print(str(b))
        sys.exit(1)
    else:
        print("All tests passed.")
