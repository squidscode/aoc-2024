"""

What is going on with the OCAML BigInt library?

I'm giving up on ocaml for P11 and I will be writing the solution in python...
If someone knows what's going on, please let me know...

"""

import argparse
import typing
from collections import Counter, defaultdict

ITERATIONS = 25

def step(i: int) -> list[int]:
    if i == 0:
        return [1]
    elif (sz := len(str(i))) % 2 == 0:
        return [int(str(i)[:sz//2]), int(str(i)[sz//2:])]
    else:
        return [i*2024]


def do(ints: typing.Iterable[int]) -> int:
    ints = list(ints)
    for i in range(ITERATIONS):
        print(f"Current Iteration: #{i}, sz:{len(ints)}")
        expanded = [step(i) for i in ints]
        flattened = []
        for l in expanded:
            flattened.extend(l)
        ints = flattened
    return len(ints)

def do_compressed(ints: typing.Iterable[int]) -> int:
    """

    The instructions stress that the ordering of the numbers matter,
    but nothing about the problem necessitates them being in-line...

    Lets just store a frequency table of the ints, and do the transformations
    over them instead...

    """
    ints = Counter(ints)
    for i in range(ITERATIONS):
        # print(f"Current Iteration: #{i+1}, sz:{sum(ints.values())}")
        new_counter = defaultdict(int)
        for k in ints:
            for i in step(k):
                new_counter[i] += 1 * ints[k]
        ints = new_counter
        # print(ints)
    return sum(ints.values())



"""

Lets say the multiplication part and the "storing arbitrary sized integers"
is what's causing the slowdown (I have no proof of this, but I think this
may be a fun optimization to go after, and I'm easily distracted)...

If we store the log10(x) instead of x, we can simply add log10(2024) to each 
x whenever we need to multiply by 2024. And, importantly, finding the number of
digits in a number is easy to do if we know what log10(x) is...

Q: How would splitting work?
A: It probably wouldn't...
"""

# NOTE: since the optimization above worked, we don't need to implement this
def do_log_optimization(ints: typing.Iterable[int]) -> list[float]:
    return []

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file")
    parser.add_argument("-i", "--iterations", type=int, default=25)
    parser.add_argument("-o", "--optimization", 
        choices=["compressed", "default"], default="default")
    args = parser.parse_args()
    ITERATIONS = args.iterations
    match args.optimization:
        case "default": fn = do
        case "compressed": fn = do_compressed
        case _: fn = do
    with open(args.file) as f:
        print("ans: " + str(fn(int(s) for s in f.readline().split(" "))))
