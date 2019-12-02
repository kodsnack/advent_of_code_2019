# Advent of Code 2019 Solutions

## Requirements

Python>=3.7.0

Only requires Python standard library for solving the problems. Requires
`pytest` for running some tests:

```console
$ pip install pytest
```


## Running

Each day in a separate file named `dayNN.py` where `NN` is the current
day number. Input is expected to be in a file named `dayNN.txt`. Run
each day with:

```console
$ python dayNN.py
```


## Running Tests

Some days have simple tests written to be run with `pytest`, run them
with:

```console
$ pytest dayNN.py
```

Some tests print timing information which is suppressed by `pytest`
unless the `-s` flag is passed.
