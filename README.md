# Dyvider-hs

A command-line (and faster) Haskell implementation of [dyvider].

The algorithm is from the paper [\"Exact and rapid linear clustering of networks with dynamic programming\"](https://arxiv.org/abs/2301.10403).

**Disclaimer**: The code is not thouroughly tested yet, but the results seem consistent with networkx and the original package.

[dyvider]: https://github.com/jg-you/dyvider


## Usage

Dyvider-hs provides a simple command-line interface to perform community detection on a edgelist:
```console
$ ./dyvider-hs graph.txt
Q*=0.4444444444444444
[[2,3,1],[4]]
```
where the file `graph.txt` contains the vertices scores on the first line and the edges on the subsequent lines
```
12.3, 3, 4, -12
1,2
3,2
4,4
```

Flags:
- `--zero`: Notice the vertices are indexed from 1. Use the `--zero` argument to read a zero-indexed edgelist.
- `--directed`: Edgelist is a directed graph (instead of the default undirected).
- `--output`: File name to write the output to (instead of standard output).
- `--memoize`: Memoized version of the algorithm (described in the appendix).
- `--quality`: Quality metric to optimize. Currently only supports modularity.

## Compiling

This project is built using [Cabal](https://cabal.readthedocs.io/en/stable/getting-started.html):
```console
$ cabal build -O2
$ cabal install --installdir=.
```
