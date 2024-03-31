import os
from time import perf_counter, perf_counter
from tqdm import tqdm
import basegraph

import networkx as nx
import numpy as np
from matplotlib import pyplot as plt

import sys
sys.path.insert(0, 'dyvider/')
import dyvider as dy


np.random.seed(42)


def run_haskell(memoize):
    os.system(f"./dyvider-hs random.txt --zero {'--memoize' if memoize else ''} >> /dev/null")


def run_dyvider(graph, obj):
    g = dy.utilities.preprocess(graph)
    return dy.algorithms.run(g, obj)


def timereps(reps, func):
    start = perf_counter()
    for _ in range(reps):
        func()
    return perf_counter() - start


objective_function = dy.objectives.Modularity()
# ns = np.arange(20, 300, 20)
ns=[1000]
reps = 5
names = ["dyvider", "dyvider-hs",  "dyvider-hs (memoized)"]
res = {name: [] for name in names}
fs = [
    lambda: run_dyvider(graph, objective_function),
    lambda: run_haskell(memoize=False),
    lambda: run_haskell(memoize=True)
]

for j, _n in tqdm(enumerate(ns), total=len(ns)):
    n = int(_n)
    for i in range(reps):
        g = basegraph.random.generate_gilbert_random_graph(n, 0.1)
        # graph = nx.watts_strogatz_graph(n, 3, 0.01)
        graph = nx.from_edgelist([e for e in g.edges()])
        with open("random.txt", "w") as file_stream:
            file_stream.write(",".join(list(map(str, range(n)))))
            file_stream.write("\n")
            for u, v, *_ in graph.edges:
                file_stream.write(f"{u},{v}\n")
        scores = {i: i for i in range(n)}
        nx.set_node_attributes(graph, values=scores, name='score')

        for name, f in zip(names, fs):
            if len(res[name]) <= j:
                res[name].append([])
            res[name][-1].append(timereps(1, f))

for name, results in res.items():
    averages = list(map(np.average, results))
    stds = list(map(np.std, results))
    print(ns)
    print(averages)
    print(stds)
    plt.errorbar(ns, averages, yerr=stds, label=name)
plt.ylabel("Time [s]")
plt.xlabel("n")
plt.legend()
plt.show()
