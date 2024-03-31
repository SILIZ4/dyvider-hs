import os
import itertools as it
import random

import numpy as np
import dyvider as dy
import networkx as nx


# with open("graph.txt") as file_stream:
    # lines = file_stream.readlines()
    # scores = {i+1: float(x.strip()) for i, x in enumerate(lines[0].split(","))}
    # edges = [tuple(map(int, line.split(","))) for line in lines[1:]]
# graph = nx.from_edgelist(edges, create_using=nx.DiGraph)
# n = graph.number_of_nodes()

np.random.seed(42)
random.seed(690)
n = 9
# graph = nx.connected_watts_strogatz_graph(n, 3, 0.01)
graph = nx.directed_configuration_model(np.random.randint(1, 4, size=n), np.random.randint(1, 4, size=n))
remove = [(u, v, k) for u, v, k in graph.edges if k>0]
graph.remove_edges_from(remove)

with open("random.txt", "w") as file_stream:
    file_stream.write(",".join(list(map(str, range(n)))))
    file_stream.write("\n")
    for u, v, *_ in graph.edges:
        file_stream.write(f"{u},{v}\n")
os.system("cabal run dyvider-hs -- random.txt --zero --directed")
os.system("cabal run dyvider-hs -- random.txt --memoize --zero --directed")

# p = [list(range(x-1, y)) for (x, y) in [(1, 3), (4, 6), (7, 9)]]
# print(p, nx.community.modularity(graph, p))
# print("in=",list(graph.in_degree()))
# print("out=", list(graph.out_degree()))
# exit()

tmp = []
qmax = -1
for k in list(range(1, n + 1)):
    for boundaries in it.combinations(range(n - 1), k - 1):
        solution = dy.layers.Layers(list(boundaries) + [n - 1])
        # partition = [{y+1 for y in ys} for ys in solution]
        partition = [{y for y in ys} for ys in solution]
        q = nx.community.modularity(graph, partition)
        if q>qmax:
            qmax = q
        tmp.append([q, solution])

print("Quality  | Solution\n----------------------------------------")
for Q, solution in tmp:
    print("{0: .3f}".format(Q), "**" if Q == qmax else "  ", "|", solution)
