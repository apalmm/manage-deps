import networkx as nx
from .build_graph import build_graph

def multi_root_graph(roots, depth=2):
    #create a directed graph containing dependencies for multiple root packages 
    #just call build_graph for each root and merge into one graph
    G = nx.DiGraph()
    for root in roots:
        build_graph(root, depth, graph=G)

    return G

# def shared_dependencies(roots, depth=2):
#     depsets = {}
#     for r in roots:
#         G = build_graph(r, depth)
#         depsets[r] = set(G.nodes)

#     shared = {}
#     for i, a in enumerate(roots):
#         for b in roots[i+1:]:
#             shared[(a, b)] = len(depsets[a] & depsets[b])

#     return shared
