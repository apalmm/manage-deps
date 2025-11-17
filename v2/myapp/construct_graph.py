import networkx as nx
from .fetch_CRAN_data import fetch_cran_metadata

def get_dependencies(pkg):
    skip = { #skip these packages because they are base R packages that come pre-installed / CRAN doesn't support metadata for them
        "R", "base", "compiler", 
        "grid", "parallel", "splines", 
        "stats4", "tcltk", "methods", 
        "utils", "stats", "graphics", 
        "grDevices", "datasets", "tools"
    }

    if pkg in skip:
        #print(pkg)
        return {} #ignore base packages

    data = fetch_cran_metadata(pkg)
    deps = {}
    for key in ["Imports", "LinkingTo"]: #only dealing with these two because they are the most relevant for compilation
        #filter returned json for just the names of the dependent packages
        deps[key] = list(data.get(key, {}).keys()) if data.get(key) else []

    return deps

def build_graph(pkg, seen=None, graph=None, level=0, max_depth=[0]):
    if seen is None:
        seen = set()

    if graph is None:
        graph = nx.DiGraph()

    if pkg in seen:
        return graph  #stop if we already visited this pkg

    seen.add(pkg)
    max_depth[0] = max(max_depth[0], level)

    deps = get_dependencies(pkg)
    if not deps or all(len(v) == 0 for v in deps.values()):
        return graph  #base case: no dependencies left to follow

    for dep_type, targets in deps.items():
        for target in targets:
            graph.add_edge(pkg, target, type=dep_type)
            build_graph(target, seen, graph, level + 1, max_depth)

    graph.graph["max_depth"] = max_depth[0]
    return graph