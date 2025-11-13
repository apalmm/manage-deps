import networkx as nx
from .fetch import fetch_cran_metadata

def get_dependendencies(pkg):
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

def build_graph(pkg, depth=2, seen=None, graph=None):
    if seen is None: 
        seen = set()

    if graph is None: 
        graph = nx.DiGraph()

    if pkg in seen or depth == 0: #base case: already seen this package or reached max depth
        return graph

    seen.add(pkg)

    deps = get_dependendencies(pkg) #get dependencies for this package (above)
    for dep_type, targets in deps.items():
        for target in targets:
            graph.add_edge(pkg, target, type=dep_type)
            build_graph(target, depth-1, seen, graph)
    
    return graph