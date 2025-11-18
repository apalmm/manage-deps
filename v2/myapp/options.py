import networkx as nx


def remove_same_level(G):
    # mimic deepdep by removing edges between nodes on the same layer
    to_remove = []
    for u, v in G.edges():
        lu = G.nodes[u].get("layer", None)
        lv = G.nodes[v].get("layer", None)

        if lu is not None and lv is not None and lu == lv:
            to_remove.append((u, v))

    G.remove_edges_from(to_remove)
    return G


def remove_reverse_edges(G):
    to_remove = []
    for u, v in G.edges():
        lu = G.nodes[u].get("layer", None)
        lv = G.nodes[v].get("layer", None)
        if lu is not None and lv is not None and lu > lv:
            to_remove.append((u, v))

    G.remove_edges_from(to_remove)

    return G


def compute_node_metrics(G):
    # compute in-degree and normalized importance for each node
    indeg = dict(G.in_degree())
    max_indeg = max(indeg.values()) if indeg else 1

    # normalized weight 0–1 for color/size scaling for each node for scaling (more betweeness = higher in-degree)
    weights = {n: v / max_indeg for n, v in indeg.items()}

    # attach as node attributes
    nx.set_node_attributes(G, indeg, "in_degree")
    nx.set_node_attributes(G, weights, "importance")

    return G
