import os
import networkx as nx
from pyvis.network import Network
from .options import remove_reverse_edges, compute_node_metrics
from .graph_features import add_features


def assign_layers(G, roots):
    """
    Assign 'layer' attribute to each node based on distance from nearest root.
    """
    if isinstance(roots, str):
        roots = [roots]

    layers = {}
    for r in roots:
        lengths = nx.single_source_shortest_path_length(G, r)
        for node, dist in lengths.items():
            if node not in layers or dist < layers[node]:
                layers[node] = dist

    nx.set_node_attributes(G, layers, "layer")

    return G


def visualize_graph(
    G,
    filename="data/graphs/dependencies.html",
    roots=None,
    max_depth=4,
    same_level=False,
    graph_title=None,
):
    """
    Visualize the dependency graph using PyVis and save to an HTML file.
    """

    # ensure output directory exists
    os.makedirs(os.path.dirname(filename), exist_ok=True)

    # assign layers to each node
    if roots is None:
        roots = [n for n in G.nodes if G.in_degree(n) == 0]

    assign_layers(G, roots)

    # if not same_level:
    # TODO add cmdline option to control these features
    # G = remove_same_level(G)
    G = remove_reverse_edges(G)
    G = compute_node_metrics(G)

    nt = Network(height="100vh", width="100%", directed=True, bgcolor="#dddddd")

    nt.toggle_physics(True)
    nt.force_atlas_2based(  # circle layout
        gravity=-50,
        central_gravity=0.01,
        spring_length=100,
        spring_strength=0.1,
        damping=0.8,
        overlap=0.5,
    )
    nt.barnes_hut(
        gravity=-10000,
        central_gravity=0.01,
        spring_length=100,
        spring_strength=0.1,
        damping=0.8,
    )  # settings for the physics engine

    # color the graph dependencies layer by layer
    def layer_color(layer):
        palette = ["#5fc8f4", "#a1ce40", "#fde74c", "#ff8330", "#e55934", "#7b5e7b"]
        return palette[layer % len(palette)]

    # Add nodes
    for node, data in G.nodes(data=True):
        layer = data.get("layer", 0)
        indeg = data.get("in_degree", 0)
        importance = data.get("importance", 0)

        size = 15 + 30 * importance  # base size + scaled by importance
        if layer == 0:
            # base node
            size = 45

        # base_color = "#5fc8f4"  # light blue
        # highlight_color = "#ff7402"  # deep blue

        # Interpolate manually
        # def blend(c1, c2, t):
        #     r1, g1, b1 = to_rgb(c1)
        #     r2, g2, b2 = to_rgb(c2)
        #     r, g, b = (r1 + (r2 - r1)*t, g1 + (g2 - g1)*t, b1 + (b2 - b1)*t)

        #     return to_hex((r, g, b))

        # color = blend(base_color, highlight_color, importance)

        color = layer_color(layer)
        title = f"In-degree: {indeg} | Layer: {layer}"
        border_width = 3 if layer == 0 else 0.5

        nt.add_node(
            node,
            label=node,
            title=title,
            color=color,
            size=size,
            font={
                "size": 22,
                "color": "black",
                "strokeWidth": 15,
                "strokeColor": "white",
            },
            borderWidth=border_width,
        )

    # add edges with dependency type
    for u, v, data in G.edges(data=True):
        dep_type = data.get("type", "")
        color = {
            "Imports": "#999",
            "LinkingTo": "#ff5b02",
        }.get(dep_type, "#ccc")

        # same_level = u.color == v.color
        u_layer = G.nodes[u].get("layer", 0)
        v_layer = G.nodes[v].get("layer", 0)
        same_level = u_layer == v_layer

        if dep_type == "LinkingTo":
            nt.add_edge(
                u, v, color=color, dashes=True, title=dep_type, same_level=same_level
            )
        else:
            nt.add_edge(
                u, v, color=color, title=dep_type, arrows="to", same_level=same_level
            )

    nt.save_graph(filename)
    add_features(filename, title=graph_title, max_depth=max_depth)
