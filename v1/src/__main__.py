import argparse

from .build_graph import build_graph
from .visualize import visualize_graph
from .analyze import multi_root_graph
from .extract_functions import export_package_functions

def main():
    #add in my arguments, and their descriptions for command line interface
    parser = argparse.ArgumentParser(description="R package dependency mapper")
    parser.add_argument("packages", nargs="+", help="Packages to map")
    parser.add_argument("--depth", type=int, default=4, help="Recursion depth")
    parser.add_argument("--same_level", type=bool, default=False, help="If enabled, keeps edges between nodes on the same layer")
    parser.add_argument("--output", type=str, default="data/graphs/output.html")
    
    args = parser.parse_args()

    #if only dealing with one package, build single root graph
    if len(args.packages) == 1:
        G = build_graph(args.packages[0], depth=args.depth)
    else:
        #if multiple packages, build multi root graph
        G = multi_root_graph(args.packages, depth=args.depth)
    
    #different title if single vs multi root
    if len(args.packages) == 1:
        graph_title = f"Dependency Map for {args.packages[0]} (depth={args.depth})"
    else:
        pkg_list = ", ".join(args.packages)
        graph_title = f"Multi-root Dependency Map ({pkg_list}; depth={args.depth})"

    output_file = "data/graphs/" + "_".join(args.packages) + f"_depth{args.depth}.html"

    #VISUALIZE the built graph with G, adding the visualization options (same_level, etc)
    visualize_graph(G, output_file, roots=args.packages, max_depth=args.depth, graph_title=graph_title)

    #EXPORT EACH PACKAGE FUNCTION METADATA
    all_packages = list(G.nodes())
    export_package_functions(all_packages)

    print(f"Saved graph to {output_file}")

if __name__ == "__main__":
    main()
