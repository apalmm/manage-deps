import argparse
import os
from datetime import datetime
import webbrowser

from .construct_graph import build_graph
from .visualize_graph import visualize_graph
from .analyze import multi_root_graph
from .extract_package_functions import export_package_functions


def main():
    parser = argparse.ArgumentParser(description="r package dependency mapper")
    parser.add_argument("packages", nargs="+", help="packages to map")
    parser.add_argument(
        "--output_dir",
        type=str,
        default="data/graphs",
        help="where to store generated html files",
    )

    args = parser.parse_args()

    # build dependency graph
    if len(args.packages) == 1:
        G = build_graph(args.packages[0])
        graph_title = f"Dependency Map for {args.packages[0]}"
    else:
        G = multi_root_graph(args.packages)
        # create title
        pkg_list = ", ".join(args.packages)
        graph_title = f"Multi-root Dependency Map ({pkg_list}"

    # make sure output dir exists
    output_dir = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", args.output_dir)
    )

    os.makedirs(output_dir, exist_ok=True)

    max_depth = G.graph["max_depth"]

    # give each file a unique name
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    filename = f"{'_'.join(args.packages)}##depth{max_depth}_{timestamp}.html"
    output_file = os.path.join(output_dir, filename)

    # render the graph and export package metadata
    visualize_graph(
        G,
        output_file,
        roots=args.packages,
        max_depth=max_depth,
        graph_title=graph_title,
    )

    all_packages = list(G.nodes())
    export_package_functions(all_packages)

    webbrowser.open(f"http://127.0.0.1:5000/graphs/{filename}")


if __name__ == "__main__":
    main()
