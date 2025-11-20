from flask import Flask, render_template, send_from_directory, request, jsonify
from .function_dependencies import function_dependencies
from .scrape_function_description import scrape_function_description


def create_app():
    app = Flask(__name__)

    # serve main page
    @app.route("/")
    def index():
        return render_template("index.html")

    # serve generated graph files or data
    @app.route("/data/<path:filename>")
    def data_files(filename):
        return send_from_directory("../data", filename)

    # serve generated graph HTML files
    @app.route("/graphs/<path:filename>")
    def serve_graph(filename):
        return send_from_directory("../data/graphs", filename)

    # API endpoint for dependency analysis
    @app.route("/analyze", methods=["POST"])
    def analyze_function_dependencies():
        data = request.get_json()
        func = data.get("function")
        pkgs = data.get("packages")

        result = function_dependencies(func, pkgs)
        print(result)
        result = [x for x in result if isinstance(x, str)]

        return jsonify({"required_packages": result})


    @app.route("/describe", methods=["POST"])
    def describe_function():
        data = request.get_json()
        func = data.get("function")
        pkg = data.get("package")

        desc = scrape_function_description(pkg, func)
        if not desc:
            return jsonify({"description": "No description available."})
        return jsonify({"description": desc})

    return app
