from flask import Flask, render_template, send_from_directory, request, jsonify
from .function_dependencies import function_dependencies
from .descriptions import scrape_package_description, scrape_function_description

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

    #API endpoint for dependency analysis (getting function dependencies from a function and package list)
    @app.route("/analyze", methods=["POST"])
    def analyze_function_dependencies():
        data = request.get_json()
        func = data.get("function")
        pkgs = data.get("packages")

        result = function_dependencies(func, pkgs, debug=True)
        
        print(result)

        result = [x for x in result if isinstance(x, str)]

        return jsonify({"required_packages": result})


    @app.route("/describe_package", methods=["POST"])
    def describe_package():
        data = request.get_json(silent=True) or {}
        pkg = data.get("package")
        if not pkg:
            return jsonify({"error": "Missing 'package'"}), 400

        desc = scrape_package_description(pkg)  # <-- you implement / already have
        if not desc:
            return jsonify({"description": ""})  # empty string is nicer for UI logic
        return jsonify({"description": desc})


    @app.route("/describe_function", methods=["POST"])
    def describe_function():
        data = request.get_json(silent=True) or {}
        func = data.get("function")
        pkg = data.get("package")
        if not pkg or not func:
            return jsonify({"error": "Missing 'package' or 'function'"}), 400

        desc = scrape_function_description(pkg, func)  # your existing function
        if not desc:
            return jsonify({"description": ""})
        return jsonify({"description": desc})

    return app
