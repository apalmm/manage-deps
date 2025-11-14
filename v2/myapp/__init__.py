from flask import Flask, render_template, send_from_directory, request, jsonify
from .function_dependencies import function_dependencies

def create_app():
    app = Flask(__name__)
    #serve main page
    @app.route("/")
    def index():
        return render_template("index.html")

    #serve generated graph files or data
    @app.route("/data/<path:filename>")
    def data_files(filename):
        return send_from_directory("../data", filename)

    #serve generated graph HTML files
    @app.route("/graphs/<path:filename>")
    def serve_graph(filename):
        return send_from_directory("../data/graphs", filename)

    #API endpoint for dependency analysis
    @app.route("/analyze", methods=["POST"])
    def analyze_function_dependencies():
        data = request.get_json()
        func = data.get("function")
        pkgs = request.url_rule.rule.split("/")[-1]

        print(pkgs)
        deps = function_dependencies(func, ["tidyr", "stringr", "stringi", "cli", "stats", "tools"])
        deps = [x for x in deps if isinstance(x, str)]
        
        print(deps)
        return jsonify({"required_packages": list(deps)})
    
    return app

