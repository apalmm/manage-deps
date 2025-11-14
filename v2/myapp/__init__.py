from flask import Flask, render_template, send_from_directory, request, jsonify

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
    def analyze():
        data = request.get_json()
        func = data.get("function")
        # deps = function_deps(func)

        return jsonify({"required_packages": list(func)})
    
    return app

